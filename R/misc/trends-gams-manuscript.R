# load model files --------------------------------------------------------

load(file = here("output", "models", "fm.mod.RData")) 
load(file = here("output", "models", "pc.mod.RData")) 
load(file = here("output", "models", "pi.mod.RData"))
load(file = here("output", "models", "ss.mod.RData")) 


# overall trend plots -----------------------------------------------------

# modifications of `wqtrends::show_prdseries_mod()` fxn

show_prdseries_mod <-  function (mod, alpha = 0.7, base_size = 11, xlim = NULL, 
                                 ylim = NULL) 
{
  prds <- anlz_prd(mod)
  trans <- unique(prds$trans)
  tobacktrans <- mod$model %>% dplyr::mutate(trans = mod$trans)
  moddat <- anlz_backtrans(tobacktrans) %>% dplyr::mutate(date = lubridate::date_decimal(cont_year), 
                                                          date = as.Date(date))
  p <- ggplot2::ggplot(prds, ggplot2::aes(x = date)) + 
    ggplot2::geom_point(data = moddat, 
                        ggplot2::aes(y = value), color = "gray60", size = 1) + 
    ggplot2::geom_line(ggplot2::aes(y = value), 
                       linewidth = 0.75, alpha = alpha, colour = "#56B4E9") + 
    ggplot2::theme_bw(base_family = "serif", 
                      base_size = base_size) + 
    ggplot2::theme(legend.position = "top", 
                   legend.title = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_blank(),
                   axis.text = element_text(size = 12, color = "black")) +
    ggplot2::labs(y = "Chlorophyll-a (\U00B5g/L)") + 
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  if (trans != "ident") 
    p <- p + ggplot2::scale_y_log10()
  return(p)
}

# modification of `wqtrends::show_trndseason()` fxn

show_trndseason_mod <- function (mod, metfun = mean, doystr = 1, doyend = 364, 
                                 type = c("log10", "approx"), justify = c("left", "right", "center"), win = 5, 
          ylab, nsim = 10000, useave = FALSE, base_size = 11, xlim = NULL, 
          ylim = NULL, ...) 
{
  justify <- match.arg(justify)
  type <- match.arg(type)
  trndseason <- anlz_trndseason(mod = mod, metfun = metfun, 
                                doystr = doystr, doyend = doyend, justify = justify, 
                                win = win, nsim = nsim, useave = useave, ...)
  dts <- as.Date(c(doystr, doyend), origin = as.Date("2000-12-31"))
  strt <- paste(lubridate::month(dts[1], label = T, abbr = T), 
                lubridate::day(dts[1]))
  ends <- paste(lubridate::month(dts[2], label = T, abbr = T), 
                lubridate::day(dts[2]))
  subttl <- paste0("Estimates based on ", justify, " window of ", 
                   win, " years")
  yrrng <- range(trndseason$yr, na.rm = T)
  toplo <- trndseason %>% dplyr::mutate(pval = dplyr::case_when(pval < 
                                                                  0.05 ~ "p < 0.05", T ~ "ns"), pval = factor(pval, levels = c("ns", 
                                                                                                                               "p < 0.05"))) %>% na.omit()
  if (type == "log10" & mod$trans == "log10") {
    ttl <- paste0("Annual log-slopes (+/- 95%) for seasonal trends: ", 
                  strt, "-", ends)
    p <- ggplot2::ggplot(data = toplo, ggplot2::aes(x = yr, 
                                                    y = yrcoef, fill = pval)) + ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = yrcoef_lwr, 
                                          ymax = yrcoef_upr, color = pval), width = 0) + 
      ggplot2::scale_color_manual(values = c("black", "tomato1"), 
                                  drop = FALSE)
  }
  if (type == "approx" & mod$trans == "log10") {
    ttl <- paste0("Annual slopes (approximate) for seasonal trends: ", 
                  strt, "-", ends)
    p <- ggplot2::ggplot(data = toplo, ggplot2::aes(x = yr, 
                                                    y = appr_yrcoef, fill = pval)) + ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::labs(title = ttl, subtitle = subttl, y = ylab)
  }
  if (mod$trans == "ident") {
    ttl <- paste0("Annual slopes (+/- 95%) for seasonal trends: ", 
                  strt, "-", ends)
    p <- ggplot2::ggplot(data = toplo, ggplot2::aes(x = yr, 
                                                    y = yrcoef, fill = pval)) + ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = yrcoef_lwr, 
                                          ymax = yrcoef_upr, color = pval), width = 0) + 
      ggplot2::scale_color_manual(values = c("black", "tomato1"), 
                                  drop = FALSE)
  }
  p <- p + ggplot2::geom_point(shape = 21, size = 3) + ggplot2::scale_fill_manual(values = c("white", 
                                                                                             "tomato1"), drop = FALSE) + ggplot2::scale_x_continuous(limits = yrrng) + 
    ggplot2::theme_bw(base_family = "serif", 
                      base_size = base_size) + 
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "black", size = 12)) + 
    ggplot2::labs(title = ttl, subtitle = subttl, y = ylab) + 
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  return(p)
}

# modification of `wqtrends::show_metseason()` fxn

show_metseason_mod <- function (mod, metfun = mean, doystr = 1, doyend = 364, yrstr = 2000, 
          yrend = 2019, yromit = NULL, ylab, nsim = 10000, useave = FALSE, 
          base_size = 11, xlim = NULL, ylim = NULL, ...) 
{
  chk <- identical(deparse(metfun), deparse(mean))
  if (!chk & useave) 
    stop("Specify metfun = mean if useave = T")
  if (useave) 
    metseason <- anlz_avgseason(mod, doystr = doystr, doyend = doyend)
  if (!useave) 
    metseason <- anlz_metseason(mod, metfun, doystr = doystr, 
                                doyend = doyend, nsim = nsim, ...)
  if (!is.null(yromit)) 
    metseason <- metseason %>% dplyr::filter(!yr %in% yromit)
  trans <- mod$trans
  dts <- as.Date(c(doystr, doyend), origin = as.Date("2000-12-31"))
  strt <- paste(lubridate::month(dts[1], label = T, abbr = T), 
                lubridate::day(dts[1]))
  ends <- paste(lubridate::month(dts[2], label = T, abbr = T), 
                lubridate::day(dts[2]))
  func <- as.character(substitute(metfun))
  ttl <- paste0("Est. ", func, " with 95% confidence intervals: ", 
                strt, "-", ends)
  subttl <- NULL
  toplo1 <- metseason
  p <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr, 
                                                   y = bt_met)) + ggplot2::geom_point(colour = "deepskyblue3") + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), 
                           colour = "deepskyblue3") + ggplot2::theme_bw(base_family = "serif", 
                                                                        base_size = base_size) + 
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(color = "black", size = 12))
  if (!any(is.null(yrstr) | is.null(yrend))) {
    mixmet <- anlz_mixmeta(metseason, yrstr = yrstr, yrend = yrend, 
                           yromit = yromit)
    toplo2 <- data.frame(yr = seq(yrstr, yrend, length = 50)) %>% 
      dplyr::mutate(met = predict(mixmet, newdata = data.frame(yr = yr)), 
                    se = predict(mixmet, newdata = data.frame(yr = yr), 
                                 se = T)[, 2], bt_lwr = met - 1.96 * se, bt_upr = met + 
                      1.96 * se, bt_met = met)
    pval <- coefficients(summary(mixmet)) %>% data.frame %>% 
      .[2, 4] %>% anlz_pvalformat()
    if (mod$trans == "log10") {
      dispersion <- summary(mod)$dispersion
      toplo2 <- data.frame(yr = seq(yrstr, yrend, length = 50)) %>% 
        dplyr::mutate(met = predict(mixmet, newdata = data.frame(yr = yr)), 
                      se = predict(mixmet, newdata = data.frame(yr = yr), 
                                   se = T)[, 2], bt_lwr = 10^((met - 1.96 * 
                                                                 se) + log(10) * dispersion/2), bt_upr = 10^((met + 
                                                                                                                1.96 * se) + log(10) * dispersion/2), bt_met = 10^(met + 
                                                                                                                                                                     log(10) * dispersion/2))
      slope <- lm(bt_met ~ yr, toplo2) %>% summary %>% 
        coefficients %>% .[2, 1]
      slope <- round(slope, 2)
      logslope <- summary(mixmet)$coefficients[2, c(1, 
                                                    5, 6)]
      logslope <- round(logslope, 2)
      logslope <- paste0(logslope[1], " (", logslope[2], 
                         ", ", logslope[3], ")")
      subttl <- paste0("Trend from ", yrstr, " to ", yrend, 
                       ": approximate slope ", slope, ", log-slope ", 
                       logslope, ", ", pval)
    }
    if (mod$trans == "ident") {
      slope <- summary(mixmet)$coefficients[2, c(1, 5, 
                                                 6)]
      slope <- round(slope, 2)
      slope <- paste0(slope[1], " (", slope[2], ", ", slope[3], 
                      ")")
      subttl <- paste0("Trend from ", yrstr, " to ", yrend, 
                       ": slope ", slope, ", ", pval)
    }
    p <- p + ggplot2::geom_ribbon(data = toplo2, ggplot2::aes(ymin = bt_lwr, 
                                                              ymax = bt_upr), fill = "pink", alpha = 0.4) + ggplot2::geom_line(data = toplo2, 
                                                                                                                               color = "pink")
  }
  p <- p + ggplot2::labs(title = ttl, subtitle = subttl, y = ylab) + 
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  return(p)
}

# create series plots -----------------------------------------------------

plot1 <- function(mod, site, threshold, save) {
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,".plots.png")
  
  a <- show_prdseries_mod(mod)
  b <- show_metseason_mod(mod, doystr = 1, doyend = 365, yrstr = 2003, yrend = 2022, ylab = ylab) + 
    geom_hline(yintercept = threshold, linetype = "dashed", linewidth = 1, color = "gray75") 
  c <- show_trndseason_mod(mod, doystr = 1, doyend = 365, justify = 'center', win = 5, 
                       ylab = 'Log10 chlorophyll a change/yr, average')
  
  d <- a / b / c
  
  if (save == TRUE) {
    ggsave(d, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(d)
  }
  
}

plot1(mod = pi.mod, site = "PI", threshold = 6.6, save = F)
plot1(mod = j17.mod, site = "JXTR17", threshold = 6.6, save = T)
plot1(mod = ss.mod, site = "GTMSSNUT", threshold = 4.0, save = T)
plot1(mod = j21.mod, site = "JXTR21", threshold = 4.0, save = T)
plot1(mod = fm.mod, site = "GTMFMNUT", threshold = 5.5, save = T)
plot1(mod = mrt.mod, site = "MRT", threshold = 5.5, save = T)
plot1(mod = pc.mod, site = "GTMPCNUT", threshold = 4.3, save = T)

# seasonal trend plots based on fulcrum period ----------------------------


