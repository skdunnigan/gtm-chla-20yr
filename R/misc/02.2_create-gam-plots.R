
# set up session ----------------------------------------------------------

library(here)
source(here('R', '00_loadpackages.R'))

# load model files --------------------------------------------------------

load(file = here("output", "models", "fm.mod.RData")) 
load(file = here("output", "models", "pc.mod.RData")) 
load(file = here("output", "models", "pi.mod.RData"))
load(file = here("output", "models", "ss.mod.RData")) 


# plots fxn ---------------------------------------------------------------
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


# create series plots -----------------------------------------------------

plot1 <- function(mod, site, threshold, save) {
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,".plots.png")
  
  a <- show_prdseries_mod(mod)
  b <- show_metseason(mod, doystr = 1, doyend = 365, yrstr = 2003, yrend = 2022, ylab = ylab) + 
    geom_hline(yintercept = threshold, linetype = "dashed", linewidth = 1, color = "gray75") 
  
  c <- a / b
  
  if (save == TRUE) {
  ggsave(c, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(c)
  }
  
}

plot1(mod = pi.mod, site = "GTMPINUT", threshold = 6.6, save = T)
plot1(mod = ss.mod, site = "GTMSSNUT", threshold = 4.0, save = T)
plot1(mod = fm.mod, site = "GTMFMNUT", threshold = 5.5, save = T)
plot1(mod = pc.mod, site = "GTMPCNUT", threshold = 4.3, save = T)


# create trend plots for 3 timeframes -------------------------------------

plot2 <- function(mod, site, threshold, save) {
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,"trend.plots.png")
  
  a <- show_metseason(mod, doystr = 1, doyend = 365, yrstr = 2003, yrend = 2012, ylab = ylab) + 
    geom_hline(yintercept = threshold, linetype = "dashed", linewidth = 1, color = "gray75") 
  b <- show_metseason(mod, doystr = 1, doyend = 365, yrstr = 2013, yrend = 2022, ylab = ylab) + 
    geom_hline(yintercept = threshold, linetype = "dashed", linewidth = 1, color = "gray75") 
  c <- show_metseason(mod, doystr = 1, doyend = 365, yrstr = 2018, yrend = 2022, ylab = ylab) + 
    geom_hline(yintercept = threshold, linetype = "dashed", linewidth = 1, color = "gray75")
    
  d <- a / b / c
  
  if (save == TRUE) {
    ggsave(d, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(d)
  }
  
}

plot2(mod = pi.mod, site = "GTMPINUT", threshold = 6.6, save = T)
plot2(mod = j17.mod, site = "JXTR17", threshold = 6.6, save = T)
plot2(mod = ss.mod, site = "GTMSSNUT", threshold = 4.0, save = T)
plot2(mod = j21.mod, site = "JXTR21", threshold = 4.0, save = T)
plot2(mod = fm.mod, site = "GTMFMNUT", threshold = 5.5, save = T)
plot2(mod = mrt.mod, site = "MRT", threshold = 5.5, save = T)
plot2(mod = pc.mod, site = "GTMPCNUT", threshold = 4.3, save = T)


# create prddoy figures for each site -------------------------------------

plot3 <- function(mod, site, save){
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,"prddoy.plots.png")
  
  a <- show_prddoy(mod, ylab = ylab)
  
  if (save == TRUE) {
    ggsave(a, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(a)
  }
  
}

plot3(mod = pi.mod, site = "GTMPINUT", save = T)
plot3(mod = j17.mod, site = "JXTR17", save = T)
plot3(mod = ss.mod, site = "GTMSSNUT", save = T)
plot3(mod = j21.mod, site = "JXTR21", save = T)
plot3(mod = fm.mod, site = "GTMFMNUT", save = T)
plot3(mod = mrt.mod, site = "MRT", save = T)
plot3(mod = pc.mod, site = "GTMPCNUT", save = T)


ylab <- "Chlorophyll-a (\U00B5g/L)"

a <- show_prddoy(pi.mod, ylab = ylab) + labs(title = "GTMPINUT")
b <- show_prddoy(j17.mod, ylab = ylab) + labs(title = "JXTR17")

print((a + labs(x = "")) / (b + theme(legend.position = "none"))) %>% ggsave(filename = here('output', 'figures', 'trends', 'doy-tolomato.png'))


a <- show_prddoy(j21.mod, ylab = ylab) + labs(title = "JXTR21")
b <- show_prddoy(fm.mod, ylab = ylab) + labs(title = "GTMFMNUT")
c <- show_prddoy(mrt.mod, ylab = ylab) + labs(title = "MRT")

print((a + labs(x = "")) / (b + theme(legend.position = "none") + labs(x = "")) / (c + theme(legend.position = "none"))) %>% ggsave(filename = here('output', 'figures', 'trends', 'doy-matanzas.png'), height = 11, width = 8.5, units = "in")



# gam-plots for spring/fall comparisons -----------------------------------

show_met2 <- function (mod, metfun = mean, doystr = 1, doyend = 364, yrstr = 2000, 
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
  strt <- paste(lubridate::month(dts[1], label = T, abbr = T))
  ends <- paste(lubridate::month(dts[2], label = T, abbr = T))
  func <- as.character(substitute(metfun))
  ttl <- NULL
  subttl <- NULL
  toplo1 <- metseason
  p <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr, 
                                                   y = bt_met)) + ggplot2::geom_point(colour = "deepskyblue3") + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), 
                           colour = "deepskyblue3") + ggplot2::theme_bw(base_family = "serif", 
                                                                        base_size = base_size) + ggplot2::theme(axis.title.x = ggplot2::element_blank())
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
      subttl <- paste0(yrstr, "-", yrend, ", ", strt, "-", ends, ", ", logslope, ", ", pval)
    }
    if (mod$trans == "ident") {
      slope <- summary(mixmet)$coefficients[2, c(1, 5, 
                                                 6)]
      slope <- round(slope, 2)
      slope <- paste0(slope[1], " (", slope[2], ", ", slope[3], 
                      ")")
      subttl <- paste0(yrstr, "-", yrend, ", ", strt, "-", ends, ", ",
                       slope, ", ", logslope, ", ", pval)
    }
    p <- p + ggplot2::geom_ribbon(data = toplo2, ggplot2::aes(ymin = bt_lwr, 
                                                              ymax = bt_upr), fill = "pink", alpha = 0.4) + ggplot2::geom_line(data = toplo2, 
                                                                                                                               color = "pink")
  }
  p <- p + ggplot2::labs(title = ttl, subtitle = subttl, y = ylab) + 
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  return(p)
}


plot4 <- function(mod, site, save) {
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,"snl-trend.plots.png")
  
  a <- show_met2(mod, doystr = 1, doyend = 181, yrstr = 2003, yrend = 2012, ylab = ylab)
  b <- show_met2(mod, doystr = 1, doyend = 181, yrstr = 2013, yrend = 2022, ylab = ylab) + labs(y = "")
  c <- show_met2(mod, doystr = 1, doyend = 181, yrstr = 2018, yrend = 2022, ylab = ylab) + labs(y = "")
  
  d <- show_met2(mod, doystr = 182, doyend = 365, yrstr = 2003, yrend = 2012, ylab = ylab) 
  e <- show_met2(mod, doystr = 182, doyend = 365, yrstr = 2013, yrend = 2022, ylab = ylab) + labs(y = "")
  f <- show_met2(mod, doystr = 182, doyend = 365, yrstr = 2018, yrend = 2022, ylab = ylab) + labs(y = "")
  
  g <- (a + b + c) / (d + e + f) + 
    plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 12))
  
  if (save == TRUE) {
    ggsave(g, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(g)
  }
  
}

plot4(mod = pi.mod, site = "GTMPINUT", save = T)
plot4(mod = j17.mod, site = "JXTR17", save = T)
plot4(mod = ss.mod, site = "GTMSSNUT", save = T)
plot4(mod = j21.mod, site = "JXTR21", save = T)
plot4(mod = fm.mod, site = "GTMFMNUT", save = T)
plot4(mod = mrt.mod, site = "MRT", save = T)
plot4(mod = pc.mod, site = "GTMPCNUT", save = T)


plot5 <- function(mod, site, save) {
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,"20snl-trend.plots.png")
  
  a <- show_met2(mod, doystr = 1, doyend = 181, yrstr = 2003, yrend = 2022, ylab = ylab)
  b <- show_met2(mod, doystr = 182, doyend = 365, yrstr = 2003, yrend = 2022, ylab = ylab) 

  c <- a / b 

  if (save == TRUE) {
    ggsave(c, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(c)
  }
  
}

plot5(mod = pi.mod, site = "GTMPINUT", save = T)
plot5(mod = j17.mod, site = "JXTR17", save = T)
plot5(mod = ss.mod, site = "GTMSSNUT", save = T)
plot5(mod = j21.mod, site = "JXTR21", save = T)
plot5(mod = fm.mod, site = "GTMFMNUT", save = T)
plot5(mod = mrt.mod, site = "MRT", save = T)
plot5(mod = pc.mod, site = "GTMPCNUT", save = T)

# fulcrum timeframes

plot6 <- function(mod, site, save) {
  ylab <- "Chlorophyll-a (\U00B5g/L)"
  
  site <- paste(site)
  filename <- paste0(site,"fulc_snl-trend.plots.png")
  
  a <- show_met2(mod, doystr = 121, doyend = 227, yrstr = 2003, yrend = 2022, ylab = ylab)
  b <- show_met2(mod, doystr = 121, doyend = 227, yrstr = 2003, yrend = 2012, ylab = ylab) 
  c <- show_met2(mod, doystr = 121, doyend = 227, yrstr = 2013, yrend = 2022, ylab = ylab) 
  d <- show_met2(mod, doystr = 121, doyend = 227, yrstr = 2018, yrend = 2022, ylab = ylab) 
  
  
  e <- a / b / c / d + 
    plot_annotation(tag_levels = "a") & theme(plot.tag = element_text(size = 12))
  
  if (save == TRUE) {
    ggsave(e, filename = here('output', 'figures', 'trends', filename))
  }
  else {
    print(e)
  }
  
}

plot6(mod = pi.mod, site = "GTMPINUT", save = T)
plot6(mod = j17.mod, site = "JXTR17", save = T)
plot6(mod = ss.mod, site = "GTMSSNUT", save = T)
plot6(mod = j21.mod, site = "JXTR21", save = T)
plot6(mod = fm.mod, site = "GTMFMNUT", save = T)
plot6(mod = mrt.mod, site = "MRT", save = T)
plot6(mod = pc.mod, site = "GTMPCNUT", save = T)

rm(list = ls())
