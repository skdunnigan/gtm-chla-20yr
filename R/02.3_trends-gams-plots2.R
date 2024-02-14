# pi mix-meta regression model results for 

# set the seasonal timeframe for annual period
metseason_pi <- anlz_metseason(pi.mod, doystr = 1, doyend = 365)

# first 10 years
summary(anlz_mixmeta(metseason_pi, yrstr = 2003, yrend = 2012))
# last 10 years
summary(anlz_mixmeta(metseason_pi, yrstr = 2013, yrend = 2022))
# last five years
summary(anlz_mixmeta(metseason_pi, yrstr = 2018, yrend = 2022))
# full 20 years
summary(anlz_mixmeta(metseason_pi, yrstr = 2003, yrend = 2022))

# create stacked graph combining annual averages from fitted GAM with points added for the metrics colored by trends estimated 
# in a 5 year center justified window
show_mettrndseason2 <- function (mod, metfun = mean, doystr = 1, doyend = 364, justify = c("center", "left", "right"), 
                                 win = 5, nsim = 10000, useave = FALSE, 
          yromit = NULL, ylab, width = 0.9, size = 3, nms = NULL, cols = NULL, 
          cmbn = F, base_size = 11, xlim = NULL, ylim = NULL, ...) 
{
  trndseason <- anlz_trndseason(mod = mod, metfun, doystr = doystr, 
                                doyend = doyend, justify = justify, win = win, useave = useave)
  if (!cmbn) {
    if (is.null(nms)) 
      nms <- c("Increasing", "Decreasing", "No trend", 
               "No estimate")
    if (is.null(cols)) 
      cols <- c("tomato1", "#0075AC", "white", "darkgrey")
    if (length(cols) != 4 | length(nms) != 4) 
      stop("Four names or colors must be provided")
    trndseason <- trndseason %>% dplyr::mutate(trnd = dplyr::case_when(yrcoef > 
                                                                         0 & pval < 0.05 ~ cols[1], yrcoef < 0 & pval < 0.05 ~ 
                                                                         cols[2], pval >= 0.05 ~ cols[3], is.na(yrcoef) ~ 
                                                                         cols[4]), trnd = factor(trnd, levels = cols, labels = nms))
  }
  if (cmbn) {
    if (is.null(nms)) 
      nms <- c("Increasing", "Decreasing", "No trend")
    if (is.null(cols)) 
      cols <- c("tomato1", "#0075AC", "white")
    if (length(cols) != 3 | length(nms) != 3) 
      stop("Three names or colors must be provided")
    trndseason <- trndseason %>% 
      dplyr::mutate(trnd = dplyr::case_when(yrcoef > 0 & pval < 0.05 ~ cols[1], 
                                            yrcoef < 0 & pval < 0.05 ~ cols[2], 
                                            pval >= 0.05 | is.na(yrcoef) ~ cols[3]), 
                                               trnd = factor(trnd, levels = cols, labels = nms))
  }
  names(cols) <- nms
  if (!is.null(yromit)) 
    trndseason <- trndseason %>% dplyr::filter(!yr %in% yromit)
  dts <- as.Date(c(doystr, doyend), origin = as.Date("2000-12-31"))
  strt <- paste(lubridate::month(dts[1], label = T, abbr = T), 
                lubridate::day(dts[1]))
  ends <- paste(lubridate::month(dts[2], label = T, abbr = T), 
                lubridate::day(dts[2]))
  func <- as.character(substitute(metfun))
  # ttl <- paste0("Est. ", func, " with 95% confidence intervals: ", 
  #               strt, "-", ends)
  # subttl <- paste0("Points colored by trend for ", win, "-year, ", 
  #                  justify, "-justified window")
  toplo <- trndseason
  p <- ggplot2::ggplot(data = toplo, ggplot2::aes(x = yr, y = bt_met)) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), 
                           colour = "black", width = width) + 
    ggplot2::geom_point(ggplot2::aes(fill = trnd), 
                        pch = 21, color = "black", size = size) + 
    ggplot2::theme_bw(base_size = base_size, 
                      base_family = "serif") + 
    ggplot2::scale_x_continuous(minor_breaks = c(2003, 2004, 2005, 2006, 2007,
                                                 2008, 2009, 2010, 2011, 2012,
                                                 2013, 2014, 2015, 2016, 2017, 
                                                 2018, 2019, 2020, 2021, 2022)) +
    ggplot2::scale_fill_manual(values = cols, drop = F) + 
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   legend.position = "top",
                   axis.text = ggplot2::element_text(color = "black")) + 
    ggplot2::labs(y = ylab, fill = NULL) + 
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  return(p)
}


a <- show_mettrndseason2(pi.mod, metfun = mean, doystr = 1, doyend = 365, justify = 'center', 
                   win = 5, ylab = 'Chlorophyll-a (ug/L)') + theme(legend.position = "none") + labs(title = "A")
b <- show_mettrndseason2(ss.mod, metfun = mean, doystr = 1, doyend = 365, justify = 'center', 
                        win = 5, ylab = 'Chlorophyll-a (ug/L)') + theme(legend.position = "none") + labs(title = "B")
c <- show_mettrndseason2(fm.mod, metfun = mean, doystr = 1, doyend = 365, justify = 'center', 
                        win = 5, ylab = 'Chlorophyll-a (ug/L)') + theme(legend.position = "none") + labs(title = "C")
d <- show_mettrndseason2(pc.mod, metfun = mean, doystr = 1, doyend = 365, justify = 'center', 
                        win = 5, ylab = 'Chlorophyll-a (ug/L)') + theme(legend.position = "bottom") + labs(title = "D")

a / b / c / d