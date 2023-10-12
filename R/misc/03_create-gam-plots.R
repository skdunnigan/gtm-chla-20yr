
# load model files --------------------------------------------------------

load(file = here("output", "models", "fm.mod.RData")) 
load(file = here("output", "models", "j17.mod.RData")) 
load(file = here("output", "models", "j21.mod.RData")) 
load(file = here("output", "models", "mrt.mod.RData")) 
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
plot1(mod = j17.mod, site = "JXTR17", threshold = 6.6, save = T)
plot1(mod = ss.mod, site = "GTMSSNUT", threshold = 4.0, save = T)
plot1(mod = j21.mod, site = "JXTR21", threshold = 4.0, save = T)
plot1(mod = fm.mod, site = "GTMFMNUT", threshold = 5.5, save = T)
plot1(mod = mrt.mod, site = "MRT", threshold = 5.5, save = T)
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

print((a + labs(x = "")) / (b + theme(legend.position = "none"))) %>% ggsave(filename = here('output', 'figures', 'doy-tolomato.png'))


a <- show_prddoy(j21.mod, ylab = ylab) + labs(title = "JXTR21")
b <- show_prddoy(fm.mod, ylab = ylab) + labs(title = "GTMFMNUT")
c <- show_prddoy(mrt.mod, ylab = ylab) + labs(title = "MRT")

print((a + labs(x = "")) / (b + theme(legend.position = "none") + labs(x = "")) / (c + theme(legend.position = "none"))) %>% ggsave(filename = here('output', 'figures', 'doy-matanzas.png'), height = 11, width = 8.5, units = "in")


rm(list = ls())
