
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



show_prdseries_mod(pi.mod)
ylab <- "Chlorophyll-a (\U00B5g/L)"
show_prddoy(pi.mod, ylab = ylab)
