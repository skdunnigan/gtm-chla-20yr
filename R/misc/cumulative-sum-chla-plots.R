library(here)
source(here('R', '00_loadpackages.R'))


# load data ---------------------------------------------------------------
# load all data that has had the missing values filled in with the GAM predictions
load(here('output', 'data', 'pi.RData'))
load(here('output', 'data', 'j17.RData'))
load(here('output', 'data', 'ss.RData'))
load(here('output', 'data', 'j21.RData'))
load(here('output', 'data', 'fm.RData'))
load(here('output', 'data', 'mrt.RData'))
load(here('output', 'data', 'pc.RData'))


csumfig <- function(dat, station, save){
  
  df <- dat
  filename <- paste0(station,"-csum.png")
  
  a <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2003) %>% mutate(mo = c(1:12))
  b <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2004) %>% mutate(mo = c(1:12))
  c <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2005) %>% mutate(mo = c(1:12))
  d <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2006) %>% mutate(mo = c(1:12))
  e <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2007) %>% mutate(mo = c(1:12))
  f <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2008) %>% mutate(mo = c(1:12))
  g <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2009) %>% mutate(mo = c(1:12))
  h <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2010) %>% mutate(mo = c(1:12))
  i <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2011) %>% mutate(mo = c(1:12))
  j <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2012) %>% mutate(mo = c(1:12))
  k <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2013) %>% mutate(mo = c(1:12))
  l <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2014) %>% mutate(mo = c(1:12))
  m <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2015) %>% mutate(mo = c(1:12))
  n <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2016) %>% mutate(mo = c(1:12))
  o <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2017) %>% mutate(mo = c(1:12))
  p <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2018) %>% mutate(mo = c(1:12))
  q <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2019) %>% mutate(mo = c(1:12))
  r <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2020) %>% mutate(mo = c(1:12))
  s <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2021) %>% mutate(mo = c(1:12))
  t <- dat %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2022) %>% mutate(mo = c(1:12))
  
  cumsum <- bind_rows(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) %>% 
    ggplot(aes(x = mo, y = x)) +
    geom_smooth(color = "black", linetype = "dashed") +
    geom_line(aes( group = yr, color = yr)) + 
    scale_x_continuous(breaks = c(1:12)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_color_continuous(type = "viridis") +
    theme_bw() +
    theme(legend.position = "top",
          axis.text = element_text(size = 12, color = "black")) +
    ggplot2::guides(colour = ggplot2::guide_colourbar(barheight = 1, 
                                                      barwidth = 20)) +
    labs(x = "Month",
         y = "CUMSUM Functions of Chl",
         color = "",
         title = station)
    
  if (save == TRUE) {
    ggsave(cumsum, filename = here('output', 'figures', 'cumulative-sum', filename))
  }
  else {
    print(cumsum)
  }
  
  rm(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  
}


# create plots ------------------------------------------------------------

csumfig(dat = mdat.pi, station = "GTMPINUT", save = F) 
csumfig(dat = mdat.j17, station = "JXTR17", save = F)
csumfig(dat = mdat.ss, station = "GTMSSNUT", save = F)
csumfig(dat = mdat.j21, station = "JXTR21", save = F)
csumfig(dat = mdat.fm, station = "GTMFMNUT", save = F)
csumfig(dat = mdat.mrt, station = "MRT", save = F)
csumfig(dat = mdat.pc, station = "GTMPCNUT", save = F)

rm(list = ls())
