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


fulc <- function(dat, station, save){
  
  filename <- paste0(station,"-fulcrum.png")
  
  ts <- ts(as.data.frame(dat %>%
                           ungroup() %>% 
                           select(date, value) %>%
                           arrange(date) %>%
                           select(value)
  ),
  start = c(2003, 1),
  end = c(2022, 12),
  frequency = 12
  )
  
  
  phenoPhase(ts)
  
  fulc <-
  ggplot(data = phenoPhase(ts), 
         aes(x = year, y = fulcrum)) +
    geom_hline(aes(yintercept = mean(fulcrum)), color = "darkorange") +
    geom_smooth(method = "lm", se = F, color = "black", linetype = "dashed") +
    geom_point(size=5, color="#0072B2") +
    theme_bw() +
    theme(axis.text = element_text(size = 12, color = "black")) +
    labs(x = "",
         y = "Fulcrums of Cumulative Chl",
         title = station)
  
  if (save == TRUE) {
    ggsave(fulc, filename = here('output', 'figures', 'fulcrums', filename))
  }
  else {
    print(fulc)
  }
}

fulc(dat = mdat.pi, station = "GTMPINUT", save = F)
fulc(dat = mdat.j17, station = "JXTR17", save = F)
fulc(dat = mdat.ss, station = "GTMSSNUT", save = F)
fulc(dat = mdat.j21, station = "JXTR21", save = F)
fulc(dat = mdat.fm, station = "GTMFMNUT", save = F)
fulc(dat = mdat.mrt, station = "MRT", save = F)
fulc(dat = mdat.pc, station = "GTMPCNUT", save = F)