library(here)
source(here('R', '00_loadpackages.R'))

# MET ---------------------------------------------------------------------

fun_in <- function(x) {sum(x, na.rm = TRUE)}

# read in data

MET <- SWMPr::import_local(path = here::here('data',
                                             'swmp'), 
                           station_code = 'gtmpcmet') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5')) 

# get monthly precipitation totals for correlation analysis ---------------

MET_mo <- MET %>% 
  aggreswmp(by = "months", FUN = fun_in, params = c('totprcp')) %>% 
  filter(datetimestamp > "2002-12-31")

# save(MET_mo, file = here('output', 'data', 'MET_mo.RData'))


# annual precipitation totals with deviation calculations -----------------

MET_yr <- MET %>% 
  aggreswmp(by = "years", FUN = fun_in, params = c('totprcp')) %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  mutate(rainfall_cm = totprcp/10,
         dev.avg = rainfall_cm - 119.832)


# annual rainfall plots ---------------------------------------------------

# rainfall graph
MET_yr %>% 
  ggplot() +
  geom_col(aes(x = datetimestamp, y = rainfall_cm), fill = "#0075AC",
           width = 150) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  geom_hline(yintercept = 119.834, linetype = "dashed") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black")) +
  labs(x = "", y = "Rainfall, cm")

MET_yr %>% 
  ggplot(aes(x = datetimestamp)) +
  geom_col(aes(y = dev.avg), , fill = "#0075AC",
           width = 150) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw(base_family = "serif") +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black")) +
  labs(x = "", y = "Deviation from Average Rainfall, cm") +
  annotate("text",
           label = "Wetter", 
           x = as.Date("2003-01-01"),
           y = 20) +
  annotate("text",
           label = "Drier", 
           x = as.Date("2003-01-01"),
           y = -30)

