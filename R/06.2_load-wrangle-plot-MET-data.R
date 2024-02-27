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
a <-
MET_yr %>% 
  ggplot() +
  geom_col(aes(x = datetimestamp, y = rainfall_cm), fill = "#0075AC",
           width = 150) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  geom_hline(yintercept = 119.834, linetype = "dashed") +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.margin = unit(c(1,1,1,1), "pt")) +
  labs(x = "", y = "Total Rainfall (cm)",
       title = "A")

b <- 
MET_yr %>% 
  ggplot(aes(x = datetimestamp)) +
  geom_segment(aes(xend = datetimestamp, y = 0, yend = dev.avg)) +
  geom_point(aes(y = dev.avg, shape = dev.avg > 0), size = 2, color = "#0075AC") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_shape_discrete(name = "", labels = c("Drier", "Wetter")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(1,1,1,1), "pt")) +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_text(color = "black")) +
  labs(x = "", y = "Deviation from Average Rainfall (cm)",
       title = "B") 

fig10 <- 
a / b

ggsave(fig10, filename = here('output', 'figures', 'finals', 'figure10.png'),
       dpi = 600, units = 'in',
       width = 6.5, height = 7)