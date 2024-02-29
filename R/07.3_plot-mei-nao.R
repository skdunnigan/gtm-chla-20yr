library(here)
source(here('R', '00_loadpackages.R')) # load packages
source(here('R', '07.1_load-plot-mei.R')) # load mei data and plot
source(here('R', '07.2_load-plot-nao.R')) # load nao data and plot

# create stacked multiplot
# 
# 
fig11 <-
(mei.plot + labs(title = "A")) / (nao.plot + labs(title = "B"))

ggsave(fig11, filename = here('output', 'figures', 'finals', 'figure11.png'),
       dpi = 600, unit = 'in',
       width = 6.5, height = 6)
