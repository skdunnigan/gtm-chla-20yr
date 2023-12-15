# load model files --------------------------------------------------------

load(file = here("output", "models", "fm.mod.RData")) 
load(file = here("output", "models", "pc.mod.RData")) 
load(file = here("output", "models", "pi.mod.RData"))
load(file = here("output", "models", "ss.mod.RData")) 

a <- anlz_trndseason(pi.mod, doystr = 1, doyend = 365, justify = 'left', win = 10)
head(a)

show_trndseason(pi.mod, doystr = 1, doyend = 365, justify = 'center', win = 5, 
                ylab = 'Chl. change/yr, average', usearrow = T)
show_trndseason(pi.mod, nsim = 100, doystr = 121, doyend = 227, justify = 'center', win = 5, 
                ylab = 'Chl. change/yr, average', usearrow = T)
