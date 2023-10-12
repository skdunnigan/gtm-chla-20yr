# cumulative sum figure code

# mutate(mo = factor(mo, 
#                                                                                              levels = c(1:12),
#                                                                                              labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 

a <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2003) %>% mutate(mo = c(1:12))
b <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2004) %>% mutate(mo = c(1:12))
c <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2005) %>% mutate(mo = c(1:12))
d <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2006) %>% mutate(mo = c(1:12))
e <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2007) %>% mutate(mo = c(1:12))
f <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2008) %>% mutate(mo = c(1:12))
g <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2009) %>% mutate(mo = c(1:12))
h <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2010) %>% mutate(mo = c(1:12))
i <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2011) %>% mutate(mo = c(1:12))
j <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2012) %>% mutate(mo = c(1:12))
k <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2013) %>% mutate(mo = c(1:12))
l <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2014) %>% mutate(mo = c(1:12))
m <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2015) %>% mutate(mo = c(1:12))
n <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2016) %>% mutate(mo = c(1:12))
o <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2017) %>% mutate(mo = c(1:12))
p <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2018) %>% mutate(mo = c(1:12))
q <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2019) %>% mutate(mo = c(1:12))
r <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2020) %>% mutate(mo = c(1:12))
s <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2021) %>% mutate(mo = c(1:12))
t <- mdat.pi %>% group_by(yr) %>% reframe(x = cumsum(value)) %>% filter(yr == 2022) %>% mutate(mo = c(1:12))

cumsum_pi <- bind_rows(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) %>% 
  ggplot(aes(x = mo, y = x, group = yr, color = yr)) +
  geom_line() + 
  scale_x_continuous(breaks = c(1:12)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Month",
       y = "CUMSUM Functions of Chl") +
  theme_bw()
cumsum_pi

rm(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)