load(file = here("output", "data", "dat.RData")) # load prepped data file

glimpse(dat)

# create models for each site and export them

# pine island -------------------------------------------------------------


pi.mod <- dat %>%
          filter(station %in% "GTMPINUT") # only keep pine island data

pi.mod <- wqtrends::anlz_gam(pi.mod, trans = "log10")

pdf(file = here('output', 'models', 'pi-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(pi.mod)
dev.off()

pi.sum <- anlz_smooth(pi.mod) %>% mutate(station = "GTMPINUT")
pi.fit <- anlz_fit(pi.mod) %>% mutate(station = "GTMPINUT")

save(pi.mod, file = here('output', 'models', 'pi.mod.RData'))


# JXTR7 -------------------------------------------------------------------


j17.mod <- dat %>%
  filter(station %in% "JXTR17") # only keep JXTR17 data

j17.mod <- wqtrends::anlz_gam(j17.mod, trans = "log10")

pdf(file = here('output', 'models', 'jxtr17-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(j17.mod)
dev.off()

j17.sum <- anlz_smooth(j17.mod) %>% mutate(station = "JXTR17")
j17.fit <- anlz_fit(j17.mod) %>% mutate(station = "JXTR17")

save(j17.mod, file = here('output', 'models', 'j17.mod.RData'))

# san sebastian -----------------------------------------------------------


ss.mod <- dat %>%
  filter(station %in% "GTMSSNUT") # only keep gtmssnut data

ss.mod <- wqtrends::anlz_gam(ss.mod, trans = "log10")

pdf(file = here('output', 'models', 'ss-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(ss.mod)
dev.off()

ss.sum <- anlz_smooth(ss.mod) %>% mutate(station = "GTMSSNUT")
ss.fit <- anlz_fit(ss.mod) %>% mutate(station = "GTMSSNUT")

save(ss.mod, file = here('output', 'models', 'ss.mod.RData'))


# JXTR21 ------------------------------------------------------------------

j21.mod <- dat %>%
  filter(station %in% "JXTR21") # only keep JXTR21 data

j21.mod <- wqtrends::anlz_gam(j21.mod, trans = "log10")

pdf(file = here('output', 'models', 'jxtr21-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(j21.mod)
dev.off()

j21.sum <- anlz_smooth(j21.mod) %>% mutate(station = "JXTR21")
j21.fit <- anlz_fit(j21.mod) %>% mutate(station = "JXTR21")

save(j21.mod, file = here('output', 'models', 'j21.mod.RData'))


# fort matanzas -----------------------------------------------------------

fm.mod <- dat %>%
  filter(station %in% "GTMFMNUT") # only keep fm data

fm.mod <- wqtrends::anlz_gam(fm.mod, trans = "log10")

pdf(file = here('output', 'models', 'fm-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(fm.mod)
dev.off()

fm.sum <- anlz_smooth(fm.mod) %>% mutate(station = "GTMFMNUT")
fm.fit <- anlz_fit(fm.mod) %>% mutate(station = "GTMFMNUT")

save(fm.mod, file = here('output', 'models', 'fm.mod.RData'))


# MRT ---------------------------------------------------------------------

mrt.mod <- dat %>%
  filter(station %in% "MRT") # only keep fm data

mrt.mod <- wqtrends::anlz_gam(mrt.mod, trans = "log10")

pdf(file = here('output', 'models', 'mrt-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(mrt.mod)
dev.off()

mrt.sum <- anlz_smooth(mrt.mod) %>% mutate(station = "MRT")
mrt.fit <- anlz_fit(mrt.mod) %>% mutate(station = "MRT")

save(mrt.mod, file = here('output', 'models', 'mrt.mod.RData'))


# pellicer creek ----------------------------------------------------------

pc.mod <- dat %>%
  filter(station %in% "GTMPCNUT") # only keep pc data

pc.mod <- wqtrends::anlz_gam(pc.mod, trans = "log10")

pdf(file = here('output', 'models', 'pc-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(pc.mod)
dev.off()

pc.sum <- anlz_smooth(pc.mod) %>% mutate(station = "GTMPCNUT")
pc.fit <- anlz_fit(pc.mod) %>% mutate(station = "GTMPCNUT")

save(pc.mod, file = here('output', 'models', 'pc.mod.RData'))

sum <- bind_rows(pi.sum,
                  j17.sum,
                  ss.sum,
                  j21.sum,
                  fm.sum,
                  mrt.sum,
                  pc.sum) %>% select(station, 1:5)
fit <- bind_rows(pi.fit,
                 j17.fit,
                 ss.fit,
                 j21.fit,
                 fm.fit,
                 mrt.fit,
                 pc.fit) %>% select(station, 1:3)

write.csv(sum, file = here('output', 'models', 'gam.summary.csv'))
write.csv(fit, file = here('output', 'models', 'gam.fit.csv'))

dev.off()
rm(list = ls())
