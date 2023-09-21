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

save(pi.mod, file = here('output', 'models', 'pi.mod.RData'))


# JXTR7 -------------------------------------------------------------------


j17.mod <- dat %>%
  filter(station %in% "JXTR17") # only keep JXTR17 data

j17.mod <- wqtrends::anlz_gam(j17.mod, trans = "log10")

pdf(file = here('output', 'models', 'jxtr17-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(j17.mod)
dev.off()

save(j17.mod, file = here('output', 'models', 'j17.mod.RData'))

# san sebastian -----------------------------------------------------------


ss.mod <- dat %>%
  filter(station %in% "GTMSSNUT") # only keep gtmssnut data

ss.mod <- wqtrends::anlz_gam(ss.mod, trans = "log10")

pdf(file = here('output', 'models', 'ss-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(ss.mod)
dev.off()

save(ss.mod, file = here('output', 'models', 'ss.mod.RData'))


# JXTR21 ------------------------------------------------------------------

j21.mod <- dat %>%
  filter(station %in% "JXTR21") # only keep JXTR21 data

j21.mod <- wqtrends::anlz_gam(j21.mod, trans = "log10")

pdf(file = here('output', 'models', 'jxtr21-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(j21.mod)
dev.off()

save(j21.mod, file = here('output', 'models', 'j21.mod.RData'))


# fort matanzas -----------------------------------------------------------

fm.mod <- dat %>%
  filter(station %in% "GTMFMNUT") # only keep fm data

fm.mod <- wqtrends::anlz_gam(fm.mod, trans = "log10")

pdf(file = here('output', 'models', 'fm-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(fm.mod)
dev.off()

save(fm.mod, file = here('output', 'models', 'fm.mod.RData'))


# MRT ---------------------------------------------------------------------

mrt.mod <- dat %>%
  filter(station %in% "MRT") # only keep fm data

mrt.mod <- wqtrends::anlz_gam(mrt.mod, trans = "log10")

pdf(file = here('output', 'models', 'mrt-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(mrt.mod)
dev.off()

save(mrt.mod, file = here('output', 'models', 'mrt.mod.RData'))


# pellicer creek ----------------------------------------------------------

pc.mod <- dat %>%
  filter(station %in% "GTMPCNUT") # only keep pc data

pc.mod <- wqtrends::anlz_gam(pc.mod, trans = "log10")

pdf(file = here('output', 'models', 'pc-gam.check.pdf'))
par(mfrow=c(2,2))
gam.check(pc.mod)
dev.off()

save(pc.mod, file = here('output', 'models', 'pc.mod.RData'))

dev.off()
rm(list = ls())