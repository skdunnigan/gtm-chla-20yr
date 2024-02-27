library(here)
source(here('R', '00_loadpackages.R'))

# start from here with wq -------------------------------------------------

load(here('output', 'data', 'wq_mean_dat.RData'))
load(here('output', 'data', 'wq_max_dat.RData'))
load(here('output', 'data', 'wq_min_dat.RData'))
load(here('output', 'data', 'wq_min_max.RData'))

# monthly avg temperature 
a <- wq_mean_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = temp, color = station)) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        legend.position = "top") +
  labs(x = "", y = "Temperature (\u00b0C)", color = "")

# monthly avg sal
b <- wq_mean_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = sal, color = station)) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        legend.position = "none") +
  labs(x = "", y = "Salinity (PSU)", color = "")

# stacked plot
fig8 <-
a / b

ggsave(fig8, filename = here('output', 'figures', 'finals', 'figure8.png'),
       dpi = 600, units = "in",
       height = 7, width = 6.5)

# annual min and max plot
temp_max <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = temp_max)) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        legend.position = "bottom") +
  labs(x = "", y = "Annual Max. Temperature (\u00b0C)",
       color = "") +
  guides(color = guide_legend(nrow = 2))

temp_min <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = temp_min)) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        legend.position = "none") +
  labs(x = "", y = "Annual Min. Temperature (\u00b0C)", color = "")

sal_max <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = sal_max)) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        legend.position = "bottom") +
  labs(x = "", y = "Annual Max. Salinity (PSU)",
       color = "") +
  guides(color = guide_legend(nrow = 2))

sal_min <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = sal_min)) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black"),
        legend.position = "none") +
  labs(x = "", y = "Annual Min. Salinity (PSU)", color = "")


a <- (temp_max + labs(title = "A")) + (sal_max + labs(title = "B")) 
b <- (temp_min + labs(title = "C")) + (sal_min + labs(title = "D"))

fig9 <-
a / b 

ggsave(fig9, filename = here('output', 'figures', 'finals', 'figure9.png'),
       dpi = 600, units = 'in',
       width = 6.5, height = 7)
