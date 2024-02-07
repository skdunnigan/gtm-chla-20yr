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
  geom_line(aes(x = datetimestamp, y = temp, color = station), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "top") +
  labs(x = "", y = "Temperature, Celsius", color = "")

# monthly avg sal
b <- wq_mean_dat %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot() +
  geom_line(aes(x = datetimestamp, y = sal, color = station), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Salinity, psu", color = "")

# stacked plot
a / b

# annual min and max plot
temp_max <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = temp_max), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Max. Temperature, Celsius", color = "") 

temp_min <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = temp_min), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Min. Temperature, Celsius", color = "")

sal_max <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = sal_max), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Max. Salinity, PSU", color = "")

sal_min <- wq_min_max %>% 
  filter(datetimestamp > "2002-12-31") %>% 
  ggplot(aes(x = datetimestamp, color = station)) +
  geom_line(aes(y = sal_min), linewidth = 1) +
  scale_x_date(date_labels = "%Y", minor_breaks = "years") +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_colorblind() +
  theme_bw(base_family = "serif") + 
  theme(axis.text = element_text(colour = "black", size = 12),
        legend.position = "none") +
  labs(x = "", y = "Annual Min. Salinity, PSU", color = "")


a <- (temp_max + labs(title = "A")) + (sal_max + labs(title = "B")) 
b <- (temp_min + labs(title = "C")) + (sal_min + labs(title = "D"))

a / b 

