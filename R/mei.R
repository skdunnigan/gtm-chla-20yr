# data from: https://psl.noaa.gov/enso/mei/

mei <- read.csv(here('data', 'enso', 'meiv2.csv'),
         header = T) %>% 
  filter(YEAR >= 2003) %>% 
  pivot_longer(names_to = "season",
               values_to = "mei",
               cols = c(2:13)) 

sa <- tribble(
  ~season, ~month,
  "DJ", 01,
  "JF", 02,
  "FM", 03,
  "MA", 04,
  "AM", 05,
    "MJ", 06,
    "JJ", 07,
    "JA", 08,
    "AS", 09,
    "SO", 10,
    "ON", 11,
    "ND", 12)

meiv2 <- left_join(mei, sa) %>% 
  mutate(day = 1, 
         date = as.Date(paste0(YEAR,"-",month,"-",day)))

# save(meiv2, file = here('output', 'data', 'mei.RData'))

mei.plot <-
ggplot(meiv2, aes(x = date, y = mei)) +
  geom_ribbon(aes(ymin = pmin(mei, 0), ymax = 0), fill = "blue", col = "blue", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(mei, 0)), fill = "red", col = "red", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") + 
  scale_x_date(date_minor_breaks = "year") +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black")) +
  labs(y = "MEI.v2",
       x = "",
       title = "Multivariate ENSO Index (MEI) from 2003-2022.
       Positive values (red) indicate El Ni\U00F1o periods and negative values (blue) La Ni\U00F1 a periods.")

print(mei.plot)

# ggsave(mei.plot, filename = here('output', 'figures', 'mei-plot.png'))
