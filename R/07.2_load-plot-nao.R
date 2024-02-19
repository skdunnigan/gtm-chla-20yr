nao <- read.table(here('data', 'NAO_index.txt'),
                  header = TRUE, sep = "")

glimpse(nao)

nao %>% filter(Year > 2002) %>% 
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "nao") %>% 
  write.csv(file = here('output', 'data', 'nao_values.csv'))

months <- tribble(
  ~month, ~month_no,
  "Jan", 01,
  "Feb", 02,
  "Mar", 03,
  "Apr", 04,
  "May", 05,
  "Jun", 06,
  "Jul", 07,
  "Aug", 08,
  "Sep", 09,
  "Oct", 10,
  "Nov", 11,
  "Dec", 12)

nao %>% 
  filter(Year > 2002) %>% 
  pivot_longer(cols = c(2:13), names_to = "month", values_to = "nao") %>% 
  left_join(months, by = "month") %>% 
  mutate(day = 1,
         date = as.Date(paste0(Year,"-",month_no,"-",day))) %>% 
  ggplot(aes(x = date, y = nao)) +
  geom_ribbon(aes(ymin = pmin(nao, 0), ymax = 0), fill = "#0075AC", col = "#0075AC", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(nao, 0)), fill = "tomato", col = "tomato", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") + 
  scale_x_date(date_minor_breaks = "year") +
  theme_bw(base_family = "sans") +
  theme(axis.text = element_text(size = 12, color = "black")) +
  labs(y = "NAO",
       x = "")