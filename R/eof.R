library(here)
library(wql)
library(tidyverse)

# load pi site data
load(here('output', 'data', 'pi.RData'))


# convert data into a ts object
pi_ts <- ts(as.data.frame(mdat.pi %>%
                            ungroup() %>% 
                            select(date, value) %>%
                            arrange(date) %>%
                            select(value)
                          ),
            start = c(2003, 1),
            end = c(2022, 12),
            frequency = 12
            )

# Run empirical orthogonal function analysis
eof(pi_ts, 1)