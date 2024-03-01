# Run correlations on mei and monthly chlorophyll with environmental data
# data includes sample month and preceding month for
# total rainfall, monthly average, avg daily minimums, avg daily maximums
# https://github.com/easystats/report

# load packages
library(here)
source(here('R', '00_loadpackages.R'))

# load correlation data for PI

pi_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "PI") 
ss_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "SS") %>% mutate_if(is.character, as.numeric)
fm_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "FM") 
pc_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "PC") 

corr_plot_fxn <- function(dat) {

dat <- dat %>% 
  select(-datetimestamp) %>% 
  data.matrix()

# compute correlation matrix
corr <- cor(dat, method = "spearman", use = "complete.obs")
head(corr)

# compute a matrix of correlation p-values
p.mat <- cor_pmat(dat)
head(p.mat[1:4])

a <- ggcorrplot(corr, p.mat = p.mat,
           type = "lower",
           insig = "blank",
           method = "circle",
           outline.color = "transparent",
           lab = TRUE,
           legend.title = "Spearman rho",
           colors = c("#0075AC", "white", "tomato"),
           lab_size = 3) +
  theme_classic(base_family = "serif") +
  theme(# everything in theme is strictly aesthetics
    legend.text = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color='black'),
    plot.caption = element_text(size=6, face='italic'),
    axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
    axis.text.y = element_text(size=12, color='black'),
    axis.ticks.x = element_line(color='black'),
    # plot.title = element_text(size = 16, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic'))

print(a)
}


# run correlation plot creation
corr_plot_fxn(pi_cor)
corr_plot_fxn(ss_cor)
corr_plot_fxn(fm_cor)
corr_plot_fxn(pc_cor)

# examine matrix
pi.cor <- 
pi_cor %>% 
select(-datetimestamp) %>% 
  data.matrix() %>% 
  cor(method = "spearman", use = "complete.obs") %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  # tidyr::pivot_longer(-rowname)
  select(rowname, chla) %>% 
  rename(corr = chla)

pi.cor2 <- 
  pi_cor %>% 
  select(-datetimestamp) %>% 
  data.matrix() %>% 
  cor_pmat(method = "spearman", use = "complete.obs") %>%
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  # tidyr::pivot_longer(-rowname) 
  select(rowname, chla) %>% 
  rename(pval = chla) 

corr <- left_join(pi.cor, pi.cor2, by = "rowname") %>% 
  mutate(pval1 = format.pval(pval, digits = 2),
         pval2 = scales::pvalue(pval,
                                accuracy = 0.01,
                                decimal.mark = ".",
                                add_p = TRUE),
         pval3 = psycho::format_p(pval,
                                  stars_only = TRUE))
# combination -------------------------------------------------------------

a <- pi_cor %>% mutate(station = "PI") 
b <- ss_cor %>% mutate(station = "SS") 
c <- fm_cor %>% mutate(station = "FM")
d <- pc_cor %>% mutate(station = "PC")

dat <- bind_rows(a, b, c, d) 

corr <- dat %>% 
  dplyr::select(-datetimestamp) %>% 
  dplyr::group_by(station) %>% 
  correlation::correlation(method = "spearman", 
                           p_adjust = "none",
                           redundant = TRUE) %>% 
  dplyr::filter(Parameter1 == "chla") %>% 
  xlsx::write.xlsx(file = here('output', 'correlation-chla.xlsx'))

View(corr)

friendly_names <- tribble(
  ~Parameter2, ~friendly,
  "mei", "MEI",
  "nao", "NAO"
  "sal_avg", "Monthly\nSportabout"
  "sal_max_avg",
  "sal_max_prec_avg",
  "sal_min_avg",
  "sal_min_prec_avg",
  "sal_prec_avg",
  "temp_avg",
  "temp_max_avg",
  "temp_max_prec_avg",
  "temp_min_avg",
  "temp_min_prec_avg",
  "temp_prec_avg",
  "totprcp",
  "totprcp_prec",
)

corr_plot <- 
corr %>% 
  select(Group, Parameter2, rho, p) %>% 
  mutate(Group = factor(Group, levels = c("PC",
                                          "FM",
                                          "SS",
                                          "PI"))) %>% 
  filter(Parameter2 != "chla" & p < 0.05) %>% 
  ggplot(aes(x = Parameter2, y = Group)) +
  geom_tile(aes(fill = rho), color = "black") +
  scale_fill_distiller(palette = "RdBu", limits = c(-1, 1)) +
  geom_text(aes(label = round(rho, digits = 2)),
            size = 3) +
  theme_classic() +
  theme(axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.4,
                                   hjust = 1),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "top") +
  labs(x = "Parameter and Aggregation",
       y = "Station")

ggsave(corr_plot, filename = here('output', 'figures', 'finals',
                                  'figure12.png'),
       dpi = 600, units = "in",
       width = 6.5, height = 4.5)