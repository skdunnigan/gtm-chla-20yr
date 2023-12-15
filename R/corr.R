# combine monthly total rainfall, chla, avg temp, avg sal, mei for correlation

# load packages
library(here)
source(here('R', '00_loadpackages.R'))
library(ggcorrplot)

# load correlation data for PI

pi_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "PI") 
ss_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "SS") 
fm_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "FM") 
pc_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "PC") 

corr_plot_fxn <- function(dat) {

dat <- dat %>% 
  select(-datetimestamp) %>% 
  data.matrix()

# compute correlation matrix
corr <- cor(dat, method = "spearman", use = "everything")
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
           colors = c("#0075AC", "white", "tomato")) +
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
    plot.title = element_text(size = 16, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic'))

print(a)
}


# run correlation plot creation
corr_plot_fxn(pi_cor)
corr_plot_fxn(ss_cor)
corr_plot_fxn(fm_cor)
corr_plot_fxn(pc_cor)
