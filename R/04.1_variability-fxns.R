
# quantify variability ----------------------------------------------------

library(here)
source(here('R', '00_loadpackages.R'))


# decomp-fxn --------------------------------------------------------------


# function comes from Cloern and Jassby 2010 Supplemental Material

decomp.mult <- function(x, startyr = NULL , endyr = NULL, event = T){
  #R2 .8.0 3/2/09 4:48 PM
  if(class(x)[1]!='mts') dim(x)=c(length(x),1)
  
  if(is.null(startyr)) startyr <- start(x)[1]
  
  if(is.null(endyr)) endyr <- end(x)[1]
  
  d <- window(x, start = c(startyr,1), end = c(endyr,12), extend = T)
  
  results <- vector('list',dim(d)[2])
  
  names(results)= colnames(d)
  
  for(site in colnames (d)){
    d1=d[,site]
    
    #long-term mean
    grandmean = mean(d1, na.rm = T)
    
    # annual component
    annualmean = aggregate(d1,1, mean, na.rm = T)
    annualmeanreps = as.vector(t(matrix(rep(annualmean,12), 
                                        ncol = 12)))
    interann = ts(annualmeanreps,s=c(startyr,1),f = 12) / grandmean
    # remaining components
    if(event) {
      # monthly component
      d2 = matrix(d1, nrow = 12)
      monthdev = sweep(d2,2, annualmean ,'/')
      monthmean = apply(monthdev,1, mean , na.rm = T)
      season = ts(rep(monthmean, endyr - startyr + 1), 
                  s=c(startyr, 1), f = 12)
      # events component
      resids = sweep(monthdev, 1, monthmean , '/')
      events = ts(as.vector(resids),
                  s=c(startyr, 1),f = 12)
    }
    else {
      # monthly component
      season = d1/(grandmean * interann)
    }
    # prepare output
    if(event) dcomp = ts.union(d1, grandmean, interann, season,
                               events) else dcomp =ts.union (d1, grandmean, interann,
                                                             season)
    colnames(dcomp)[1]= 'original'
    results[[site]]= dcomp
  }
  if(class(x)[1]!='mts') results[[1]] else results
}


# full figure -------------------------------------------------------------

var_fig <- function(dat, site, save){
  
  filename <- paste0(site,"-variability.png")
  
  ts <- ts(as.data.frame(dat %>%
                           ungroup() %>% 
                           select(date, value) %>%
                           arrange(date) %>%
                           select(value)
  ),
  start = c(2003, 1),
  end = c(2022, 12),
  frequency = 12
  )
  
  decomp <- data.frame(decomp.mult(ts)
  ) %>% 
    rename(value = original) %>% 
    bind_cols((dat %>% 
                 select(date, value) %>% 
                 arrange(date)
    )) %>% 
    select(-7) %>% 
    rename(value = 1)
  
  
  a <- ggplot(decomp) +
    geom_line(aes(x = date, y = value), color = "blue") +
    scale_x_date(date_breaks = "year", date_labels = "%Y") +
    theme_bw(base_family = "serif") +
    theme(axis.text.x = element_text(size = 12, colour = c(NA, NA, "black", NA, NA)),
          panel.grid.minor = element_blank(),
          axis.text = element_text(color = "black")) +
    labs(x = '',
         y = 'Chl-a \u00b5g/L',
         title = site)
  b <- decomp %>% 
    mutate(year = year(date)) %>% 
    select(year, interann) %>% 
    unique() %>% 
    ggplot() +
    geom_col(aes(x = year, y = interann-1), fill = "tomato") +
    geom_hline(yintercept = 0, color = "tomato") +
    scale_x_continuous(breaks = seq(2000, 2022, by = 1)) +
    scale_y_continuous(limits = c(-1,1)) +
    theme_bw(base_family = "serif") +
    theme(axis.text.x = element_text(size = 12, colour = c(NA, NA, NA, "black", NA)),
          panel.grid = element_blank(),
          axis.text = element_text(color = "black")) +
    labs(x = '',
         y = 'Annual')
  c <- ggplot(decomp) +
    geom_col(aes(x = date, y = events-1), fill = "darkorchid4") +
    geom_hline(yintercept = 0, color = "darkorchid4") +
    scale_x_date(date_breaks = "year", date_labels = "%Y") +
    scale_y_continuous(limits = c(-1,2)) +
    theme_bw(base_family = "serif") +
    theme(axis.text.x = element_text(size = 12, colour = c(NA, NA,"black", NA, NA)),
          panel.grid = element_blank(),
          axis.text = element_text(color = "black")) +
    labs(x = '',
         y = 'Residual')
  d <- decomp %>% 
    mutate(month = month(date, label = T)) %>% 
    select(month, season) %>% 
    unique %>% 
    ggplot() +
    geom_col(aes(x = month, y = season-1), fill = "darkgreen") +
    geom_hline(yintercept = 0, color = "darkgreen") +
    scale_y_continuous(limits = c(-1,1)) +
    theme_bw(base_family = "serif") +
    theme(axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank()) +
    labs(x = '',
         y = 'Season')
  multi <- a/b/c/d
  
  if (save == TRUE) {
    ggsave(multi, filename = here('output', 'figures', 'variability', filename))
  }
  else {
    print(multi)
  }
  
}


# seasonal_fig ------------------------------------------------------------

seas_var_fig <- function(dat){
  
  filename <- paste0("seasonal-var.png")
  
  ts <- ts(as.data.frame(dat %>%
                           ungroup() %>% 
                           select(date, value) %>%
                           arrange(date) %>%
                           select(value)
  ),
  start = c(2003, 1),
  end = c(2022, 12),
  frequency = 12
  )
  
  decomp <- data.frame(decomp.mult(ts)
  ) %>% 
    rename(value = original) %>% 
    bind_cols((dat %>% 
                 select(date, value) %>% 
                 arrange(date)
    )) %>% 
    select(-7) %>% 
    rename(value = 1)
  
  d <- decomp %>% 
    mutate(month = month(date, label = T)) %>% 
    select(month, season) %>% 
    unique %>% 
    ggplot() +
    geom_segment(aes(x = month, xend = month, y = 0, yend = season-1), color = "black") +
    geom_point(aes(x = month, y = season-1), size = 3,color = "darkgreen") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    scale_y_continuous(limits = c(-1,1)) +
    theme_bw(base_family = "serif") +
    theme(axis.text = element_text(size = 12, color = "black"),
          panel.grid = element_blank()) +
    labs(x = '',
         y = 'Season')
  
  print(d)
}


# standard deviations of coefficients -------------------------------------



sd_fxn <- function(dat, station) {
  
  ts <- ts(as.data.frame(dat %>%
                           ungroup() %>% 
                           select(date, value) %>%
                           arrange(date) %>%
                           select(value)
  ),
  start = c(2003, 1),
  end = c(2022, 12),
  frequency = 12
  )
  
  decomp <- data.frame(decomp.mult(ts)
  ) %>% 
    rename(value = original) %>% 
    bind_cols((dat %>% 
                 select(date, value) %>% 
                 arrange(date)
    )) %>% 
    select(-7) %>% 
    rename(value = 1)
  
  
  sd <- decomp %>% summarize_all(., ~sd(.x, na.rm = T)) %>% select(-1, -2, -date) %>% mutate(station = station)
  print(sd)
}
