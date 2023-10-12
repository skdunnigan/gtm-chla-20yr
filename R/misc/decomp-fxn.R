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