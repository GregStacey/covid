
require(ggplot2)
require(dplyr)
require(tidyr)
require(RCurl)
library(httr)
library(readr)
require(reshape2)


# read current covid data
read.covid.web = function(urlfile) {
  # # Rcurl
  # x = getURL(urlfile)
  # y = read.csv(text=x)
  # 
  # # httr
  # x = text=GET(urlfile), skip=7, header=T)
  # y = read.csv(text=x)
  # 
  # # readr
  # y = read_csv(url(urlfile))
}

read.jh.covid = function(fns) {
  # read JH death, confirmed, and recovered files
  # convert to
  #   - confirmed(t)
  #   - deaths(t)
  #   - recovered(t)
  #   - active_cases(t) = confirmed - deaths - recovered
  #   - death_rate(t) = deaths / confirmed
  # daily rates
  #   - daily confirmed cases
  #   - daily deaths
  #   - recovered
  # normalized
  #   - confirmed normalized by country
  #   - deaths normalized by country
  
  # read confirmed.csv
  confirmed = jh.csv.by.country(fns[1])
  names(confirmed) = c("country", "date", "confirmed")
  
  # read deaths.csv
  deaths = jh.csv.by.country(fns[2])
  names(deaths) = c("country", "date", "deaths")
  
  # read recovered.csv
  recovered = jh.csv.by.country(fns[3])
  names(recovered) = c("country", "date", "recovered")
  
  # collate
  if (!identical(confirmed[,c(1,2)], deaths[,c(1,2)]) & identical(confirmed[,c(1,2)], recovered[,c(1,2)])) {
    stop("collate doesn't work anymore!")
  }
  covid = cbind(confirmed, deaths[,3], recovered[,3])
  names(covid) = c("country", "date", "confirmed", "deaths", "recovered")
  
  # calculate daily numbers
  covid$daily.confirmed = calc.daily(covid, stat = "confirmed", by="country")
  covid$daily.deaths = calc.daily(covid, stat = "deaths", by="country")
  
  # calculate normalized
  # (... unfinished)
  print("reminder - normalizing is unfinished")
  pops = data.frame(country = c("US", "Canada", "Italy"),
                    value = c(330e6, 33e6, 66e6), stringsAsFactors = F)
  covid$confirmed.norm = calc.norm(covid, pops, stat="confirmed", by="country")
  covid$deaths.norm = calc.norm(covid, pops, stat="deaths", by="country")
  
  # calculate death rate
  covid$death.rate = covid$deaths/covid$confirmed
  
  return(covid)
}


jh.csv.by.country = function(fn) {
  # read confirmed.csv
  tmp = as.data.frame(read_csv(fn))
  names(tmp)[1:2] = c("province", "country")
  # melt
  tmp = melt(tmp, id.vars = c("province", "country", "Lat", "Long"))
  names(tmp) = c("province","country","lat","lon","date","confirmed")
  # add date
  tmp$date = as.Date(tmp$date,format="%m/%d/%y")
  xdate = function(x) {
    tmp = as.numeric(unlist(strsplit(as.character(x), "-")))
    return(tmp[1]*365 + tmp[2]*30 + tmp[3] - 737351)}
  tmp$date.x = unlist(sapply(tmp$date, FUN = xdate))
  # aggregate by country
  tmp = aggregate(tmp$confirmed,by = list(tmp$country, tmp$date.x), FUN = sum)
  
  return(tmp)
}

calc.daily = function(covid, stat = "confirmed", by = "country") {
  unqs = unique(covid[, by])
  tmp.daily = rep(NA, nrow(covid))
  for (ii in 1:length(unqs)) {
    I = covid[,by] == unqs[ii]
    # add a flag in case date is ever mis-ordereed
    if (!identical(covid$date[I], sort(covid$date[I], decreasing = F))) {
      stop("this doesn't work anymore!")
    }
    x = diff(covid[I,stat])
    tmp.daily[I] = c(0,x)
  }
  
  return(tmp.daily)
}

calc.norm = function(covid, pops, stat = "confirmed", by = "country") {
  unqs = unique(covid[, by])
  unqs = unqs[unqs %in% pops[,by]]
  tmp.norm = rep(NA, nrow(covid))
  for (ii in 1:length(unqs)) {
    I = covid[,by] == unqs[ii]
    ia = pops[,by] == unqs[ii]
    pop = pops$value[ia]
    tmp.norm[I] = covid[I,stat] / pop
  }
  
  return(tmp.daily)
}

sigmoid = function(x, A =1, mu=0, ss = 1) {
  A*1 / (1 + exp(-(x-mu) / ss))
}

my.ccf = function(x,y, lag=20) {
  lags = -lag : lag
  
  # add padding to y
  y.padded = c(rep(NA,lag), y, rep(NA,lag))
  
  # correlate
  rr = numeric(length(lags))
  for (ii in 1:length(lags)) {
    # apply lag to y.padded
    I = (1:length(x)) + (ii-1)
    y.lagged = y.padded[I]
    
    rr[ii] = cor.test(x, y.lagged)$estimate
  }
  return(rr)
}




