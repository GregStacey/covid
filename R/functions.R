
require(ggplot2)
require(dplyr)
require(tidyr)
require(RCurl)
library(httr)
library(readr)


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



