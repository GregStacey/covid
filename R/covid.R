
source("functions.R")


# get data
#fn.url = "https://github.com/CSSEGISandData/COVID-19/blob/master/archived_data/archived_time_series/time_series_2019-ncov-Confirmed.csv"
#covid = read.covid.web()

fns = c("../COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
        "../COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
cov = list()
for (ii in 1:length(fns)) {
  covid = as.data.frame(read_csv(fns[ii]))
  names(covid)[1:2] = c("province", "country")
  
  # melt
  covid = melt(covid, id.vars = c("province", "country", "Lat", "Long"))
  names(covid) = c("province","country","lat","lon","date","confirmed")
  
  # process data
  covid$date = as.Date(covid$date,format="%m/%d/%y")
  xdate = function(x) {
    tmp = as.numeric(unlist(strsplit(as.character(x), "-")))
    return(tmp[1]*365 + tmp[2]*30 + tmp[3] - 737351)
  }
  covid$date.x = unlist(sapply(covid$date, FUN = xdate))
  
  # aggregate by country
  cov[[ii]] = aggregate(covid$confirmed,
                  by = list(covid$country, covid$date.x),
                  FUN = sum)
}
cov2 = cov[[1]]
names(cov2) = c("country", "date", "confirmed")
cov2$deaths = cov[[2]]$x

# normalize c(confirmed,deaths) by country population
# .... 
# ....
cov2$confirmed.norm = cov2$confirmed
cov2$confirmed.norm[cov2$country == "US"] = cov2$confirmed.norm[cov2$country == "US"] / 3.3e8
cov2$confirmed.norm[cov2$country == "Canada"] = cov2$confirmed.norm[cov2$country == "Canada"] / 3.3e7

# calculate fatality rate
cov2$death.rate = cov2$deaths/cov2$confirmed


# plot
# canada v us, confirmed cases
I = cov2$country %in% c("US", "Canada")
df = rbind(as.matrix(cov2[I,c("country", "date", "deaths")]),
           as.matrix(cov2[I,c("country", "date", "confirmed.norm")]),
           as.matrix(cov2[I,c("country", "date", "death.rate")]))
df = as.data.frame(df)
names(df) = c("country", "date", "y")
df$country = as.character(df$country)
df$date = as.numeric(df$date)
df$y = as.numeric(df$y)
df$type = c(rep("deaths", sum(I)),rep("confirmed.per.capita", sum(I)), rep("death.rate", sum(I)))

df = melt(cov2[I,], id.vars = c("country", "date"), measure.vars = c("deaths", "death.rate", "confirmed.norm"))

ggplot(df, aes(x=date, y=value, color = country)) + facet_wrap(~variable, scales="free") +
  geom_line()

# current death rate by country
I = cov2$date == max(cov2$date)
tmp = cov2[I,]
tmp = tmp[tmp$confirmed > 250,]
ggplot(tmp, aes(x=country, y=death.rate)) + geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust =1))

# does death rate change with time?
big.countries = unique(cov2$country[cov2$date == max(cov2$date) & cov2$confirmed>1000])
I = cov2$country%in%big.countries & cov2$death.rate>0
ggplot(cov2[I,], aes(x=date, y=death.rate)) + geom_point(alpha = 0.5) +
  facet_wrap(~country, scales = "free") + geom_smooth(method = "lm")



