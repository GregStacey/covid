
source("functions.R")


#################### get data
fns = c("../COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
        "../COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
        "../COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
covid = read.jh.covid(fns)

# define some useful things
big.countries = unique(covid$country[covid$date == max(covid$date) & covid$confirmed>250])
really.big.countries = unique(covid$country[covid$date == max(covid$date) & covid$confirmed>750])
tmp = aggregate(covid$deaths, by=list(covid$country), FUN=sum)
countries.with.deaths = tmp$Group.1[tmp$x>0]



#################### plot
# canada, US, italy
I = covid$country %in% c("US", "Canada", "Italy")
df = melt(covid[I,], id.vars = c("country", "date"), measure.vars = c("deaths", "death.rate", "confirmed.norm"))
ggplot(df, aes(x=date, y=value, color = country)) + facet_wrap(~variable, scales="free") +
  geom_line()
ggsave("../figures/canada_us_italy.png", width=9, height=2)


# current death rate by country
I = covid$date == max(covid$date)
tmp = covid[I,]
tmp = tmp[tmp$confirmed > 250,]
ggplot(tmp, aes(x=country, y=death.rate)) + geom_point() +
  theme(axis.text.x = element_text(angle=45, hjust =1))
ggsave("../figures/deathrate_by_country.png", width=7, height=2)


# does death rate change with time?
I = covid$country %in% intersect(big.countries, countries.with.deaths)
ggplot(covid[I,], aes(x=date, y=death.rate)) + geom_line() +
  facet_wrap(~country) + theme_bw() + 
  coord_cartesian(ylim = c(0,.12), xlim = c(20, max(covid$date)))
ggsave("../figures/deathrate_vs_time.png", width=14, height=14)


# cross-cor of deaths and confirmed
lag = matrix(NA, nrow=length(really.big.countries),ncol=29)
for (ii in 1:length(really.big.countries)) {
  I = covid$country == really.big.countries[ii]
  xx = covid$date[I]
  conf = covid$confirmed[I]
  dead = covid$deaths[I]
  tmp = ccf(conf, dead, plot = F, lag.max = 14)
  lag[ii,] = tmp$acf
}
# make vector to sort by
nn = apply(lag, 1, FUN = function(x) {
  x = (x-min(x)) / (max(x) - min(x))
  tmp = mean(x* (-14:14))
})
badc = really.big.countries[is.nan(nn)]
#
lag = as.data.frame(lag)
names(lag) = tmp$lag
lag$country = really.big.countries
lag2 = melt(lag, id.vars = "country")
lag2$variable = as.numeric(as.character(lag2$variable))
lag2$country = factor(lag2$country, levels = really.big.countries[order(nn)])
lag2 = lag2[!lag2$country %in% badc,]
p1 = ggplot(lag2, aes(x=variable, y=country, fill=value)) + geom_tile() +
  scale_fill_viridis(name = "Pearson\ncorrelation") + 
  scale_x_continuous(breaks=c(-14,-10, -5, 0, 5, 10,14),
                     labels=c("Cases come first","-10", "-5", "0", "5", "10", "Deaths come first")) +
  xlab("Lag between new cases and deaths, days") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 
# schematic plots
# aus
I = covid$country=="Australia"
df = data.frame(xx = covid$date[I],
                cases = covid$confirmed[I],
                deaths = covid$deaths[I], stringsAsFactors = F)
df$cases = df$cases / max(df$cases, na.rm=T)
df$deaths = df$deaths / max(df$deaths, na.rm=T)
df = melt(df, id.vars = "xx")
p2 = ggplot(df, aes(x=xx, y=value, color=variable)) + geom_line() + theme_bw() +
  xlab("") + ylab("") + ggtitle("Australia") +theme(axis.text.y = element_blank())
# US
I = covid$country=="US"
df = data.frame(xx = covid$date[I],
                cases = covid$confirmed[I],
                deaths = covid$deaths[I], stringsAsFactors = F)
df$cases = df$cases / max(df$cases, na.rm=T)
df$deaths = df$deaths / max(df$deaths, na.rm=T)
df = melt(df, id.vars = "xx")
p3 = ggplot(df, aes(x=xx, y=value, color=variable)) + geom_line() + theme_bw() +
  xlab("") + ylab("") + ggtitle("USA") +theme(axis.text.y = element_blank())
# s korea
I = covid$country=="Korea, South"
df = data.frame(xx = covid$date[I],
                cases = covid$confirmed[I],
                deaths = covid$deaths[I], stringsAsFactors = F)
df$cases = df$cases / max(df$cases, na.rm=T)
df$deaths = df$deaths / max(df$deaths, na.rm=T)
df = melt(df, id.vars = "xx")
p4 = ggplot(df, aes(x=xx, y=value, color=variable)) + geom_line() + theme_bw() +
  xlab("time, days since Jan 22") + ylab("") + ggtitle("S Korea") +theme(axis.text.y = element_blank())
#layout
p1 | (p2 / p3 / p4)
ggsave("../figures/xcor_deaths_vs_confirmed.png", width=9, height=8)

# new cases vs time
I = covid$country %in% intersect(big.countries, countries.with.deaths)
ggplot(covid[I,], aes(x=date, y=daily.confirmed)) + geom_line() +
  facet_wrap(~country, scales = "free") + theme_bw() 
ggsave("../figures/newcases_vs_time.png", width=14, height=14)

# total sick vs time
I = covid$country %in% intersect(big.countries, countries.with.deaths)
ggplot(covid[I,], aes(x=date, y=confirmed - deaths - recovered)) + geom_line() +
  facet_wrap(~country, scales = "free") + theme_bw() + ggtitle("Number of active cases")
ggsave("../figures/activecases_vs_time.png", width=14, height=14)



# active cases:
# which countries are on the upstroke? which are at the peak? which are past the peak?
gaussian = function(x, y, a=1, b=1, c=1) {
  yfit = a * exp(-(x-b)^2 / 2 / c^2)
  return(mean((y - yfit)^2))
}

par = c(a= 60000, b= 30, c = 10)
lower = c(a=0, b=1, c = 1)
upper = c(a=1e9, b=1000, c = 1e3)
I = covid$country=="China"
x = covid$date[I]
y = covid$confirmed[I] - covid$recovered[I] - covid$deaths[I]
tmp = optim(par, fn=gaussian, x=x, y=y, method = "L-BFGS-B", lower = lower, upper = upper)



