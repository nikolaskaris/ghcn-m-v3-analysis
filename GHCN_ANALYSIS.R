#------------------------------------------------------------------------------
# GHCN_ANALYSIS.R
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Let's first compare the adjusted and raw data from GHCN-M to see how much
# they differ.
#------------------------------------------------------------------------------
adj_nm$VALUE1 <- adj_nm$VALUE1/100 
raw_nm$VALUE1 <- raw_nm$VALUE1/100
adj_nm <- adj_nm[,c("year", "VALUE1")]
raw_nm <- raw_nm[,c("year", "VALUE1")]

ggplot() +
  geom_smooth(data = adj_nm, aes(x = year, y = VALUE1)) +
  geom_smooth(data = raw_nm, aes(x = year, y = VALUE1)) +
  xlab('year') +
  ylab('Average January Temperature Degrees Celsius') +
  ggtitle('Comparison of Raw and Adjusted Data')

save(adj_nm, file = "adjusted_for_comparison.RData")
save(raw_nm, file = "raw_for_comparison.RData") 

#------------------------------------------------------------------------------
# We want to look at average monthly temperature across North America from 1951-
# 2010. To do this we will take an average (mean) temperature for each month
# of each year and plot that temperature over time.
#------------------------------------------------------------------------------
# Now that we have no missing values we should calculate monthly mean 
# temperatures for each year in avg_nm_final
#------------------------------------------------------------------------------
# set up the range of years to calculate
#------------------------------------------------------------------------------

year0 <- 1951
year1 <- max(avg_nm_final[,3])
range <- year1 - year0 + 1

#------------------------------------------------------------------------------
# initialize some vectors to hold annual results
#------------------------------------------------------------------------------
avgmean <- minmean <- maxmean <- janmean <- rep(NA, range - 1)
avgyear <- minyear <- maxyear <- janyear <- rep(NA, range - 1)
avgcount <- mincount <- maxcount <-jancount <- rep(NA, range - 1)


#------------------------------------------------------------------------------
# loop through the range for annual averages for avg, min, max
#------------------------------------------------------------------------------
for (i in 1:range) {
  
  year <- i + year0 -1
  
  idx=avg_nm_final[,3]==year
  avgyr=avg_nm_final[idx,]
  minyr=min_nm_final[idx,]
  maxyr=max_nm_final[idx,]
  
  
  # calculate the annual means
  # divide by 10 since means are stored as 10ths of T
  # only use years with data for every month
  avgmean[i]=mean(rowMeans(avgyr[,c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)]))/100
  minmean[i]=mean(rowMeans(minyr[,c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)]))/100
  maxmean[i]=mean(rowMeans(maxyr[,c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)]))/100
  janmean[i]=mean(avgyr$VALUE1, na.rm=TRUE)
  
  
  # use every year, even those with partial data
  #lmean[i]=mean(rowMeans(mmyr[,4:15],na.rm=T),na.rm=T)/10
  
  avgyear[i] <- minyear[i] <- maxyear[i] <- janyear[i]<- year
  avgcount[i] <- jancount[i] <- nrow(avgyr)
  mincount[i] <- nrow(minyr)
  maxcount[i] <- nrow(maxyr)
    
}



#------------------------------------------------------------------------------
# Now loop through to calculate monthly averages

#------------------------------------------------------------------------------
lmonthly <-c()

for (year in unique(avg_nm_final$year)) {
  idx=avg_nm_final[,3]==year
  mmyr=avg_nm_final[idx,]
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    
    lmonthly <- rbind(lmonthly, mean(mmyr[,value])/100)
  }
}

lmonthly <- as.numeric(lmonthly)
lyear_month <- rep(avgyear, each = 12)
month <- rep(c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JLY', 'AUG',
               'SEP', 'OCT', 'NOV', 'DEC'), 60)

month_data <- data.frame(year= lyear_month,
                         month = month,
                         mean = lmonthly)

month_data$month <- factor(month_data$month, levels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JLY', 'AUG',
                                                        'SEP', 'OCT', 'NOV', 'DEC'))




#------------------------------------------------------------------------------
# Let's start exploring some graphics 
#------------------------------------------------------------------------------

fd <- data.frame(year = avgyear,
                          max = maxmean,
                          min = minmean,
                          avg = avgmean,
                          jan = janmean)
fd$period <- NA
for (i in 1:nrow(fd)) {
  if (fd$year[i] <=1980) {
    fd$period[i] <- 1
  } else {
    fd$period[i] <- 2
  }
}

library(ggplot2)

#------------------------------------------------------------------------------
# BARPLOT of average mean temperature superimposed with number of stations
# over time
#------------------------------------------------------------------------------

barplot(avgmean, ylim=c(9,12), xpd=F, col="tan", space=1)
par(new=T)
plot (avgcount ~ avgyear, type="l", xlab="year", ylab="Temp (C)",
      main="GHCN Average Temperature\nSimple Mean of Set",
      axes=F, ylim=c(500,2000))
axis(side=4, ylab="Station Count")
axis(side=1)
points(avgcount ~ avgyear, col="blue", pch=18, cex=1.5)
legend(1950,2000, c("Average Temp","Station Count"),
       pch=c(15,18), col=c("tan","blue"), lty=c(0,1))


#------------------------------------------------------------------------------
# ggplots yearLY
#------------------------------------------------------------------------------

avg <- ggplot(fd, aes(x = year, y = avg, fill = factor(period)))
min <- ggplot(fd, aes(x = year, y = min, fill = factor(period)))
max <- ggplot(fd, aes(x = year, y = max, fill = factor(period)))



avg + geom_line() + 
  geom_smooth(data=fd[fd$period == 2,], method=lm) +
  geom_smooth(data=fd[fd$period == 1,], method=lm) +
  xlab('Year') +
  ylab('Average Annual (Jan-Dec) Temperature Degrees Celsius') +
  ggtitle('Annual Temperature by Period (1: 1951-1980, 2: 1981-2010')

min + geom_line() +
  geom_smooth(method=lm) +
  geom_smooth(data=fd[fd$period == 2,], method=lm) + 
  xlab('Year') +
  ylab('Minimum Annual (Jan-Dec) Temperature Degrees Celsius') +
  ggtitle('Annual Temperature by Period (1: 1951-1980, 2: 1981-2010')

max + geom_line() +
  geom_smooth(method=lm) +
  geom_smooth(data=fd[fd$period == 2,], method=lm) + 
  xlab('Year') +
  ylab('Maximum Annual (Jan-Dec) Temperature Degrees Celsius') +
  ggtitle('Annual Temperature by Period (1: 1951-1980, 2: 1981-2010')

avg + geom_boxplot()

#------------------------------------------------------------------------------
# ggplots MONTHLY
#------------------------------------------------------------------------------

month_avg <- ggplot(month_data, aes(x = year, y = mean,
                                    color = month))

month_avg + geom_line() + geom_smooth(method=lm) +
  xlab("Year") +
  ylab("Annual Average Temperature by Month") +
  ggtitle("Annual Average Temperature by Month")

#------------------------------------------------------------------------------
# An exploration of explanatory variables. Let's see if including any other
# variables from the inv_nm_final improves our model
#------------------------------------------------------------------------------

# Baseline
lm.0 <- lm(avgmean ~ avgyear)
summary(lm.0)
qqnorm(lm.0$resid)
plot(lm.0$resid)

lm.jan <- lm(meanyear ~ YEAR, data = rd)
summary(lm.jan)
qqnorm(lm.0$resid)
plot(lm.0$resid)
# The baseline regression, year as an explanatory variable for temperature,
# is very good. 

# Just to make this data set easier to work with
rd <- avg_nm_final

# Let's add some variables from the inv dataset to see if we can make
# this regression more precise.



for (i in 1:nrow(rd)) {
  # Only mean concerning the same year as the row in rd
  newdata <- fd[fd$year == rd$YEAR[i],]
  monthdata <- month_data[month_data$year == rd$YEAR[i],]
  rd$meanyear[i] <- newdata$avg
  rd$meanJAN[i] <- monthdata$mean[monthdata$month == "JAN"]
  rd$meanFEB[i] <- monthdata$mean[monthdata$month == "FEB"]
  rd$meanMAR[i] <- monthdata$mean[monthdata$month == "MAR"]
  rd$meanAPR[i] <- monthdata$mean[monthdata$month == "APR"]
  rd$meanMAY[i] <- monthdata$mean[monthdata$month == "MAY"]
  rd$meanJUN[i] <- monthdata$mean[monthdata$month == "JUN"]
  rd$meanJLY[i] <- monthdata$mean[monthdata$month == "JLY"]
  rd$meanAUG[i] <- monthdata$mean[monthdata$month == "AUG"]
  rd$meanSEP[i] <- monthdata$mean[monthdata$month == "SEP"]
  rd$meanOCT[i] <- monthdata$mean[monthdata$month == "OCT"]
  rd$meanNOV[i] <- monthdata$mean[monthdata$month == "NOV"]
  rd$meanDEC[i] <- monthdata$mean[monthdata$month == "DEC"]
}

#------------------------------------------------------------------------------
# Adding variables from the inv_nm_final dataset
#------------------------------------------------------------------------------
for (i in 1:nrow(rd)) {
  rd$GRELEV[i] <- inv_nm_final$GRELEV[inv_nm_final$ID == rd$ID[i]]
  rd$POPSIZ[i] <- inv_nm_final$POPSIZ[inv_nm_final$ID == rd$ID[i]]
  rd$OCNDIS[i] <- inv_nm_final$OCNDIS[inv_nm_final$ID == rd$ID[i]]
}

save(rd, file = "rd.RDATA")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

lm.1 <- lm(meanyear ~ YEAR + GRELEV + POPSIZ + OCNDIS, data = rd)
summary(lm.1)
lm.2 <- lm(meanyear ~ YEAR, data = rd)
summary(lm.2)


lm.3 <- lm(avg ~ year + period*year, data = fd)
fd$pbin = ifelse(fd$year < 1980, 0, 1)

summary(lm.3)
qqnorm(lm.3$resid)
plot(lm.3$resid)

ggplot(fd, aes(year,avg)) + geom_smooth() +
  ylab("Average Annual (Jan-Dec) Temperature in Degrees Celsius") +
  xlab("Year")


# MOVING AVERAGE

#------------------------------------------------------------------------------
# Let's explore why 1950-1980 is doing weird things (i.e., decreasing).
# Let's look at other explanatory variables. All of them, perhaps.
#------------------------------------------------------------------------------

r2 = dropBoring(keepNumeric(rd))
require(reshape)
m = melt(r2, id.vars=c('YEAR'))
ggplot(r2, aes(x=YEAR, y=meanyear)) + geom_smooth()

# Seeing how every variable responds to year
ggplot(m, aes(x=YEAR, y=value)) + 
  geom_smooth() +
  facet_wrap(~variable, scales='free_y') +
  ggtitle("All Explanatory Variables Against Year")

# CNTRYCODE and GRELEV are the only non-value explanatory variables that
# resemble the dip in mean monthly temperature, and the variance is small.
# Let's see if we can incorporate other explanatory variables from other 
# data sources. Let's look at North American CO2 Emissions and rate of 
# GDP growth over this period. 
#------------------------------------------------------------------------------
# CO2
# Data Source: http://cdiac.ornl.gov/CO2_Emission/timeseries/regional
# Carbon Dioxide Information Analysis Center
#------------------------------------------------------------------------------
co2 <- read.csv("Regional_CO2_emission_5454.csv", as.is=TRUE, skip=22)
co2 <- co2[-1,(1:2)]
fd$co2 <- co2[,2]


lm.4 <- lm(avg ~ co2, data = fd)
summary(lm.4)
qqnorm(lm.4$resid)
plot(lm.4$resid)

ggplot(fd, aes(x = year, y = co2)) +
  geom_line() +
  geom_smooth() +
  ylab("CO2 in Thousands of Metric Tons") +
  xlab("Year")
 
#------------------------------------------------------------------------------
# Let's look at GDP growth in North America over this period
# Data Source: http://www.worldeconomics.com/Data/MadisonHistoricalGDP/Madison%20Historical%20GDP%20Data.efp
# Maddison North America GDP Data
#------------------------------------------------------------------------------
gdp <- read.csv("fredgraph (1).csv", as.is=TRUE, skip = 11, header = FALSE)
gdp <- gdp[2:61,]
names(gdp) <- c("year",  "total")


# This data is missing two years: 2009 and 2010. Since we care more about the 
# earlier period, this isn't problematic. 

fd$gdp <- gdp$total


ggplot(fd, aes(x = year, y = gdp)) +
  geom_line() +
  geom_smooth() +
  ylab("Annual GDP in Billions of USD") +
  xlab("Year")

lm.5 <- lm(avg ~ gdp, data =fd)
summary(lm.5)
qqnorm(lm.5$resid)
plot(lm.5$resid)

lm.6<- lm(avg ~ year, data =fd)
summary(lm.6)
#------------------------------------------------------------------------------
# GDP is also a good explanatory variabl but does little to explain the dip
# in temperature between 1950 and 1980
#------------------------------------------------------------------------------
# The last place we'll look is an extended view of temperature readings
# from 1900-present day. Let's see if the downward trend if annual/monthly 
# average temperatures from 1950-1965/70 is a trend or a hiccup in an otherwise
# increase over the 20th century
#------------------------------------------------------------------------------

avg <- avgDATA[(avgDATA$CNTRYCODE == 425 |
                avgDATA$CNTRYCODE == 403 |
                avgDATA$CNTRYCODE == 414),]

# Count NAs in each column
numNAs <- apply(avg, 1, function(z) sum(is.na(z)))
a_full <- avg[!(numNAs > 6),]

# Fill in missing values with means from the year
for (i in 1:nrow(a_full)) {
  temp_mean <- sum(a_full[i,5],
                   a_full[i,9],
                   a_full[i,13],
                   a_full[i,17],
                   a_full[i,21],
                   a_full[i,25],
                   a_full[i,29],
                   a_full[i,33],
                   a_full[i,37],
                   a_full[i,41],
                   a_full[i,45],
                   a_full[i,49],
                   na.rm=TRUE) / 12
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    if (is.na(a_full[i,value])) {
      a_full[i,value] <- temp_mean
    } else {
      next}
  }
}


save(a_full, file = "a_full.RDATA")


# find average temp values for years from 1900-2010
yrmax <- max(a_full[,3])
range_full <- yrmax - min(a_full[,3]) + 1

mean_full <- rep(NA, range_full)
year_full <-  rep(NA, range_full)
count_full <- rep(NA, range_full)



for (i in 1:range_full) {
  
  year <- i + min(a_full[,3]) -1
  
  idx=a_full[,3]==year
  avgyr=a_full[idx,]

  
  
  # calculate the annual means
  # divide by 10 since means are stored as 10ths of T
  # only use years with data for every month
  mean_full[i]=mean(rowMeans(avgyr[,c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)]))/100
  
  # use every year, even those with partial data
  #lmean[i]=mean(rowMeans(mmyr[,4:15],na.rm=T),na.rm=T)/10
  
  year_full[i] <- year
  count_full[i] <- nrow(avgyr)

}

data_full <- data.frame(year = year_full,
                       avg = mean_full)
data_full <- data_full[data_full$year <= 2014,]
data_half <- data_full[data_full$year >= 1850,]

dt <- ggplot(data_full, aes(x = year, y = avg))
dt + geom_line()
dh <- ggplot(data_half, aes(x = year, y = avg))
dh + geom_line() +
  geom_smooth(method=lm) +
  ylab("Average Annual Temperature in Degrees Celsius") +
  xlab("Year")

lm.7 <- lm(avg ~ year, data = data_half)
summary(lm.7)
qqnorm(lm.7$resid)
plot(lm.7$resid)

#------------------------------------------------------------------------------
# Anomalies analysis
#------------------------------------------------------------------------------


# bl == baseline
bl50 <- mean(fd$avg[fd$year <= 1980])
bl80 <- mean(fd$avg[fd$year >= 1981])
blall <- mean(fd$avg)
fd$anomaly50 <- 0
fd$anomaly80 <- 0
fd$anomalyall <- 0

for (i in 1:nrow(fd)) {
fd$anomaly50[i] <- fd$avg[i] - bl50
fd$anomaly80[i] <- fd$avg[i] - bl80
fd$anomalyall[i] <- fd$avg[i] -blall
}

fd$sign50 <- ifelse(fd$anomaly50 >= 0, "positive", "negative")
fd$sign80 <- ifelse(fd$anomaly80 >= 0, "positive", "negative")
fd$signall <- ifelse(fd$anomalyall >= 0, "positive", "negative")

anom50 <- ggplot(fd, aes(x=year, y=anomaly50, fill=sign50))
anom50 + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("positive" = "red", "negative" = "blue")) +
  ylab("Distance from Mean, Degrees Celsius") +
  xlab("Year") +
  ggtitle("Temperature Anomalies Relative to 1951-1980 Baseline")

anom80 <- ggplot(fd, aes(x=year, y=anomaly80, fill=sign80))
anom80 + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("positive" = "red", "negative" = "blue")) +
  ylab("Distance from Mean, Degrees Celsius") +
  xlab("Year") +
  ggtitle("Temperature Anomalies Relative to 1980-2010 Baseline")

anomall <- ggplot(fd, aes(x = year, y = anomalyall, fill = signall))
anomall + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("positive" = "red", "negative" = "blue")) +
  ylab("Distance from Mean, Degrees Celsius") +
  xlab("Year") +
  ggtitle("Temperature Anomalies Relative to 1951-2010 Baseline")


#------------------------------------------------------------------------------
# RATE OF WARMING
#------------------------------------------------------------------------------
# Let's finally calculate the rate of warming per year and per decade
# across North America for the two periods
#------------------------------------------------------------------------------

a<- getRate(fd$year, fd$avg)
  
fd_1 <- fd[fd$year <=1980,]
fd_2 <- fd[fd$year >= 1981,]

b <- getRate(fd_2$year, fd_2$avg)

c <- getRate(fd_1$year, fd_1$avg)

rates <- data.frame(c("1951-2010", "1951-1980", "1981-2010"),
                    c(a, c, b))

names(rates) <- c("Period", "Rate of Warming")



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------