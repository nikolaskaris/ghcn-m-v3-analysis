#------------------------------------------------------------------------------
# GHCN_CLEAN
#
# Loading each data set and labeling it 'avg', 'max', or 'min'. Here we'll
# also load the avg raw data set for purposes of comparing the raw with 
# adjusted. If they behave in a similar fashion, we will limit our analysis to
# the adjusted (since NOAA has simply made quality control adjustments and 
# condensed multiple time series into single rows, I suspect this to be the 
# case.
#------------------------------------------------------------------------------
rm(list=ls())
load("tavg-qca.RData")
avgDATA <- dat
avgINV <- inv
rm(dat, inv)
load("tmax-qca.RData")
maxDATA <- dat
maxINV <- inv
rm(dat, inv)
load("tmin-qca.RData")
minDATA <- dat
minINV <- inv
rm(dat, inv)
load("tavg-qcu.RData")
avgUDATA <- dat
avgUINV <- inv
rm(dat, inv)

#------------------------------------------------------------------------------
# Downloading country codes 
#------------------------------------------------------------------------------
codes <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/country-codes",
                         as.is=TRUE, header=FALSE, strip.white=TRUE)
as.data.frame(codes)
codes$codes <- NA
codes$countries <- NA
# Populate codes and countries with substrings of the initial character
# vectors.
for (i in 1:nrow(codes)) {
  codes$codes[i] <- substr(codes$V1[i], 1, 3)
}
for (i in 1:nrow(codes)) {
  codes$countries[i] <- substr(codes$V1[i], 4, nchar(codes[i,]))
}
codes$V1 <- NULL
# Trim out whitespace for subsetting
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
codes$countries <- trim(codes$countries)
#------------------------------------------------------------------------------
# As per the project description on the course website, we'll construct a 
# reduced size data set concerning only North American meteorological stations
# from 1950 to 2010. USA country code == 425 / CANADA country code == 403 / 
# MEXICO country code == 414
#------------------------------------------------------------------------------
avg_nm <- avgDATA[(avgDATA$YEAR >= 1951 &
                     avgDATA$YEAR <= 2010 &
                       (avgDATA$CNTRYCODE == 425 |
                        avgDATA$CNTRYCODE == 403 |
                        avgDATA$CNTRYCODE == 414)),]
#------------------------------------------------------------------------------
# Here we're going to construct raw and adjusted data sets for the full length
# of recorded observations to compare how the two sets of data behave
#------------------------------------------------------------------------------
# Raw all years
raw_nm <- avgUDATA[(avgUDATA$CNTRYCODE == 425 |
                      avgUDATA$CNTRYCODE == 403 |
                      avgUDATA$CNTRYCODE == 414),]
# Adjusted all years
adj_nm <- avgDATA[(avgDATA$CNTRYCODE == 425 |
                    avgDATA$CNTRYCODE == 403 |
                    avgDATA$CNTRYCODE == 414),]
#------------------------------------------------------------------------------
# We now need to remove stations with too many missing years across this period.
#------------------------------------------------------------------------------
require(dplyr)
# Avg adjusted data set for 1950-2010 ONLY
counts = group_by(avg_nm, ID) %>% summarise(num = length(ID))
avg_nm = inner_join(avg_nm, counts, by = 'ID')
avg_nm$missing = 60 - avg_nm$num


# Raw data set for all observed years
counts = group_by(raw_nm, ID) %>% summarise(num = length(ID))
raw_nm = inner_join(raw_nm, counts, by = 'ID')
raw_nm$missing = 60 - raw_nm$num
# Adjusted data set for all observed years
counts = group_by(adj_nm, ID) %>% summarise(num = length(ID))
adj_nm = inner_join(adj_nm, counts, by = 'ID')
adj_nm$missing = 60 - adj_nm$num

# We'll determine where to make the cutoff by the distribution.
hist(avg_nm$missing,
     breaks = 60,
     main = "Number of Missing Years of Data from 1951 - 2010",
     xlab = "Number of Missing Years",
     ylab = "Frequency")

#------------------------------------------------------------------------------
# It looks like there's a high concentration of stations missing less than
# 6-7 years of data, many missing 19, and the rest fairly low-frequency. 
# We'll cutoff at 15 - stations missing more than 25% of years from 1951 - 
# 2010 will not be included.We'll say if stations are missing more than 
# 15 years of data, they will be removed from the data set. Since the data set
# is so large, this is a reasonable arbitrary decision to make.
#------------------------------------------------------------------------------

# Average 1950 - 2010
avg_nm <- avg_nm[avg_nm$missing <= 15,]
# Raw all years
raw_nm <- raw_nm[raw_nm$missing <=15,]
# Adjusted all years
adj_nm <- adj_nm[adj_nm$missing <=15,]
#-----------------------------------------------------------------------------
# We'll similarly make a new inventory set containing only North American 
# stations and ones that are in avg_nm only -- meaning only including stations
# that have data between 1951 and 2010.
#-----------------------------------------------------------------------------
inv_nm <- avgINV[avgINV$ID %in% avg_nm$ID,]

#-----------------------------------------------------------------------------
# Now that we have our dataset containing only station measurements since 1950
# and limited to North America (USA, CANADA, MEXICO), we can begin to plot
# the rate of warming from 1951-1980 and 1981-2010. 
#-----------------------------------------------------------------------------

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim=c(-177,-30), ylim =c(14.92, 85), asp = 1,
     main = "NORTH AMERICAN STATIONS WITH DATA FROM 1951 TO 2010
    AND NO MORE THAN 15 YEARS MISSING")
points(inv_nm$LONGITUDE, inv_nm$LATITUDE, col = "red",pch = 6, cex = .2)

#-----------------------------------------------------------------------------
# It doesn't appear that there are too many missing regions for weather data
# in the US. MEXICO is more sparse but consistently distributed, and CANADA
# has a considerably higher density of stations near the US border than it 
# does further north. We'll take this into account later.
#-----------------------------------------------------------------------------
# Another problem with this dataset is missing values. Many stations have 
# extended periods without data observations. Let's see how many of the 
# stations in our newly constructed norAM datasets have fewer than 60 missing
# months of data from 1951 to 2010 (5 years worth of data, or less than
# 10% missing).
#-----------------------------------------------------------------------------
avg_nm$NAs <- 0
# Number of distinct stations in the avg_nm data set
length(unique(avg_nm$ID))
# We want to count the number of those that have more than 60 NA values 
# across all of their time series.
# Counts number of missing values in each row and adds the total number to 
# column NAs
for (i in 1:nrow(avg_nm)) {
  count <- 0
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    if (is.na(avg_nm[i,value])) {
      count <- count + 1
    } else {
      next}
  }
  avg_nm$NAs[i] <- count
}
avg_nm$NA_TOTAL <- 0
# For each distinct station ID in avg_nm, sums the total missing values
# for that station over the period 1951-2010, and stores that value in each 
# row of the data frame in column GOOD_TOTAL
for (ID in unique(avg_nm$ID)) {
  newdata <- avg_nm[avg_nm$ID == ID,]
  count <- sum(newdata$NAs)
  avg_nm$NA_TOTAL[avg_nm$ID == ID] <- count
}
# _final stations kept
avg_nm_final <- avg_nm[avg_nm$NA_TOTAL <= 59,]
# Stations _removed
avg_nm_removed <- avg_nm[avg_nm$NA_TOTAL > 59,]
# inventory stations kept
inv_nm_final <- inv_nm[inv_nm$ID %in% avg_nm_final$ID,]
# inventory stations _removed
inv_nm_removed <- inv_nm[inv_nm$ID %in% avg_nm_removed$ID,]
# plot to make sure that the stations we are removing from our analysis
# are evenly distributed across North America (more or less). If they were
# geographically concentrated we would have a problem.
newmap2 <- getMap(resolution = "low")
plot(newmap2, xlim=c(-177,-30), ylim =c(14.92, 85), asp = 1,
     main = "STATIONS 1951 - 2010",
     legend(-60, 40, c("Kept", "Removed"), pch = c(6, 4)))
points(inv_nm_final$LONGITUDE,
       inv_nm_final$LATITUDE,
       col = "red",
       pch = 6,
       cex = .3)
points(inv_nm_removed$LONGITUDE,
       inv_nm_removed$LATITUDE,
       col = "blue",
       pch = 4,
       cex = 1)

# We have now filtered our original average temperature data set to include
# only stations that have at least 45 of 60 years of monthly average surface
# temperature readings from 1951-2010, and also only stations that have less
# than 60 missing months of data from that period. 

#------------------------------------------------------------------------------
# No we remove rows with more than 6 NA values and replace NAs with mean values 
# from those rows. This will simplify the analysis later on.
#------------------------------------------------------------------------------
# AVERAGE
#------------------------------------------------------------------------------
avg_nm_final <- avg_nm_final[avg_nm_final$NAs<=6,]
for (i in 1:nrow(avg_nm_final)) {
  temp_mean <- sum(avg_nm_final[i,5],
                   avg_nm_final[i,9],
                   avg_nm_final[i,13],
                   avg_nm_final[i,17],
                   avg_nm_final[i,21],
                   avg_nm_final[i,25],
                   avg_nm_final[i,29],
                   avg_nm_final[i,33],
                   avg_nm_final[i,37],
                   avg_nm_final[i,41],
                   avg_nm_final[i,45],
                   avg_nm_final[i,49],
                   na.rm=TRUE) / 12
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    if (is.na(avg_nm_final[i,value])) {
      avg_nm_final[i,value] <- temp_mean
    } else {
      next}
  }
}

#------------------------------------------------------------------------------
# Let's make sure we've got rid of the NA values. We'll run through
# the loop we did in the clean again.
#------------------------------------------------------------------------------
for (i in 1:nrow(avg_nm_final)) {
  count <- 0
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    if (is.na(avg_nm_final[i,value])) {
      count <- count + 1
    } else {
      next}
  }
  avg_nm_final$NAs[i] <- count
}
table(avg_nm_final$NAs)
#------------------------------------------------------------------------------
# Now that we have the stations with data, let's make sure the same set has
# data for Min and Max temp readings also
#------------------------------------------------------------------------------
max_nm <- maxDATA[maxDATA$ID %in% avg_nm_final$ID,]
min_nm <- minDATA[minDATA$ID %in% avg_nm_final$ID,]

min_nm_final <- min_nm[min_nm$YEAR >= 1951 &
                       min_nm$YEAR <= 2010,]
max_nm_final <- max_nm[max_nm$YEAR >= 1951 &
                       max_nm$YEAR <= 2010,]
#------------------------------------------------------------------------------
# MIN
#------------------------------------------------------------------------------
for (i in 1:nrow(min_nm_final)) {
  temp_mean <- sum(min_nm_final[i,5],
                   min_nm_final[i,9],
                   min_nm_final[i,13],
                   min_nm_final[i,17],
                   min_nm_final[i,21],
                   min_nm_final[i,25],
                   min_nm_final[i,29],
                   min_nm_final[i,33],
                   min_nm_final[i,37],
                   min_nm_final[i,41],
                   min_nm_final[i,45],
                   min_nm_final[i,49],
                   na.rm=TRUE) / 12
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    if (is.na(min_nm_final[i,value])) {
      min_nm_final[i,value] <- temp_mean
    } else {
      next}
  }
}
#------------------------------------------------------------------------------
# MAX
#------------------------------------------------------------------------------
for (i in 1:nrow(max_nm_final)) {
  temp_mean <- sum(max_nm_final[i,5],
                   max_nm_final[i,9],
                   max_nm_final[i,13],
                   max_nm_final[i,17],
                   max_nm_final[i,21],
                   max_nm_final[i,25],
                   max_nm_final[i,29],
                   max_nm_final[i,33],
                   max_nm_final[i,37],
                   max_nm_final[i,41],
                   max_nm_final[i,45],
                   max_nm_final[i,49],
                   na.rm=TRUE) / 12
  for (value in c(5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49)) {
    if (is.na(max_nm_final[i,value])) {
      max_nm_final[i,value] <- temp_mean
    } else {
      next}
  }
}

#------------------------------------------------------------------------------
# Good! We can now remove the NAs and the TOTAL_NAs
#------------------------------------------------------------------------------
avg_nm_final$NAs <- NULL
avg_nm_final$NA_TOTAL <- NULL
#------------------------------------------------------------------------------
# _final avg dataset, min dataset, and max dataset
#------------------------------------------------------------------------------

save(avg_nm_final, file = "avg_nm_final.RDATA")
save(min_nm_final, file = "min_nm_final.RDATA")
save(max_nm_final, file = "max_nm_final.RDATA")
save(inv_nm_final, file = "inv_nm_final.RDATA")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------