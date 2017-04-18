library(foreign)
library(ggplot2)
#library(plyr)
# IPCSR 9028 - 
# http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/9028?sortBy=&searchSource=revise&q=ICPSR+9028
# 
df <- read.spss("09028-0098-Data.sav", to.data.frame = TRUE,
                max.value.labels = 5)
officersByAgency <- aggregate(df[c(16, 19)],
                             list(v7 = df$v7),
                             FUN = sum)
officersByState <- aggregate(df[c(16, 19)],
                              list(v15 = df$v15),
                              FUN = sum)
officersKilledByMonth <- aggregate(df[c(74, 256, 438, 620, 802, 984, 1166,
                                   1348, 1530, 1712, 1894, 2076)],
                                   list(v15 = df$v15),
                                   FUN = sum)
# The next 12 data frames are assaults with injury - firearm, knife, other,
# hands / feet, total
officersAssaultedJan <- aggregate(df[c(76, 77, 78, 79, 80)],
                                      list(v15 = df$v15),
                                      FUN = sum)
officersAssaultedFeb <- aggregate(df[c(258, 259, 260, 261, 262)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedMar <- aggregate(df[c(440, 441, 442, 443, 444)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedApr <- aggregate(df[c(622, 623, 624, 625, 626)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedMay <- aggregate(df[c(804, 805, 806, 807, 808)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedJun <- aggregate(df[c(986, 987, 988, 989, 990)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedJul <- aggregate(df[c(1168, 1169, 1170, 1171, 1172)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedAug <- aggregate(df[c(1350, 1351, 1352, 1353, 1354)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedSept <- aggregate(df[c(1532, 1533, 1534, 1535, 1536)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedOct <- aggregate(df[c(1714, 1715, 1716, 1717, 1718)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedNov <- aggregate(df[c(1896, 1897, 1898, 1899, 1900)],
                                  list(v15 = df$v15),
                                  FUN = sum)
officersAssaultedDec <- aggregate(df[c(2078, 2079, 2080, 2081, 2082)],
                                  list(v15 = df$v15),
                                  FUN = sum)
names(officersAssaultedJan) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedFeb) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedMar) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedApr) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedMay) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedJun) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedJul) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedAug) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedSept) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedOct) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedNov) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersAssaultedDec) <- c("StateName09028", "InjFirearms", "InjKnife",
                                 "InjOther", "InjBody", "InjTotal")
names(officersKilledByMonth) <- c("stateNm", "Jan", "Feb", "Mar", "Apr", "May",
                                  "Jun", "Jul", "Aug", "Sept", "Oct", "Nov",
                                  "Dec")
officersAssaulted <- rbind(officersAssaultedJan, officersAssaultedFeb,
                           officersAssaultedMar, officersAssaultedApr,
                           officersAssaultedMay, officersAssaultedJun,
                           officersAssaultedJul, officersAssaultedAug,
                           officersAssaultedSept, officersAssaultedOct,
                           officersAssaultedNov, officersAssaultedDec)
officersAssaultedTotal <- aggregate(officersAssaulted[c(2, 3, 4, 5, 6)],
                                    list(StateName09028 = officersAssaulted$StateName09028),
                                    FUN = sum)
#write.csv(officersAssaultedTotal, "P32 All assaults with injury.csv",
#          row.names = FALSE)
# above work needs a little tweaking
# not using officers killed data
officersKilledByMonth$total <- rowSums(officersKilledByMonth[, 2:13])
assaultedInjuryByMonth <- aggregate(df[c(80, 262, 444, 626, 808, 990, 1172,
                                         1354, 1536, 1718, 1900, 2082)],
                                    list(v15 = df$v15),
                                    FUN = sum)
names(assaultedInjuryByMonth) <- c("stateNm", "Jan", "Feb", "Mar", "Apr", "May",
                                  "Jun", "Jul", "Aug", "Sept", "Oct", "Nov",
                                  "Dec")
assaultedInjuryByMonth$total <- rowSums(assaultedInjuryByMonth[, 2:13])
assaultedNoInjuryByMonth <- aggregate(df[c(85, 267, 449, 631, 813, 995, 1177,
                                         1359, 1541, 1723, 1905, 2087)],
                                    list(v15 = df$v15),
                                    FUN = sum)
names(assaultedNoInjuryByMonth) <- c("stateNm", "Jan", "Feb", "Mar", "Apr", "May",
                                   "Jun", "Jul", "Aug", "Sept", "Oct", "Nov",
                                   "Dec")
assaultedNoInjuryByMonth$total <- rowSums(assaultedNoInjuryByMonth[, 2:13])
assaultedNoInjuryByMonth$stateNm <- as.character(assaultedNoInjuryByMonth$stateNm)
#write.csv(assaultedNoInjuryByMonth, "P32 All assaults no injury.csv",
#          row.names = FALSE)
#
# Start exploring the data frame from the .sav file input. The code below
# is exploratory analysis, some plots, building interim data frames that 
# eventually build a summary file used in the hypothesis testing and map /
# leaflet / Shiny code.
#
# This code way overcounts the number of officers as compared to LEOKA report,
# will have to explore further. Are the different months reported across
# different rows? No, one row, one agency year's worth of data.
# Does LEOKA report agencies that are "not reported"? For certain months, or
# the whole year? Or if v70 = Yes, ...   column v56 Not updated, no Police ...
nbrOfficers <- aggregate(df[c(16, 19)],
                         list(v15 = df$v15),
                         FUN = sum)
nbrOfficersByStateAgency <- aggregate(df[c(16, 19)],
                         list(v15 = df$v15, v14 = df$v14),
                         FUN = sum)
# Find the rows that have "Not reported" in one of the months and build an
# interim dataframe excluding those rows and redo officer counts
pattern1 <- "^Not reported$"
ndxJan <- grep(pattern1, df$v57)
ndxFeb <- grep(pattern1, df$v58)
ndxMar <- grep(pattern1, df$v59)
ndxApr <- grep(pattern1, df$v60)
ndxMay <- grep(pattern1, df$v61)
ndxJun <- grep(pattern1, df$v62)
ndxJul <- grep(pattern1, df$v63)
ndxAug <- grep(pattern1, df$v64)
ndxSept <- grep(pattern1, df$v65)
ndxOct <- grep(pattern1, df$v66)
ndxNov <- grep(pattern1, df$v67)
ndxDec <- grep(pattern1, df$v68)
ndxYear <- c(ndxJan, ndxFeb, ndxMar, ndxApr, ndxMay, ndxJun, ndxJul, ndxAug,
             ndxSept, ndxOct, ndxNov, ndxDec)
# There are 8776 rows that have at least one month "Not reported"
ndxYear <- unique(ndxYear)
# There are 10142 rows that have all months reported in one fashion or another
df1 <- df[-ndxYear, ]
# The number of officers is much closer to the LEOKA report, but some states
# like SC and RI are way off. What about column 70? At this point, come to
# the conclusion that the LEOKA report was created with different data that's
# available on the ICPSR site. The ICPSR data has been updated since the 
# LEOKA report was published
nbrOfficers1 <- aggregate(df1[c(16, 19)],
                         list(v15 = df1$v15),
                         FUN = sum)
# column 70 above has <NA>, "No, ..." and "Yes, ..."
# Next 3 statements explore the data. I've found that there are agencies
# that report officers but have [cover] zero population. Should those agency
# figures count toward total figures? In DC's case, one of the agencies was
# excluded because of not reporting assault data, the other agency
# reported zero assaults and 18 officers. Thinking further, covering zero
# population doesn't matter in my hypothesis. I have full year figures for
# assaults and officer numbers, that's what counts
dfDC <- df1[df1$v15 == "D C   ",]
df1DC <- dfDC[, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631, 808,
                  813, 990, 995, 1172, 1177, 1354, 1359, 1536, 1541, 1718,
                  1723, 1900, 1905, 2082, 2087)]
#write.csv(df1DC, "1997 DC LEOKA numbers.csv", row.names = FALSE)
dfIA <- df1[df1$v15 == "IOWA  ",]
df1IA <- dfIA[, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631, 808,
                  813, 990, 995, 1172, 1177, 1354, 1359, 1536, 1541, 1718,
                  1723, 1900, 1905, 2082, 2087)]
#write.csv(df1IA, "1997 IA LEOKA numbers.csv", row.names = FALSE)
# MT is an interesting case. There are zero assaults once all filters are
# applied. But, this excludes a partial year report for GREAT FALLS, where
# there's been 8 assaults. Jan - Jul was "Not reported". Go with the filters -
# counting a partial year report does "tilt" the numbers - assaults are
# potentially underreported. Supplemental material I've found indicate
# FL, VT, NH, FL, IL, DC, and KS did not report. However, I've found data
# for DC so I will include DC in my figures, as well as MT with zero assaults.
# NH has assault data as well, so my original data set and the supplmental
# material I've found have been created with different data (point in time?)
# Some assault figures agree exactly with suppl. data, so that's been a
# reassuring check
dfMT <- df1[df1$v15 == "MONT  ",]
df1MT <- dfMT[, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631, 808,
                  813, 990, 995, 1172, 1177, 1354, 1359, 1536, 1541, 1718,
                  1723, 1900, 1905, 2082, 2087)]
#write.csv(df1MT, "1997 MT LEOKA numbers.csv", row.names = FALSE)
#dfTX <- df1[df1$v15 == "TEXAS ",]
#df1TX <- dfTX[, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631, 808,
#                  813, 990, 995, 1172, 1177, 1354, 1359, 1536, 1541, 1718,
#                  1723, 1900, 1905, 2082, 2087)]
#write.csv(df1TX, "1997 TX LEOKA numbers.csv", row.names = FALSE)
dfSC <- df1[df1$v15 == "S C   ",]
df1SC <- dfSC[, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631, 808,
                  813, 990, 995, 1172, 1177, 1354, 1359, 1536, 1541, 1718,
                  1723, 1900, 1905, 2082, 2087)]
df1SC$TotalAssaults <- df1SC$v80 + df1SC$v85 + df1SC$v262 + df1SC$v267 +
  df1SC$v444 + df1SC$v449 + df1SC$v626 + df1SC$v631 + df1SC$v808 +
  df1SC$v813 + df1SC$v990 + df1SC$v995 + df1SC$v1172 + df1SC$v1177 + 
  df1SC$v1354 + df1SC$v1359 + df1SC$v1536 + df1SC$v1541 + df1SC$v1718 +
  df1SC$v1723 + df1SC$v1900 + df1SC$v1905 + df1SC$v2082 + df1SC$v2087
df1SC$NbrOfficers <- df1SC$v16 + df1SC$v19
#write.csv(df1SC, "1997 SC LEOKA numbers.csv", row.names = FALSE)
# OK, after reviewing SC numbers, there are agencies that are reporting
# assault data but no officers. Need to exclude those rows. Somehow ...
# V56 "Not updated, no Police Employee data", and can live with values
# "Contains data from previous year" as it is reasonable to assume officer
# numbers grow or shrink in small increments
pattern2 <- "^Not updated, no Police Employee data$"
dfRI <- df1[df1$v15 == "R I   ",]
ndxNoEmp <- grep(pattern2, dfRI$v56)
dfRI <- dfRI[-ndxNoEmp, ]
df1RI <- dfRI[, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631, 808,
                  813, 990, 995, 1172, 1177, 1354, 1359, 1536, 1541, 1718,
                  1723, 1900, 1905, 2082, 2087)]
df1RI$TotalAssaults <- df1RI$v80 + df1RI$v85 + df1RI$v262 + df1RI$v267 +
  df1RI$v444 + df1RI$v449 + df1RI$v626 + df1RI$v631 + df1RI$v808 +
  df1RI$v813 + df1RI$v990 + df1RI$v995 + df1RI$v1172 + df1RI$v1177 + 
  df1RI$v1354 + df1RI$v1359 + df1RI$v1536 + df1RI$v1541 + df1RI$v1718 +
  df1RI$v1723 + df1RI$v1900 + df1RI$v1905 + df1RI$v2082 + df1RI$v2087
df1RI$NbrOfficers <- df1RI$v16 + df1RI$v19
#write.csv(df1RI, "1997 RI LEOKA numbers.csv", row.names = FALSE)
# column 55 - values with "City list" are of interest
pattern3 <- "City"
#pattern3 <- "^For all reports$"
ndxCityList <- grep(pattern3, df1$v55)
# Above regex indicates there are no "City list ..." rows. All df1 entries
# have "For all reports".
#
# As mentioned previously, what to do about column 56 - has values of 
# - Police Employee data has been updated
# - Not updated, no Police Employee data
# In the case of the IA data set, these two agencies had zero assaults. So
# no big deal. But, need to exclude those rows w/o police employee data in
# final tabulation
#
rptNoData <- "Reported, no data"
df2 <- df1[df1$v57 == rptNoData &
           df1$v58 == rptNoData &
           df1$v59 == rptNoData &
           df1$v60 == rptNoData &
           df1$v61 == rptNoData &
           df1$v62 == rptNoData &
           df1$v63 == rptNoData &
           df1$v64 == rptNoData &
           df1$v65 == rptNoData &
           df1$v66 == rptNoData &
           df1$v67 == rptNoData &
           df1$v68 == rptNoData, ]
# Above code deletes too much - 4371 observations. But for SC, find it hard
# to believe that Charleston, Columbia, Spartanburg, Charleston, Richland County
# would have zero assaults - or "Reported, no data". That's 43% of
# the data that's in df1 (removing agencies that did not report for part or 
# all year). So let's ditch this part of the code.
#
# So, for the rest of this project, take the original data set, use
# agencies that only reported full year (df1), and reported police
# employee data
#
# find (grep) rows that did not report police officer data
ndx1NoEmp <- grep(pattern2, df1$v56)
# 13 through 24 are population, state, geographical info, and officer counts
# 55 through 71 are mostly month indicators, Jan - Dec, whether data was
# reported.
# The alternating columns, starting with 80, 85 are assaults with injury,
# then assaults no injury. 24 total, January through December
df3 <- df1[-ndx1NoEmp, c(13:24, 55:71, 80, 85, 262, 267, 444, 449, 626, 631,
                         808, 813, 990, 995, 1172, 1177, 1354, 1359, 1536,
                         1541, 1718, 1723, 1900, 1905, 2082, 2087)]
df3$TotalAssaults <- df3$v80 + df3$v85 + df3$v262 + df3$v267 +
  df3$v444 + df3$v449 + df3$v626 + df3$v631 + df3$v808 +
  df3$v813 + df3$v990 + df3$v995 + df3$v1172 + df3$v1177 + 
  df3$v1354 + df3$v1359 + df3$v1536 + df3$v1541 + df3$v1718 +
  df3$v1723 + df3$v1900 + df3$v1905 + df3$v2082 + df3$v2087
df3$NbrOfficers <- df3$v16 + df3$v19
patternNYC <- "^NEW YORK                "
patternLA <- "^LOS ANGELES.*$"   # This will pick up LA County and City College
patternHouston <- "^HOUSTON.*$"
#patternChicago <- "^CHICAGO.*$" # No data for Chicago, have to go to #11 city
patternPhiladelphia <- "^PHILADELPHIA.*$"
patternPhoenix <- "^PHOENIX.*$"
patternSanDiego <- "^SAN DIEGO.*$"
patternDallas <- "^DALLAS.*$"
patternSanAntonio <- "^SAN ANTONIO.*$"
patternDetroit <- "^DETROIT.*$"
patternSanJose <- "^SAN JOSE.*$"
# For the statements below, if county data is picked up, get rid of it. only
# want to report city-level
indxNY <- grep(patternNYC, df3$v14)
dfNYC <- df3[indxNY,]
dfNYC$Lat <- 40.7128
dfNYC$Lon <- -74.0059
indxLA <- grep(patternLA, df3$v14)
dfLA <- df3[indxLA, ]
# This will include the city and county of LA
dfLA <- dfLA[dfLA$v13 > 1500000, ]
dfLA$Lat <- 34.0522
dfLA$Lon <- -118.2437
indxHouston <- grep(patternHouston, df3$v14)
dfHouston <- df3[indxHouston, ]
# Have to add population to the statement because there was a "smaller"
# Houston that is definitely not the 4th largest city in the U.S.
dfHouston <- dfHouston[dfHouston$v15 == "TEXAS " & dfHouston$v13 > 1000000, ]
dfHouston$Lat <- 29.7604
dfHouston$Lon <- -95.3698
#indxChicago <- grep(patternChicago, df3$v14)
#dfChicago <- df3[indxChicago, ]
#dfChicago <- dfChicago[dfChicago$v15 == "ILL   " & dfChicago$v13 > 1000000, ]
indxPhiladelphia <- grep(patternPhiladelphia, df3$v14)
dfPhiladelphia <- df3[indxPhiladelphia, ]
dfPhiladelphia$Lat <- 39.9526
dfPhiladelphia$Lon <- -75.1652
indxPhoenix <- grep(patternPhoenix, df3$v14)
dfPhoenix <- df3[indxPhoenix, ]
dfPhoenix <- dfPhoenix[dfPhoenix$v15 == "ARIZ  " & dfPhoenix$v13 > 1000000, ]
dfPhoenix$Lat <- 33.4484
dfPhoenix$Lon <- -112.0740
indxSanDiego <- grep(patternSanDiego, df3$v14)
dfSanDiego <- df3[indxSanDiego, ]
dfSanDiego <- dfSanDiego[dfSanDiego$v15 == "CALIF " & dfSanDiego$v13 > 1000000, ]
dfSanDiego$Lat <- 32.7157
dfSanDiego$Lon <- -117.1611
indxDallas <- grep(patternDallas, df3$v14)
dfDallas <- df3[indxDallas, ]
dfDallas <- dfDallas[dfDallas$v15 == "TEXAS " & dfDallas$v13 > 1000000, ]
dfDallas$Lat <- 32.7767
dfDallas$Lon <- -96.7970
indxSanAntonio <- grep(patternSanAntonio, df3$v14)
dfSanAntonio <- df3[indxSanAntonio, ]
dfSanAntonio$Lat <- 29.4241
dfSanAntonio$Lon <- -98.4936
indxDetroit <- grep(patternDetroit, df3$v14)
dfDetroit <- df3[indxDetroit, ]
dfDetroit <- dfDetroit[dfDetroit$v15 == "MICH  " & dfDetroit$v13 > 1000000, ]
dfDetroit$Lat <- 42.3314
dfDetroit$Lon <- -83.0458
indxSanJose <- grep(patternSanJose, df3$v14)
dfSanJose <- df3[indxSanJose, ]
dfSanJose <- dfSanJose[dfSanJose$v15 == "CALIF " & dfSanJose$v13 > 840000, ]
dfSanJose$Lat <- 37.3382
dfSanJose$Lon <- -121.8863
citiesDF <- rbind(dfNYC, dfLA, dfHouston, dfPhiladelphia, dfPhoenix,
                  dfSanDiego, dfDallas, dfSanAntonio, dfDetroit, dfSanJose)
cityDF <- citiesDF[, c(2:3, 54:57)]
cityDF$FIPSCode[cityDF$v15 == "MICH  "] <- as.character("26")
cityDF$FIPSCode[cityDF$v15 == "N Y   "] <- "36"
cityDF$FIPSCode[cityDF$v15 == "PA    "] <- "42"
cityDF$FIPSCode[cityDF$v15 == "TEXAS "] <- "48"
cityDF$FIPSCode[cityDF$v15 == "ARIZ  "] <- "04"
cityDF$FIPSCode[cityDF$v15 == "CALIF "] <- "06"
cityDF$StateAbbr[cityDF$v15 == "MICH  "] <- as.character("MI")
cityDF$StateAbbr[cityDF$v15 == "N Y   "] <- "NY"
cityDF$StateAbbr[cityDF$v15 == "PA    "] <- "PA"
cityDF$StateAbbr[cityDF$v15 == "TEXAS "] <- "TX"
cityDF$StateAbbr[cityDF$v15 == "ARIZ  "] <- "AZ"
cityDF$StateAbbr[cityDF$v15 == "CALIF "] <- "CA"
write.csv(cityDF, "P3 top10 cities.csv", row.names = FALSE)
# Let's start building the final data frame which will be a csv as input to
# the map logic and logic below
df4 <- aggregate(df3[c('TotalAssaults', 'NbrOfficers')],
                 list(v15 = df3$v15),
                 FUN = sum)
names(df4) <- c("StateName09028", "OfficersAssaulted", "NbrOfOfficers")
df4$StateName09028 <- as.character(df4$StateName09028)
# Set the zero assaults to NA since we don't have assault data, except MT
df4$OfficersAssaulted[df4$OfficersAssaulted == 0 & !(df4$StateName09028 == "MONT  ")] <- NA
# Same goes for the number of officers, may be misleading in presentation
df4$NbrOfOfficers[is.na(df4$OfficersAssaulted) & !(df4$StateName09028 == "MONT  ")] <- NA
df4$FIPSCode[df4$StateName09028 == "ILL   "] <- "17"
df4$FIPSCode[df4$StateName09028 == "IND   "] <- "18"
df4$FIPSCode[df4$StateName09028 == "MICH  "] <- "26"
df4$FIPSCode[df4$StateName09028 == "OHIO  "] <- "39"
df4$FIPSCode[df4$StateName09028 == "WIS   "] <- "55"
df4$FIPSCode[df4$StateName09028 == "IOWA  "] <- "19"
df4$FIPSCode[df4$StateName09028 == "KANS  "] <- "20"
df4$FIPSCode[df4$StateName09028 == "MINN  "] <- "27"
df4$FIPSCode[df4$StateName09028 == "MO    "] <- "29"
df4$FIPSCode[df4$StateName09028 == "NEBR  "] <- "31"
df4$FIPSCode[df4$StateName09028 == "N DAK "] <- "38"
df4$FIPSCode[df4$StateName09028 == "S DAK "] <- "46"
df4$FIPSCode[df4$StateName09028 == "N J   "] <- "34"
df4$FIPSCode[df4$StateName09028 == "N Y   "] <- "36"
df4$FIPSCode[df4$StateName09028 == "PA    "] <- "42"
df4$FIPSCode[df4$StateName09028 == "CONN  "] <- "09"
df4$FIPSCode[df4$StateName09028 == "MAINE "] <- "23"
df4$FIPSCode[df4$StateName09028 == "MASS  "] <- "25"
df4$FIPSCode[df4$StateName09028 == "N H   "] <- "33"
df4$FIPSCode[df4$StateName09028 == "VT    "] <- "50"
df4$FIPSCode[df4$StateName09028 == "R I   "] <- "44"
df4$FIPSCode[df4$StateName09028 == "ALA   "] <- "01"
df4$FIPSCode[df4$StateName09028 == "KY    "] <- "21"
df4$FIPSCode[df4$StateName09028 == "MISS  "] <- "28"
df4$FIPSCode[df4$StateName09028 == "TENN  "] <- "47"
df4$FIPSCode[df4$StateName09028 == "DEL   "] <- "10"
df4$FIPSCode[df4$StateName09028 == "D C   "] <- "11"
df4$FIPSCode[df4$StateName09028 == "FLA   "] <- "12"
df4$FIPSCode[df4$StateName09028 == "GA    "] <- "13"
df4$FIPSCode[df4$StateName09028 == "MD    "] <- "24"
df4$FIPSCode[df4$StateName09028 == "N C   "] <- "37"
df4$FIPSCode[df4$StateName09028 == "S C   "] <- "45"
df4$FIPSCode[df4$StateName09028 == "VA    "] <- "51"
df4$FIPSCode[df4$StateName09028 == "W VA  "] <- "54"
df4$FIPSCode[df4$StateName09028 == "ARK   "] <- "05"
df4$FIPSCode[df4$StateName09028 == "LA    "] <- "22"
df4$FIPSCode[df4$StateName09028 == "OKLA  "] <- "40"
df4$FIPSCode[df4$StateName09028 == "TEXAS "] <- "48"
df4$FIPSCode[df4$StateName09028 == "ARIZ  "] <- "04"
df4$FIPSCode[df4$StateName09028 == "COLO  "] <- "08"
df4$FIPSCode[df4$StateName09028 == "IDAHO "] <- "16"
df4$FIPSCode[df4$StateName09028 == "MONT  "] <- "30"
df4$FIPSCode[df4$StateName09028 == "NEV   "] <- "32"
df4$FIPSCode[df4$StateName09028 == "N MEX "] <- "35"
df4$FIPSCode[df4$StateName09028 == "UTAH  "] <- "49"
df4$FIPSCode[df4$StateName09028 == "WYO   "] <- "56"
df4$FIPSCode[df4$StateName09028 == "ALASKA"] <- "02"
df4$FIPSCode[df4$StateName09028 == "CALIF "] <- "06"
df4$FIPSCode[df4$StateName09028 == "HAWAII"] <- "15"
df4$FIPSCode[df4$StateName09028 == "OREG  "] <- "41"
df4$FIPSCode[df4$StateName09028 == "WASH  "] <- "53"
df4$State[df4$StateName09028 == "ILL   "] <- as.character("IL")
df4$State[df4$StateName09028 == "IND   "] <- "IN"
df4$State[df4$StateName09028 == "MICH  "] <- "MI"
df4$State[df4$StateName09028 == "OHIO  "] <- "OH"
df4$State[df4$StateName09028 == "WIS   "] <- "WI"
df4$State[df4$StateName09028 == "IOWA  "] <- "IA"
df4$State[df4$StateName09028 == "KANS  "] <- "KS"
df4$State[df4$StateName09028 == "MINN  "] <- "MN"
df4$State[df4$StateName09028 == "MO    "] <- "MO"
df4$State[df4$StateName09028 == "NEBR  "] <- "NE"
df4$State[df4$StateName09028 == "N DAK "] <- "ND"
df4$State[df4$StateName09028 == "S DAK "] <- "SD"
df4$State[df4$StateName09028 == "N J   "] <- "NJ"
df4$State[df4$StateName09028 == "N Y   "] <- "NY"
df4$State[df4$StateName09028 == "PA    "] <- "PA"
df4$State[df4$StateName09028 == "CONN  "] <- "CT"
df4$State[df4$StateName09028 == "MAINE "] <- "ME"
df4$State[df4$StateName09028 == "MASS  "] <- "MA"
df4$State[df4$StateName09028 == "N H   "] <- "NH"
df4$State[df4$StateName09028 == "VT    "] <- "VT"
df4$State[df4$StateName09028 == "R I   "] <- "RI"
df4$State[df4$StateName09028 == "ALA   "] <- "AL"
df4$State[df4$StateName09028 == "KY    "] <- "KY"
df4$State[df4$StateName09028 == "MISS  "] <- "MS"
df4$State[df4$StateName09028 == "TENN  "] <- "TN"
df4$State[df4$StateName09028 == "DEL   "] <- "DE"
df4$State[df4$StateName09028 == "D C   "] <- "DC"
df4$State[df4$StateName09028 == "FLA   "] <- "FL"
df4$State[df4$StateName09028 == "GA    "] <- "GA"
df4$State[df4$StateName09028 == "MD    "] <- "MD"
df4$State[df4$StateName09028 == "N C   "] <- "NC"
df4$State[df4$StateName09028 == "S C   "] <- "SC"
df4$State[df4$StateName09028 == "VA    "] <- "VA"
df4$State[df4$StateName09028 == "W VA  "] <- "WV"
df4$State[df4$StateName09028 == "ARK   "] <- "AR"
df4$State[df4$StateName09028 == "LA    "] <- "LA"
df4$State[df4$StateName09028 == "OKLA  "] <- "OK"
df4$State[df4$StateName09028 == "TEXAS "] <- "TX"
df4$State[df4$StateName09028 == "ARIZ  "] <- "AZ"
df4$State[df4$StateName09028 == "COLO  "] <- "CO"
df4$State[df4$StateName09028 == "IDAHO "] <- "ID"
df4$State[df4$StateName09028 == "MONT  "] <- "MT"
df4$State[df4$StateName09028 == "NEV   "] <- "NV"
df4$State[df4$StateName09028 == "N MEX "] <- "NM"
df4$State[df4$StateName09028 == "UTAH  "] <- "UT"
df4$State[df4$StateName09028 == "WYO   "] <- "WY"
df4$State[df4$StateName09028 == "ALASKA"] <- "AK"
df4$State[df4$StateName09028 == "CALIF "] <- "CA"
df4$State[df4$StateName09028 == "HAWAII"] <- "HI"
df4$State[df4$StateName09028 == "OREG  "] <- "OR"
df4$State[df4$StateName09028 == "WASH  "] <- "WA"
df4$StateName[df4$StateName09028 == "ILL   "] <- as.character("ILLINOIS")
df4$StateName[df4$StateName09028 == "IND   "] <- "INDIANA"
df4$StateName[df4$StateName09028 == "MICH  "] <- "MICHIGAN"
df4$StateName[df4$StateName09028 == "OHIO  "] <- "OHIO"
df4$StateName[df4$StateName09028 == "WIS   "] <- "WISCONSIN"
df4$StateName[df4$StateName09028 == "IOWA  "] <- "IOWA"
df4$StateName[df4$StateName09028 == "KANS  "] <- "KANSAS"
df4$StateName[df4$StateName09028 == "MINN  "] <- "MINNESOTA"
df4$StateName[df4$StateName09028 == "MO    "] <- "MISSOURI"
df4$StateName[df4$StateName09028 == "NEBR  "] <- "NEBRASKA"
df4$StateName[df4$StateName09028 == "N DAK "] <- "NORTH DAKOTA"
df4$StateName[df4$StateName09028 == "S DAK "] <- "SOUTH DAKOTA"
df4$StateName[df4$StateName09028 == "N J   "] <- "NEW JERSEY"
df4$StateName[df4$StateName09028 == "N Y   "] <- "NEW YORK"
df4$StateName[df4$StateName09028 == "PA    "] <- "PENNSYLVANIA"
df4$StateName[df4$StateName09028 == "CONN  "] <- "CONNECTICUT"
df4$StateName[df4$StateName09028 == "MAINE "] <- "MAINE"
df4$StateName[df4$StateName09028 == "MASS  "] <- "MASSACHUSETTS"
df4$StateName[df4$StateName09028 == "N H   "] <- "NEW HAMPSHIRE"
df4$StateName[df4$StateName09028 == "VT    "] <- "VERMONT"
df4$StateName[df4$StateName09028 == "R I   "] <- "RHODE ISLAND"
df4$StateName[df4$StateName09028 == "ALA   "] <- "ALABAMA"
df4$StateName[df4$StateName09028 == "KY    "] <- "KENTUCKY"
df4$StateName[df4$StateName09028 == "MISS  "] <- "MISSISSIPPI"
df4$StateName[df4$StateName09028 == "TENN  "] <- "TENNESSEE"
df4$StateName[df4$StateName09028 == "DEL   "] <- "DELAWARE"
df4$StateName[df4$StateName09028 == "D C   "] <- "DISTRICT OF COLUMBIA"
df4$StateName[df4$StateName09028 == "FLA   "] <- "FLORIDA"
df4$StateName[df4$StateName09028 == "GA    "] <- "GEORGIA"
df4$StateName[df4$StateName09028 == "MD    "] <- "MARYLAND"
df4$StateName[df4$StateName09028 == "N C   "] <- "NORTH CAROLINA"
df4$StateName[df4$StateName09028 == "S C   "] <- "SOUTH CAROLINA"
df4$StateName[df4$StateName09028 == "VA    "] <- "VIRGINIA"
df4$StateName[df4$StateName09028 == "W VA  "] <- "WEST VIRGINIA"
df4$StateName[df4$StateName09028 == "ARK   "] <- "ARKANSAS"
df4$StateName[df4$StateName09028 == "LA    "] <- "LOUISIANA"
df4$StateName[df4$StateName09028 == "OKLA  "] <- "OKLAHOMA"
df4$StateName[df4$StateName09028 == "TEXAS "] <- "TEXAS"
df4$StateName[df4$StateName09028 == "ARIZ  "] <- "ARIZONA"
df4$StateName[df4$StateName09028 == "COLO  "] <- "COLORADO"
df4$StateName[df4$StateName09028 == "IDAHO "] <- "IDAHO"
df4$StateName[df4$StateName09028 == "MONT  "] <- "MONTANA"
df4$StateName[df4$StateName09028 == "NEV   "] <- "NEVADA"
df4$StateName[df4$StateName09028 == "N MEX "] <- "NEW MEXICO"
df4$StateName[df4$StateName09028 == "UTAH  "] <- "UTAH"
df4$StateName[df4$StateName09028 == "WYO   "] <- "WYOMING"
df4$StateName[df4$StateName09028 == "ALASKA"] <- "ALASKA"
df4$StateName[df4$StateName09028 == "CALIF "] <- "CALIFORNIA"
df4$StateName[df4$StateName09028 == "HAWAII"] <- "HAWAII"
df4$StateName[df4$StateName09028 == "OREG  "] <- "OREGON"
df4$StateName[df4$StateName09028 == "WASH  "] <- "WASHINGTON"
df4$StateAbbr <- df4$State
encFIPS <- c("17", "18", "26", "39", "55")
wncFIPS <- c("19", "20", "27", "29", "31", "38", "46")
maFIPS <- c("34", "36", "42")
neFIPS <- c("09", "23", "25", "33", "44", "50")
escFIPS <- c("01", "21", "28", "47")
saFIPS <- c("10", "11", "12", "13", "24", "37", "45", "51", "54")
wscFIPS <- c("05", "22", "40", "48")
mtnFIPS <- c("04", "08", "16", "30", "32", "35", "49", "56")
pacFIPS <- c("02", "06", "15", "41", "53")
df4$Division[df4$FIPSCode %in% encFIPS] <- "East North Central"
df4$Division[df4$FIPSCode %in% wncFIPS] <- "West North Central"
df4$Division[df4$FIPSCode %in% maFIPS] <- "Middle Atlantic"
df4$Division[df4$FIPSCode %in% neFIPS] <- "New England"
df4$Division[df4$FIPSCode %in% escFIPS] <- "East South Central"
df4$Division[df4$FIPSCode %in% saFIPS] <- "South Atlantic"
df4$Division[df4$FIPSCode %in% wscFIPS] <- "West South Central"
df4$Division[df4$FIPSCode %in% mtnFIPS] <- "Mountain"
df4$Division[df4$FIPSCode %in% pacFIPS] <- "Pacific"
df4$Region[df4$FIPSCode %in% midwestFIPS] <- "Midwest"
df4$Region[df4$FIPSCode %in% northeastFIPS] <- "Northeast"
df4$Region[df4$FIPSCode %in% southFIPS] <- "South"
df4$Region[df4$FIPSCode %in% westFIPS] <- "West"
df4$RegionAssaults[df4$FIPSCode %in% midwestFIPS] <- sum(df4[df4$FIPSCode %in% midwestFIPS,
                                                             "OfficersAssaulted"],
                                                         na.rm = TRUE)
df4$RegionAssaults[df4$FIPSCode %in% northeastFIPS] <- sum(df4[df4$FIPSCode %in% northeastFIPS,
                                                             "OfficersAssaulted"],
                                                         na.rm = TRUE)
df4$RegionAssaults[df4$FIPSCode %in% southFIPS] <- sum(df4[df4$FIPSCode %in% southFIPS,
                                                             "OfficersAssaulted"],
                                                         na.rm = TRUE)
df4$RegionAssaults[df4$FIPSCode %in% westFIPS] <- sum(df4[df4$FIPSCode %in% westFIPS,
                                                             "OfficersAssaulted"],
                                                         na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% encFIPS] <- sum(df4[df4$FIPSCode %in% encFIPS,
                                                               "OfficersAssaulted"],
                                                           na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% wncFIPS] <- sum(df4[df4$FIPSCode %in% wncFIPS,
                                                           "OfficersAssaulted"],
                                                       na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% maFIPS] <- sum(df4[df4$FIPSCode %in% maFIPS,
                                                           "OfficersAssaulted"],
                                                       na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% neFIPS] <- sum(df4[df4$FIPSCode %in% neFIPS,
                                                           "OfficersAssaulted"],
                                                       na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% escFIPS] <- sum(df4[df4$FIPSCode %in% escFIPS,
                                                          "OfficersAssaulted"],
                                                      na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% saFIPS] <- sum(df4[df4$FIPSCode %in% saFIPS,
                                                          "OfficersAssaulted"],
                                                      na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% wscFIPS] <- sum(df4[df4$FIPSCode %in% wscFIPS,
                                                          "OfficersAssaulted"],
                                                      na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% mtnFIPS] <- sum(df4[df4$FIPSCode %in% mtnFIPS,
                                                          "OfficersAssaulted"],
                                                      na.rm = TRUE)
df4$DivisionAssaults[df4$FIPSCode %in% pacFIPS] <- sum(df4[df4$FIPSCode %in% pacFIPS,
                                                           "OfficersAssaulted"],
                                                       na.rm = TRUE)
df4$RegionOfficers[df4$FIPSCode %in% midwestFIPS] <- sum(df4[df4$FIPSCode %in% midwestFIPS,
                                                             "NbrOfOfficers"],
                                                         na.rm = TRUE)
df4$RegionOfficers[df4$FIPSCode %in% northeastFIPS] <- sum(df4[df4$FIPSCode %in% northeastFIPS,
                                                               "NbrOfOfficers"],
                                                           na.rm = TRUE)
df4$RegionOfficers[df4$FIPSCode %in% southFIPS] <- sum(df4[df4$FIPSCode %in% southFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
df4$RegionOfficers[df4$FIPSCode %in% westFIPS] <- sum(df4[df4$FIPSCode %in% westFIPS,
                                                          "NbrOfOfficers"],
                                                      na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% encFIPS] <- sum(df4[df4$FIPSCode %in% encFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% wncFIPS] <- sum(df4[df4$FIPSCode %in% wncFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% maFIPS] <- sum(df4[df4$FIPSCode %in% maFIPS,
                                                          "NbrOfOfficers"],
                                                      na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% neFIPS] <- sum(df4[df4$FIPSCode %in% neFIPS,
                                                          "NbrOfOfficers"],
                                                      na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% escFIPS] <- sum(df4[df4$FIPSCode %in% escFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% saFIPS] <- sum(df4[df4$FIPSCode %in% saFIPS,
                                                          "NbrOfOfficers"],
                                                      na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% wscFIPS] <- sum(df4[df4$FIPSCode %in% wscFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% mtnFIPS] <- sum(df4[df4$FIPSCode %in% mtnFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
df4$DivisionOfficers[df4$FIPSCode %in% pacFIPS] <- sum(df4[df4$FIPSCode %in% pacFIPS,
                                                           "NbrOfOfficers"],
                                                       na.rm = TRUE)
# Add AS GU MP PR VI
# Have to add these 5 rows because they're in the US state shapefile
asRow <- c("", NA, NA, "60", "AS", "AMERICAN SAMOA", "AS", "", "", NA, NA, NA, NA)
guRow <- c("", NA, NA, "66", "GU", "GUAM", "GU", "", "", NA, NA, NA, NA)
mpRow <- c("", NA, NA, "69", "MP", "MARIANA ISLANDS", "MP", "", "", NA, NA, NA, NA)
prRow <- c("", NA, NA, "72", "PR", "PUERTO RICO", "PR", "", "", NA, NA, NA, NA)
viRow <- c("", NA, NA, "78", "VI", "VIRGIN ISLANDS", "VI", "", "", NA, NA, NA, NA)
df4 <- rbind.data.frame(df4, asRow, guRow, mpRow, prRow, viRow)
df4$OfficersAssaulted <- as.numeric(df4$OfficersAssaulted)
df4$NbrOfOfficers <- as.numeric(df4$NbrOfOfficers)
df4$RegionAssaults <- as.numeric(df4$RegionAssaults)
df4$DivisionAssaults <- as.numeric(df4$DivisionAssaults)
df4$RegionOfficers <- as.numeric(df4$RegionOfficers)
df4$DivisionOfficers <- as.numeric(df4$DivisionOfficers)
write.csv(df4, "P3 state abbr orig.csv", row.names = FALSE)
#
set.seed(1234)
# These FIPS codes below indicate the states that have data for 1997
midwestFIPS1 <- c("18", "19", "26", "27", "29", "31", "38", "39", "46", "55")
northeastFIPS1 <- c("09", "23", "25", "33", "34", "36", "42", "44")
southFIPS1 <- c("01", "05", "10", "11", "13", "21", "22", "24", "28",
                "37", "40", "45", "47", "48", "51", "54")
westFIPS1 <- c("02", "04", "06", "08", "15", "16", "30", "32", "35", "41",
               "49", "53", "56")
#
# Read back in the data set that was just written as a .csv and perform
# some testing and EDA
# This file that is being read in below is actually created earlier in the
# program through interim data frames then finally put together starting 
# around line 287, and used in subsequent R files - including server.R
# with Shiny
#stateColClasses <- c(rep("character", 7), rep("integer", 7), rep("numeric", 2))
stateColClasses <- c("character", rep("integer", 2), rep("character", 6),
                     rep("integer", 4))
#stateAbbrNbrs <- read.csv("P3 state abbr.csv", stringsAsFactors = FALSE,
stateAbbrNbrs <- read.csv("P3 state abbr orig.csv", stringsAsFactors = FALSE,
                          colClasses = stateColClasses)
stateAbbrNbrs$officers100 <- stateAbbrNbrs$NbrOfOfficers / 100
stateAbbrNbrs$officerAssault100 <- round((stateAbbrNbrs$OfficersAssaulted /
                                            stateAbbrNbrs$officers100), 1)
stateAbbrNbrs$regOfficers100 <- stateAbbrNbrs$RegionOfficers / 100
stateAbbrNbrs$RegionAssaultsPer100 <- round((stateAbbrNbrs$RegionAssaults /
                                               stateAbbrNbrs$regOfficers100), 1)
stateAbbrNbrs$divOfficers100 <- stateAbbrNbrs$DivisionOfficers / 100
stateAbbrNbrs$DivisionAssaultsPer100 <- round((stateAbbrNbrs$DivisionAssaults /
                                                 stateAbbrNbrs$divOfficers100), 1)
stateAbbrNbrsNoNA <- stateAbbrNbrs[!is.na(stateAbbrNbrs$OfficersAssaulted), ]
# could do this in a compound OR statement but doing it in 2
stateAbbrNbrsNoNA <- stateAbbrNbrsNoNA[!is.na(stateAbbrNbrsNoNA$NbrOfOfficers), ]
assaultsBar <- ggplot(data = stateAbbrNbrsNoNA, aes(x = reorder(StateAbbr, -officerAssault100),
                                                    y = officerAssault100))
#pdf("P32 Officers Assaulted by State.pdf", width = 6, height = 2.76)
assaultsBar + geom_bar(stat = "identity") +
  ggtitle("Officers Assaulted by State") +
  xlab("State") +
  ylab("Assaults per 100 Officers") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey40"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank())
#dev.off()
#
kStatesToTrain <- 4
midwestTrainFIPS <- sample(midwestFIPS1, size = kStatesToTrain, replace = F)
northeastTrainFIPS <- sample(northeastFIPS1, size = kStatesToTrain, replace = F)
southTrainFIPS <- sample(southFIPS1, size = kStatesToTrain, replace = F)
westTrainFIPS <- sample(westFIPS1, size = kStatesToTrain, replace = F)
trainFIPS <- c(midwestTrainFIPS, northeastTrainFIPS, southTrainFIPS, westTrainFIPS)
midwestTestFIPS <- midwestFIPS1[!midwestFIPS1 %in% midwestTrainFIPS]
northeastTestFIPS <- northeastFIPS1[!northeastFIPS1 %in% northeastTrainFIPS]
southTestFIPS <- southFIPS1[!southFIPS1 %in% southTrainFIPS]
westTestFIPS <- westFIPS1[!westFIPS1 %in% westTrainFIPS]
testFIPS <- c(midwestTestFIPS, northeastTestFIPS, southTestFIPS, westTestFIPS)
noNAFIPS <- c(midwestFIPS1, northeastFIPS1, southFIPS1, westFIPS1)
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTrainFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTrainFIPS,
                         "officerAssault100"])
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTrainFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTrainFIPS,
                         "officerAssault100"])
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTrainFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTrainFIPS,
                         "officerAssault100"])
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTrainFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTrainFIPS,
                         "officerAssault100"])
# Above qqnorm & qqline indicate data not normally distributed
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTestFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTestFIPS,
                         "officerAssault100"])
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTestFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTestFIPS,
                         "officerAssault100"])
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTestFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTestFIPS,
                         "officerAssault100"])
qqnorm(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTestFIPS,
                         "officerAssault100"])
qqline(stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTestFIPS,
                         "officerAssault100"])
# Above qqnorm & qqline indicate data not normally distributed
# t test between
# midwest-northeast # midwest-south # midwest-west
# northeast-south # northeast-west
# south-west
# But first an F test to see if any difference in means exists
# 04/09/2017 and just did a little more research ... ANOVA requires normal
# distribution so have to run t test
#aov.train <- aov(officerAssault100 ~ Region,
#                 data = stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in%
#                                            trainFIPS, ])
#summary(aov.train)
# No hypothesis regarding division
#aov.train1 <- aov(officerAssault100 ~ Division,
#                  data = stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in%
#                                           trainFIPS, ])
#summary(aov.train1)
# t test for Train data
xTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTrainFIPS,
                            "officerAssault100"]
yTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTrainFIPS,
                            "officerAssault100"]
midwestNortheastTr <- t.test(xTrain, y = yTrain)
# p = 0.9953
midwestNortheastTr
xTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTrainFIPS,
                            "officerAssault100"]
yTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTrainFIPS,
                            "officerAssault100"]
midwestSouthTr <- t.test(xTrain, y = yTrain)
# p = 0.4242
midwestSouthTr
xTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTrainFIPS,
                            "officerAssault100"]
yTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTrainFIPS,
                            "officerAssault100"]
midwestWestTr <- t.test(xTrain, y = yTrain)
# p = 0.8898
midwestWestTr
xTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTrainFIPS,
                            "officerAssault100"]
yTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTrainFIPS,
                            "officerAssault100"]
northeastSouthTr <- t.test(xTrain, y = yTrain)
# p = 0.3579
northeastSouthTr
xTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTrainFIPS,
                            "officerAssault100"]
yTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTrainFIPS,
                            "officerAssault100"]
northeastWestTr <- t.test(xTrain, y = yTrain)
# p = 0.8686
northeastWestTr
xTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTrainFIPS,
                            "officerAssault100"]
yTrain <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTrainFIPS,
                            "officerAssault100"]
southWestTr <- t.test(xTrain, y = yTrain)
# p = 0.5627
southWestTr
# now t test the test data
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTestFIPS,
                            "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTestFIPS,
                            "officerAssault100"]
midwestNortheastT <- t.test(xTest, y = yTest)
# p = 0.1577
midwestNortheastT
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTestFIPS,
                            "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTestFIPS,
                            "officerAssault100"]
midwestSouthT <- t.test(xTest, y = yTest)
# p = 0.2224
midwestSouthT
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestTestFIPS,
                            "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTestFIPS,
                            "officerAssault100"]
midwestWestT <- t.test(xTest, y = yTest)
# p = 0.1505
midwestWestT
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTestFIPS,
                            "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTestFIPS,
                            "officerAssault100"]
northeastSouthT <- t.test(xTest, y = yTest)
# p = 0.3294
northeastSouthT
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastTestFIPS,
                            "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTestFIPS,
                            "officerAssault100"]
northeastWestT <- t.test(xTest, y = yTest)
# p = 0.4337
northeastWestT
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% southTestFIPS,
                            "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% westTestFIPS,
                            "officerAssault100"]
southWestT <- t.test(xTest, y = yTest)
# p = 0.6318
southWestT
# t test indicates no difference in means between groups in the test samples
# just for laughs, the next few lines of code ...
# Look at all of the data - the midwest and northeast have the largest
# separation in means. This is not part of the results reported.
xTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% midwestFIPS1,
                           "officerAssault100"]
yTest <- stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in% northeastFIPS1,
                           "officerAssault100"]
midNET <- t.test(xTest, y = yTest)
# p = 0.113
midNET
regionPalette <- c("#175887", "#5BB9FD", "#4F738B", "#1A9FFF")
#                   "#1987D2", "#B4DFFE")
#pdf("P32 Assaults Against Officers box.pdf", width = 6, height = 2.76)
ggplot(data = stateAbbrNbrsNoNA, aes(Region, officerAssault100, fill = Region)) +
  geom_boxplot() +
  ggtitle("Assaults Against Officers By Region") +
  ylab("Assaults per 100 Officers") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey40"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 8)) +
#  scale_x_discrete(limits = stateAbbrNbrsNoNA[stateAbbrNbrsNoNA$FIPSCode %in%
#                                                trainFIPS, "Region"]) +
  scale_fill_manual(values = regionPalette)
#dev.off()
ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% midwestFIPS1, ],
       aes(x = reorder(StateAbbr, -officerAssault100),
           y = officerAssault100)) +
  geom_bar(stat = "identity", fill = "#738B59") +
  ggtitle("Officers Assaulted by State - Midwest") +
  xlab("State") +
  ylab("Assaults per 100 Officers") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey40"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank())
ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% northeastFIPS1, ],
       aes(x = reorder(StateAbbr, -officerAssault100),
           y = officerAssault100)) +
  geom_bar(stat = "identity", fill = "#6E7F63") +
  ggtitle("Officers Assaulted by State - Northeast") +
  xlab("State") +
  ylab("Assaults per 100 Officers") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey40"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank())
ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% southFIPS1, ],
       aes(x = reorder(StateAbbr, -officerAssault100),
           y = officerAssault100)) +
  geom_bar(stat = "identity", fill = "#656577") +
  ggtitle("Officers Assaulted by State - South") +
  xlab("State") +
  ylab("Assaults per 100 Officers") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey40"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank())
ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% westFIPS1, ],
       aes(x = reorder(StateAbbr, -officerAssault100),
           y = officerAssault100)) +
  geom_bar(stat = "identity", fill = "#605981") +
  ggtitle("Officers Assaulted by State - West") +
  xlab("State") +
  ylab("Assaults per 100 Officers") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(color = "grey60"),
        panel.grid.minor = element_line(color = "grey40"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank())