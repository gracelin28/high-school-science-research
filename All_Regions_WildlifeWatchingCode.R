install.packages('magrittr')
library(magrittr)

install.packages("dplyr")
library(dplyr)

library(readr)

setwd('/Users/hongluo/Documents/grace/rawdata')
ww <- readLines('fh4.txt')

wt <- vector()
for (record in ww) {
  wt <- append(wt, substr(record, 1994, 2005))
}
wt <- as.numeric(wt)

from <- vector()
for (record in ww) {
  from <- append(from, substr(record, 1851, 1851))
}

income <- vector()
for (record in ww) {
  income <- append(income, substr(record, 1846, 1847))
}

race <- vector()
for (record in ww) {
  race <- append(race, substr(record, 1856, 1856))
}

popden <- vector()
for (record in ww) {
  popden <- append(popden, substr(record, 1855, 1855))
}

helpwild <- vector()
for (record in ww) {
  helpwild <- append(helpwild, substr(record, 454, 454))
}

personalinfo <- data.frame(wt, from, income, race, popden, helpwild)

personalinfo



trips <- matrix(0, nrow=4018, ncol=9)

for (i in seq_along(ww)) {
  r <- vector("integer" , 9)
  r[1] <- as.numeric(substr(ww[i], 650, 651))
  r[2] <- as.numeric(substr(ww[i], 652, 653))
  r[3] <- as.numeric(substr(ww[i], 654, 655))
  r[4] <- as.numeric(substr(ww[i], 656, 657))
  r[5] <- as.numeric(substr(ww[i], 658, 659))
  r[6] <- as.numeric(substr(ww[i], 660, 661))
  r[7] <- as.numeric(substr(ww[i], 662, 663))
  r[8] <- as.numeric(substr(ww[i], 664, 665))
  r[9] <- as.numeric(substr(ww[i], 666, 667))
  t <- vector("numeric" , 9)
  t[1] <- as.numeric(substr(ww[i], 668, 671))
  t[2] <- as.numeric(substr(ww[i], 672, 675))
  t[3] <- as.numeric(substr(ww[i], 676, 679))
  t[4] <- as.numeric(substr(ww[i], 680, 683))
  t[5] <- as.numeric(substr(ww[i], 684, 687))
  t[6] <- as.numeric(substr(ww[i], 688, 691))
  t[7] <- as.numeric(substr(ww[i], 692, 695))
  t[8] <- as.numeric(substr(ww[i], 696, 699))
  t[9] <- as.numeric(substr(ww[i], 700, 703))
  for (j in 1:9) {
    if (is.na(t[j])) {
      t[j] <- 0
    }
    trips[i, r[j]] <- trips[i, r[j]] + t[j]
  }
}

trips <- data.frame(trips)
colnames(trips) <- c("New_England_trips", "Middle_Atlantic_trips", "East_North_Central_trips", "West_North_Central_trips", "South_Atlantic_trips", "East_South_Central_trips", "West_South_Central_trips", "Mountain_trips", "Pacific_trips")

trips



days <- matrix(0, nrow=4018, ncol=9)

for (i in seq_along(ww)) {
  r <- vector("integer" , 9)
  r[1] <- as.numeric(substr(ww[i], 650, 651))
  r[2] <- as.numeric(substr(ww[i], 652, 653))
  r[3] <- as.numeric(substr(ww[i], 654, 655))
  r[4] <- as.numeric(substr(ww[i], 656, 657))
  r[5] <- as.numeric(substr(ww[i], 658, 659))
  r[6] <- as.numeric(substr(ww[i], 660, 661))
  r[7] <- as.numeric(substr(ww[i], 662, 663))
  r[8] <- as.numeric(substr(ww[i], 664, 665))
  r[9] <- as.numeric(substr(ww[i], 666, 667))
  d <- vector("numeric" , 9)
  d[1] <- as.numeric(substr(ww[i], 704, 706))
  d[2] <- as.numeric(substr(ww[i], 707, 709))
  d[3] <- as.numeric(substr(ww[i], 710, 712))
  d[4] <- as.numeric(substr(ww[i], 713, 715))
  d[5] <- as.numeric(substr(ww[i], 716, 718))
  d[6] <- as.numeric(substr(ww[i], 719, 721))
  d[7] <- as.numeric(substr(ww[i], 722, 724))
  d[8] <- as.numeric(substr(ww[i], 725, 727))
  d[9] <- as.numeric(substr(ww[i], 728, 730))
  for (j in 1:9) {
    if (is.na(d[j])) {
      d[j] <- 0
    }
    days[i, r[j]] <- days[i, r[j]] + d[j]
  }
}

days <- data.frame(days)
colnames(days) <- c("New_England_days", "Middle_Atlantic_days", "East_North_Central_days", "West_North_Central_days", "South_Atlantic_days", "East_South_Central_days", "West_South_Central_days", "Mountain_days", "Pacific_days")

days



expenditures <- matrix(0, nrow=4018, ncol=9)

for (i in seq_along(ww)) {
  r <- vector("integer" , 9)
  exp1 <- vector("numeric" , 9)
  exp2 <- vector("numeric" , 9)
  exp3 <- vector("numeric" , 9)
  exp4 <- vector("numeric" , 9)
  exp5 <- vector("numeric" , 9)
  exp6 <- vector("numeric" , 9)
  exp7 <- vector("numeric" , 9)
  exp8 <- vector("numeric" , 9)
  exp9 <- vector("numeric" , 9)
  exp10 <- vector("numeric" , 9)
  exp11 <- vector("numeric" , 9)
  exp12 <- vector("numeric" , 9)
  
  for (j in 1:9) {
    r[j] <- as.numeric(substr(ww[i], 648 + 2*j, 649 + 2*j))
    
    exp1[j] <- as.numeric(substr(ww[i], (994 + 7*j), (1000 + 7*j)))
    exp2[j] <- as.numeric(substr(ww[i], (1057 + 7*j), (1063 + 7*j)))
    exp3[j] <- as.numeric(substr(ww[i], (1120 + 7*j), (1126 + 7*j)))
    exp4[j] <- as.numeric(substr(ww[i], (1183 + 7*j), (1189 + 7*j)))
    exp5[j] <- as.numeric(substr(ww[i], (1246 + 7*j), (1252 + 7*j)))
    exp6[j] <- as.numeric(substr(ww[i], (1309 + 7*j), (1315 + 7*j)))
    exp7[j] <- as.numeric(substr(ww[i], (1372 + 7*j), (1378 + 7*j)))
    exp8[j] <- as.numeric(substr(ww[i], (1435 + 7*j), (1441 + 7*j)))
    exp9[j] <- as.numeric(substr(ww[i], (1498 + 7*j), (1504 + 7*j)))
    exp10[j] <- as.numeric(substr(ww[i], (1561 + 7*j), (1567 + 7*j)))
    exp11[j] <- as.numeric(substr(ww[i], (1624 + 7*j), (1630 + 7*j)))
    exp12[j] <- as.numeric(substr(ww[i], (1687 + 7*j), (1693 + 7*j)))
    
    if (is.na(exp1[j])) {
      exp1[j] <- 0
    }
    if (is.na(exp2[j])) {
      exp2[j] <- 0
    }
    if (is.na(exp3[j])) {
      exp3[j] <- 0
    }
    if (is.na(exp4[j])) {
      exp4[j] <- 0
    }
    if (is.na(exp5[j])) {
      exp5[j] <- 0
    }
    if (is.na(exp6[j])) {
      exp6[j] <- 0
    }
    if (is.na(exp7[j])) {
      exp7[j] <- 0
    }
    if (is.na(exp8[j])) {
      exp8[j] <- 0
    }
    if (is.na(exp9[j])) {
      exp9[j] <- 0
    }
    if (is.na(exp10[j])) {
      exp10[j] <- 0
    }
    if (is.na(exp11[j])) {
      exp11[j] <- 0
    }
    if (is.na(exp12[j])) {
      exp12[j] <- 0
    }
    expenditures[i, r[j]] <- expenditures[i, r[j]] + exp1[j] + exp2[j] + exp3[j] + exp4[j] + exp5[j] + exp6[j] + exp7[j] + exp8[j] + exp9[j] + exp10[j] + exp11[j] + exp12[j]
  }
}

expenditures <- data.frame(expenditures)
colnames(expenditures) <- c("New_England_expenditures", "Middle_Atlantic_expenditures", "East_North_Central_expenditures", "West_North_Central_expenditures", "South_Atlantic_expenditures", "East_South_Central_expenditures", "West_South_Central_expenditures", "Mountain_expenditures", "Pacific_expenditures")

expenditures



equipment <- vector()

for (record in ww) {
  a <- vector(mode="numeric", length=36)
  a[1] <- as.numeric(substr(record, 152, 155))
  a[2] <- as.numeric(substr(record, 461, 464))
  a[3] <- as.numeric(substr(record, 200, 204))
  a[4] <- as.numeric(substr(record, 521, 525))
  a[5] <- as.numeric(substr(record, 156, 159))
  a[6] <- as.numeric(substr(record, 465, 468))
  a[7] <- as.numeric(substr(record, 160, 163))
  a[8] <- as.numeric(substr(record, 469, 472))
  a[9] <- as.numeric(substr(record, 164, 167))
  a[10] <- as.numeric(substr(record, 473, 476))
  a[11] <- as.numeric(substr(record, 168, 171))
  a[12] <- as.numeric(substr(record, 477, 480))
  a[13] <- as.numeric(substr(record, 172, 175))
  a[14] <- as.numeric(substr(record, 481, 484))
  a[15] <- as.numeric(substr(record, 176, 179))
  a[16] <- as.numeric(substr(record, 485, 488))
  a[17] <- as.numeric(substr(record, 180, 183))
  a[18] <- as.numeric(substr(record, 489, 492))
  a[19] <- as.numeric(substr(record, 184, 187))
  a[20] <- as.numeric(substr(record, 493, 496))
  a[21] <- as.numeric(substr(record, 188, 191))
  a[22] <- as.numeric(substr(record, 497, 500))
  a[23] <- as.numeric(substr(record, 192, 195))
  a[24] <- as.numeric(substr(record, 501, 504))
  a[25] <- as.numeric(substr(record, 196, 199))
  a[26] <- as.numeric(substr(record, 505, 508))
  a[27] <- as.numeric(substr(record, 210, 215))
  a[28] <- as.numeric(substr(record, 536, 541))
  a[29] <- as.numeric(substr(record, 205, 209))
  a[30] <- as.numeric(substr(record, 526, 530))
  a[31] <- as.numeric(substr(record, 566, 571))
  a[32] <- as.numeric(substr(record, 572, 577))
  a[33] <- as.numeric(substr(record, 592, 598))
  a[34] <- as.numeric(substr(record, 542, 547))
  a[35] <- as.numeric(substr(record, 599, 605))
  a[36] <- as.numeric(substr(record, 560, 565))
  #  a[37] <- as.numeric(substr(record, 585, 591))
  #  a[38] <- as.numeric(substr(record, 578, 584))
  
  for (i in 1:36) {
    if (is.na(a[i])) {
      a[i] <- 0
    }
  }
  
  equipment <- append(equipment, sum(a))
}


equipments <- matrix(0, nrow=4018, ncol=9)

for (j in 1:9) {
  equipments[,j] <- (days[,j]/rowSums(days))*equipment
}
equipments[is.nan(equipments)] <- 0
equipments <- data.frame(equipments)
colnames(equipments) <- c("New_England_equipments", "Middle_Atlantic_equipments", "East_North_Central_equipments", "West_North_Central_equipments", "South_Atlantic_equipments", "East_South_Central_equipments", "West_South_Central_equipments", "Mountain_equipments", "Pacific_equipments")

equipments



wildlifewatching <- cbind(personalinfo, trips, days, expenditures, equipments)

wildlifewatching


yearlywage <- 46640.94


#_NEW_ENGLAND

neww <- wildlifewatching %>%
  select(New_England_trips, wt, New_England_days, New_England_expenditures, New_England_equipments, from, income, race, popden, helpwild) %>%
  filter(New_England_trips>0) %>%
  mutate(New_England_cost = (((New_England_days * yearlywage)/365) + New_England_expenditures + New_England_equipments)/New_England_trips) %>%
  mutate(onsitewt = (wt*New_England_trips))

neww

totaltrips <- sum(neww$New_England_trips*neww$wt)

ne <- glm(New_England_trips ~ New_England_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=neww, weights=wt)

newwconsumersurplus <- (-1/(coef(summary(ne))[2,1]))*(totaltrips)

newwconsumersurplus

#_MIDDLE_ATLANTIC

maww <- wildlifewatching %>%
  select(Middle_Atlantic_trips, wt, Middle_Atlantic_days, Middle_Atlantic_expenditures, Middle_Atlantic_equipments, from, income, race, popden, helpwild) %>%
  filter(Middle_Atlantic_trips>0) %>%
  mutate(Middle_Atlantic_cost = (((Middle_Atlantic_days * yearlywage)/365) + Middle_Atlantic_expenditures + Middle_Atlantic_equipments)/Middle_Atlantic_trips) %>%
  mutate(onsitewt = (wt*Middle_Atlantic_trips))

maww

totaltrips <- sum(maww$Middle_Atlantic_trips*maww$wt)

ma <- glm(Middle_Atlantic_trips ~ Middle_Atlantic_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=maww, weights=wt)

mawwconsumersurplus <- (-1/(coef(summary(ma))[2,1]))*(totaltrips)

mawwconsumersurplus

#_EAST_NORTH_CENTRAL

encww <- wildlifewatching %>%
  select(East_North_Central_trips, wt, East_North_Central_days, East_North_Central_expenditures, East_North_Central_equipments, from, income, race, popden, helpwild) %>%
  filter(East_North_Central_trips>0) %>%
  mutate(East_North_Central_cost = (((East_North_Central_days * yearlywage)/365) + East_North_Central_expenditures + East_North_Central_equipments)/East_North_Central_trips) %>%
  mutate(onsitewt = (wt*East_North_Central_trips))

encww

totaltrips <- sum(encww$East_North_Central_trips*encww$wt)

enc <- glm(East_North_Central_trips ~ East_North_Central_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=encww, weights=wt)

encwwconsumersurplus <- (-1/(coef(summary(enc))[2,1]))*(totaltrips)

encwwconsumersurplus

#_WEST_NORTH_CENTRAL

wncww <- wildlifewatching %>%
  select(West_North_Central_trips, wt, West_North_Central_days, West_North_Central_expenditures, West_North_Central_equipments, from, income, race, popden, helpwild) %>%
  filter(West_North_Central_trips>0) %>%
  mutate(West_North_Central_cost = (((West_North_Central_days * yearlywage)/365) + West_North_Central_expenditures + West_North_Central_equipments)/West_North_Central_trips) %>%
  mutate(onsitewt = (wt*West_North_Central_trips))

wncww

totaltrips <- sum(wncww$West_North_Central_trips*wncww$wt)

wnc <- glm(West_North_Central_trips ~ West_North_Central_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=wncww, weights=wt)

wncwwconsumersurplus <- (-1/(coef(summary(wnc))[2,1]))*(totaltrips)

wncwwconsumersurplus

#_SOUTH_ATLANTIC

saww <- wildlifewatching %>%
  select(South_Atlantic_trips, wt, South_Atlantic_days, South_Atlantic_expenditures, South_Atlantic_equipments, from, income, race, popden, helpwild) %>%
  filter(South_Atlantic_trips>0) %>%
  mutate(South_Atlantic_cost = (((South_Atlantic_days * yearlywage)/365) + South_Atlantic_expenditures + South_Atlantic_equipments)/South_Atlantic_trips) %>%
  mutate(onsitewt = (wt*South_Atlantic_trips))

saww

totaltrips <- sum(saww$South_Atlantic_trips*saww$wt)

sa <- glm(South_Atlantic_trips ~ South_Atlantic_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=saww, weights=wt)

sawwconsumersurplus <- (-1/(coef(summary(sa))[2,1]))*(totaltrips)

sawwconsumersurplus

#_EAST_SOUTH_CENTRAL

escww <- wildlifewatching %>%
  select(East_South_Central_trips, wt, East_South_Central_days, East_South_Central_expenditures, East_South_Central_equipments, from, income, race, popden, helpwild) %>%
  filter(East_South_Central_trips>0) %>%
  mutate(East_South_Central_cost = (((East_South_Central_days * yearlywage)/365) + East_South_Central_expenditures + East_South_Central_equipments)/East_South_Central_trips) %>%
  mutate(onsitewt = (wt*East_South_Central_trips))

escww

totaltrips <- sum(escww$East_South_Central_trips*escww$wt)

esc <- glm(East_South_Central_trips ~ East_South_Central_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=escww, weights=wt)

escwwconsumersurplus <- (-1/(coef(summary(esc))[2,1]))*(totaltrips)

escwwconsumersurplus

#_WEST_SOUTH_CENTRAL

wscww <- wildlifewatching %>%
  select(West_South_Central_trips, wt, West_South_Central_days, West_South_Central_expenditures, West_South_Central_equipments, from, income, race, popden, helpwild) %>%
  filter(West_South_Central_trips>0) %>%
  mutate(West_South_Central_cost = (((West_South_Central_days * yearlywage)/365) + West_South_Central_expenditures + West_South_Central_equipments)/West_South_Central_trips) %>%
  mutate(onsitewt = (wt*West_South_Central_trips))

wscww

totaltrips <- sum(wscww$West_South_Central_trips*wscww$wt)

wsc <- glm(West_South_Central_trips ~ West_South_Central_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=wscww, weights=wt)

wscwwconsumersurplus <- (-1/(coef(summary(wsc))[2,1]))*(totaltrips)

wscwwconsumersurplus

#_MOUNTAIN

mww <- wildlifewatching %>%
  select(Mountain_trips, wt, Mountain_days, Mountain_expenditures, Mountain_equipments, from, income, race, popden, helpwild) %>%
  filter(Mountain_trips>0) %>%
  mutate(Mountain_cost = (((Mountain_days * yearlywage)/365) + Mountain_expenditures + Mountain_equipments)/Mountain_trips) %>%
  mutate(onsitewt = (wt*Mountain_trips))

mww

totaltrips <- sum(mww$Mountain_trips*mww$wt)

m <- glm(Mountain_trips ~ Mountain_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=mww, weights=wt)

mwwconsumersurplus <- (-1/(coef(summary(m))[2,1]))*(totaltrips)

mwwconsumersurplus

#_PACIFIC

pww <- wildlifewatching %>%
  select(Pacific_trips, wt, Pacific_days, Pacific_expenditures, Pacific_equipments, from, income, race, popden, helpwild) %>%
  filter(Pacific_trips>0) %>%
  mutate(Pacific_cost = (((Pacific_days * yearlywage)/365) + Pacific_expenditures + Pacific_equipments)/Pacific_trips) %>%
  mutate(onsitewt = (wt*Pacific_trips))

pww

totaltrips <- sum(pww$Pacific_trips*pww$wt)

p <- glm(Pacific_trips ~ Pacific_cost + factor(from) + factor(helpwild) + factor(race) + factor(popden), family="poisson", data=pww, weights=wt)

pwwconsumersurplus <- (-1/(coef(summary(p))[2,1]))*(totaltrips)

pwwconsumersurplus


# FINAL

consumersurpluses <- c(newwconsumersurplus, mawwconsumersurplus, encwwconsumersurplus, wncwwconsumersurplus, sawwconsumersurplus, escwwconsumersurplus, wscwwconsumersurplus, mwwconsumersurplus, pwwconsumersurplus)
regions <- c("New_England", "Middle_Atlantic", "East_North_Central", "West_North_Central", "South_Atlantic", "East_South_Central", "West_South_Central", "Mountain", "Pacific")
finalconsumersurpluses <- data.frame(regions, consumersurpluses)


finalconsumersurpluses