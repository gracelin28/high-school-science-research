install.packages('magrittr')
library(magrittr)

install.packages("dplyr")
library(dplyr)

library(readr)

setwd('/Users/hongluo/Documents/grace/rawdata')
sp <- readLines('fh3.txt')

wt <- vector()
for (record in sp) {
  wt <- append(wt, substr(record, 6482, 6493))
}
wt <- as.numeric(wt)

from <- vector()
for (record in sp) {
  from <- append(from, substr(record, 5644, 5644))
}

income <- vector()
for (record in sp) {
  income <- append(income, substr(record, 5639, 5640))
}

sex <- vector()
for (record in sp) {
  sex <- append(sex, substr(record, 5636, 5636))
}

race <- vector()
for (record in sp) {
  race <- append(race, substr(record, 5648, 5648))
}

popden <- vector()
for (record in sp) {
  popden <- append(popden, substr(record, 5647, 5647))
}

fown <- vector()
for (record in sp) {
  fown <- append(fown, substr(record, 1217, 1217))
}

personalinfo <- data.frame(wt, from, income, sex, race, popden, fown)

personalinfo



trips <- matrix(0, nrow=3949, ncol=9)

for (i in seq_along(sp)) {
  r1 <- vector("integer" , 3)
  r1[1] <- as.numeric(substr(sp[i], 3527, 3528))
  r1[2] <- as.numeric(substr(sp[i], 3529, 3530))
  r1[3] <- as.numeric(substr(sp[i], 3531, 3532))
  t1 <- vector("numeric" , 3)
  t1[1] <- as.numeric(substr(sp[i], 3533, 3536))
  t1[2] <- as.numeric(substr(sp[i], 3537, 3540))
  t1[3] <- as.numeric(substr(sp[i], 3541, 3544))
  
  r2 <- vector("integer" , 4)
  r2[1] <- as.numeric(substr(sp[i], 3710, 3711))
  r2[2] <- as.numeric(substr(sp[i], 3712, 3713))
  r2[3] <- as.numeric(substr(sp[i], 3714, 3715))
  r2[4] <- as.numeric(substr(sp[i], 3716, 3717))
  t2 <- vector("numeric" , 4)
  t2[1] <- as.numeric(substr(sp[i], 3718, 3721))
  t2[2] <- as.numeric(substr(sp[i], 3722, 3725))
  t2[3] <- as.numeric(substr(sp[i], 3726, 3729))
  t2[4] <- as.numeric(substr(sp[i], 3730, 3733))
  
  r3 <- vector("integer" , 4)
  r3[1] <- as.numeric(substr(sp[i], 4018, 4019))
  r3[2] <- as.numeric(substr(sp[i], 4020, 4021))
  r3[3] <- as.numeric(substr(sp[i], 4022, 4023))
  r3[4] <- as.numeric(substr(sp[i], 4024, 4025))
  t3 <- vector("numeric" , 4)
  t3[1] <- as.numeric(substr(sp[i], 4026, 4029))
  t3[2] <- as.numeric(substr(sp[i], 4030, 4033))
  t3[3] <- as.numeric(substr(sp[i], 4034, 4037))
  t3[4] <- as.numeric(substr(sp[i], 4038, 4041))
  for (j in 1:3) {
    if (is.na(t1[j])) {
      t1[j] <- 0
    }
    trips[i, r1[j]] <- trips[i, r1[j]] + t1[j]
  }
  for (j in 1:4) {
    if (is.na(t2[j])) {
      t2[j] <- 0
    }
    if (is.na(t3[j])) {
      t3[j] <- 0
    }
    trips[i, r2[j]] <- trips[i, r2[j]] + t2[j]
    trips[i, r3[j]] <- trips[i, r3[j]] + t3[j]
  }
}

trips <- data.frame(trips)
colnames(trips) <- c("New_England_trips", "Middle_Atlantic_trips", "East_North_Central_trips", "West_North_Central_trips", "South_Atlantic_trips", "East_South_Central_trips", "West_South_Central_trips", "Mountain_trips", "Pacific_trips")

trips



days <- matrix(0, nrow=3949, ncol=9)

for (i in seq_along(sp)) {
  r <- vector("integer" , 4)
  r[1] <- as.numeric(substr(sp[i], 3463, 3464))
  r[2] <- as.numeric(substr(sp[i], 3465, 3466))
  r[3] <- as.numeric(substr(sp[i], 3467, 3468))
  r[4] <- as.numeric(substr(sp[i], 3469, 3470))
  d <- vector("numeric" , 4)
  d[1] <- as.numeric(substr(sp[i], 3471, 3473))
  d[2] <- as.numeric(substr(sp[i], 3474, 3476))
  d[3] <- as.numeric(substr(sp[i], 3477, 3479))
  d[4] <- as.numeric(substr(sp[i], 3480, 3482))
  for (j in 1:4) {
    if (is.na(d[j])) {
      d[j] <- 0
    }
    days[i, r[j]] <- days[i, r[j]] + d[j]
  }
}

days <- data.frame(days)
colnames(days) <- c("New_England_days", "Middle_Atlantic_days", "East_North_Central_days", "West_North_Central_days", "South_Atlantic_days", "East_South_Central_days", "West_South_Central_days", "Mountain_days", "Pacific_days")

days



expenditures <- matrix(0, nrow=3949, ncol=9)

for (i in seq_along(sp)) {
  r1 <- vector("integer" , 3)
  r2 <- vector("integer" , 4)
  r3 <- vector("integer" , 4)
  
  exp1_1 <- vector("numeric" , 3)
  exp1_2 <- vector("numeric" , 3)
  exp1_3 <- vector("numeric" , 3)
  exp1_4 <- vector("numeric" , 3)
  exp1_5 <- vector("numeric" , 3)
  exp1_6 <- vector("numeric" , 3)
  exp1_7 <- vector("numeric" , 3)
  exp1_8 <- vector("numeric" , 3)
  exp1_9 <- vector("numeric" , 3)
  exp1_10 <- vector("numeric" , 3)
  exp1_11 <- vector("numeric" , 3)
  exp1_12 <- vector("numeric" , 3)
  exp1_13 <- vector("numeric" , 3)
  exp1_14 <- vector("numeric" , 3)
  exp1_15 <- vector("numeric" , 3)
  
  exp2_1 <- vector("numeric" , 4)
  exp2_2 <- vector("numeric" , 4)
  exp2_3 <- vector("numeric" , 4)
  exp2_4 <- vector("numeric" , 4)
  exp2_5 <- vector("numeric" , 4)
  exp2_6 <- vector("numeric" , 4)
  exp2_7 <- vector("numeric" , 4)
  exp2_8 <- vector("numeric" , 4)
  exp2_9 <- vector("numeric" , 4)
  exp2_10 <- vector("numeric" , 4)
  exp2_11 <- vector("numeric" , 4)
  exp2_12 <- vector("numeric" , 4)
  exp2_13 <- vector("numeric" , 4)
  exp2_14 <- vector("numeric" , 4)
  exp2_15 <- vector("numeric" , 4)
  
  exp3_1 <- vector("numeric" , 4)
  exp3_2 <- vector("numeric" , 4)
  exp3_3 <- vector("numeric" , 4)
  exp3_4 <- vector("numeric" , 4)
  exp3_5 <- vector("numeric" , 4)
  exp3_6 <- vector("numeric" , 4)
  exp3_7 <- vector("numeric" , 4)
  exp3_8 <- vector("numeric" , 4)
  exp3_9 <- vector("numeric" , 4)
  exp3_10 <- vector("numeric" , 4)
  exp3_11 <- vector("numeric" , 4)
  exp3_12 <- vector("numeric" , 4)
  exp3_13 <- vector("numeric" , 4)
  exp3_14 <- vector("numeric" , 4)
  exp3_15 <- vector("numeric" , 4)
  
  for (j in 1:3) {
    r1[j] <- as.numeric(substr(sp[i], 3525 + 2*j, 3526 + 2*j))
    
    exp1_1[j] <- as.numeric(substr(sp[i], (4359 + 7*j), (4365 + 7*j)))
    exp1_2[j] <- as.numeric(substr(sp[i], (4380 + 7*j), (4386 + 7*j)))
    exp1_3[j] <- as.numeric(substr(sp[i], (4401 + 7*j), (4407 + 7*j)))
    exp1_4[j] <- as.numeric(substr(sp[i], (4422 + 7*j), (4428 + 7*j)))
    exp1_5[j] <- as.numeric(substr(sp[i], (4443 + 7*j), (4449 + 7*j)))
    exp1_6[j] <- as.numeric(substr(sp[i], (4464 + 7*j), (4470 + 7*j)))
    exp1_7[j] <- as.numeric(substr(sp[i], (4485 + 7*j), (4491 + 7*j)))
    exp1_8[j] <- as.numeric(substr(sp[i], (4506 + 7*j), (4512 + 7*j)))
    exp1_9[j] <- as.numeric(substr(sp[i], (4527 + 7*j), (4533 + 7*j)))
    exp1_10[j] <- as.numeric(substr(sp[i], (4548 + 7*j), (4554 + 7*j)))
    exp1_11[j] <- as.numeric(substr(sp[i], (4569 + 7*j), (4575 + 7*j)))
    exp1_12[j] <- as.numeric(substr(sp[i], (4590 + 7*j), (4596 + 7*j)))
    exp1_13[j] <- as.numeric(substr(sp[i], (4611 + 7*j), (4617 + 7*j)))
    exp1_14[j] <- as.numeric(substr(sp[i], (4632 + 7*j), (4638 + 7*j)))
    exp1_15[j] <- as.numeric(substr(sp[i], (4653 + 7*j), (4659 + 7*j)))
    
    if (is.na(exp1_1[j])) {
      exp1_1[j] <- 0
    }
    if (is.na(exp1_2[j])) {
      exp1_2[j] <- 0
    }
    if (is.na(exp1_3[j])) {
      exp1_3[j] <- 0
    }
    if (is.na(exp1_4[j])) {
      exp1_4[j] <- 0
    }
    if (is.na(exp1_5[j])) {
      exp1_5[j] <- 0
    }
    if (is.na(exp1_6[j])) {
      exp1_6[j] <- 0
    }
    if (is.na(exp1_7[j])) {
      exp1_7[j] <- 0
    }
    if (is.na(exp1_8[j])) {
      exp1_8[j] <- 0
    }
    if (is.na(exp1_9[j])) {
      exp1_9[j] <- 0
    }
    if (is.na(exp1_10[j])) {
      exp1_10[j] <- 0
    }
    if (is.na(exp1_11[j])) {
      exp1_11[j] <- 0
    }
    if (is.na(exp1_12[j])) {
      exp1_12[j] <- 0
    }
    if (is.na(exp1_13[j])) {
      exp1_13[j] <- 0
    }
    if (is.na(exp1_14[j])) {
      exp1_14[j] <- 0
    }
    if (is.na(exp1_15[j])) {
      exp1_15[j] <- 0
    }
    
    expenditures[i, r1[j]] <- expenditures[i, r1[j]] + exp1_1[j] + exp1_2[j] + exp1_3[j] + exp1_4[j] + exp1_5[j] + exp1_6[j] + exp1_7[j] + exp1_8[j] + exp1_9[j] + exp1_10[j] + exp1_11[j] + exp1_12[j] + exp1_13[j] + exp1_14[j] + exp1_15[j]
  }
  
  for (j in 1:4) {
    r2[j] <- as.numeric(substr(sp[i], 3708 + 2*j, 3709 + 2*j))
    r3[j] <- as.numeric(substr(sp[i], 4016 + 2*j, 4017 + 2*j))
    
    exp2_1[j] <- as.numeric(substr(sp[i], (4698 + 7*j), (4704 + 7*j)))
    exp2_2[j] <- as.numeric(substr(sp[i], (4726 + 7*j), (4732 + 7*j)))
    exp2_3[j] <- as.numeric(substr(sp[i], (4754 + 7*j), (4760 + 7*j)))
    exp2_4[j] <- as.numeric(substr(sp[i], (4782 + 7*j), (4788 + 7*j)))
    exp2_5[j] <- as.numeric(substr(sp[i], (4810 + 7*j), (4816 + 7*j)))
    exp2_6[j] <- as.numeric(substr(sp[i], (4838 + 7*j), (4844 + 7*j)))
    exp2_7[j] <- as.numeric(substr(sp[i], (4866 + 7*j), (4872 + 7*j)))
    exp2_8[j] <- as.numeric(substr(sp[i], (4894 + 7*j), (4900 + 7*j)))
    exp2_9[j] <- as.numeric(substr(sp[i], (4922 + 7*j), (4928 + 7*j)))
    exp2_10[j] <- as.numeric(substr(sp[i], (4950 + 7*j), (4956 + 7*j)))
    exp2_11[j] <- as.numeric(substr(sp[i], (4978 + 7*j), (4984 + 7*j)))
    exp2_12[j] <- as.numeric(substr(sp[i], (5006 + 7*j), (5012 + 7*j)))
    exp2_13[j] <- as.numeric(substr(sp[i], (5034 + 7*j), (5040 + 7*j)))
    exp2_14[j] <- as.numeric(substr(sp[i], (5062 + 7*j), (5068 + 7*j)))
    exp2_15[j] <- as.numeric(substr(sp[i], (5090 + 7*j), (5096 + 7*j)))
    
    exp3_1[j] <- as.numeric(substr(sp[i], (5150 + 7*j), (5156 + 7*j)))
    exp3_2[j] <- as.numeric(substr(sp[i], (5178 + 7*j), (5184 + 7*j)))
    exp3_3[j] <- as.numeric(substr(sp[i], (5206 + 7*j), (5212 + 7*j)))
    exp3_4[j] <- as.numeric(substr(sp[i], (5234 + 7*j), (5240 + 7*j)))
    exp3_5[j] <- as.numeric(substr(sp[i], (5262 + 7*j), (5268 + 7*j)))
    exp3_6[j] <- as.numeric(substr(sp[i], (5290 + 7*j), (5296 + 7*j)))
    exp3_7[j] <- as.numeric(substr(sp[i], (5318 + 7*j), (5324 + 7*j)))
    exp3_8[j] <- as.numeric(substr(sp[i], (5346 + 7*j), (5352 + 7*j)))
    exp3_9[j] <- as.numeric(substr(sp[i], (5374 + 7*j), (5380 + 7*j)))
    exp3_10[j] <- as.numeric(substr(sp[i], (5402 + 7*j), (5408 + 7*j)))
    exp3_11[j] <- as.numeric(substr(sp[i], (5430 + 7*j), (5436 + 7*j)))
    exp3_12[j] <- as.numeric(substr(sp[i], (5458 + 7*j), (5464 + 7*j)))
    exp3_13[j] <- as.numeric(substr(sp[i], (5486 + 7*j), (5492 + 7*j)))
    exp3_14[j] <- as.numeric(substr(sp[i], (5514 + 7*j), (5520 + 7*j)))
    exp3_15[j] <- as.numeric(substr(sp[i], (5542 + 7*j), (5548 + 7*j)))
    
    if (is.na(exp2_1[j])) {
      exp2_1[j] <- 0
    }
    if (is.na(exp2_2[j])) {
      exp2_2[j] <- 0
    }
    if (is.na(exp2_3[j])) {
      exp2_3[j] <- 0
    }
    if (is.na(exp2_4[j])) {
      exp2_4[j] <- 0
    }
    if (is.na(exp2_5[j])) {
      exp2_5[j] <- 0
    }
    if (is.na(exp2_6[j])) {
      exp2_6[j] <- 0
    }
    if (is.na(exp2_7[j])) {
      exp2_7[j] <- 0
    }
    if (is.na(exp2_8[j])) {
      exp2_8[j] <- 0
    }
    if (is.na(exp2_9[j])) {
      exp2_9[j] <- 0
    }
    if (is.na(exp2_10[j])) {
      exp2_10[j] <- 0
    }
    if (is.na(exp2_11[j])) {
      exp2_11[j] <- 0
    }
    if (is.na(exp2_12[j])) {
      exp2_12[j] <- 0
    }
    if (is.na(exp2_13[j])) {
      exp2_13[j] <- 0
    }
    if (is.na(exp2_14[j])) {
      exp2_14[j] <- 0
    }
    if (is.na(exp2_15[j])) {
      exp2_15[j] <- 0
    }
    
    if (is.na(exp3_1[j])) {
      exp3_1[j] <- 0
    }
    if (is.na(exp3_2[j])) {
      exp3_2[j] <- 0
    }
    if (is.na(exp3_3[j])) {
      exp3_3[j] <- 0
    }
    if (is.na(exp3_4[j])) {
      exp3_4[j] <- 0
    }
    if (is.na(exp3_5[j])) {
      exp3_5[j] <- 0
    }
    if (is.na(exp3_6[j])) {
      exp3_6[j] <- 0
    }
    if (is.na(exp3_7[j])) {
      exp3_7[j] <- 0
    }
    if (is.na(exp3_8[j])) {
      exp3_8[j] <- 0
    }
    if (is.na(exp3_9[j])) {
      exp3_9[j] <- 0
    }
    if (is.na(exp3_10[j])) {
      exp3_10[j] <- 0
    }
    if (is.na(exp3_11[j])) {
      exp3_11[j] <- 0
    }
    if (is.na(exp3_12[j])) {
      exp3_12[j] <- 0
    }
    if (is.na(exp3_13[j])) {
      exp3_13[j] <- 0
    }
    if (is.na(exp3_14[j])) {
      exp3_14[j] <- 0
    }
    if (is.na(exp3_15[j])) {
      exp3_15[j] <- 0
    }
    
    expenditures[i, r2[j]] <- expenditures[i, r2[j]] + exp2_1[j] + exp2_2[j] + exp2_3[j] + exp2_4[j] + exp2_5[j] + exp2_6[j] + exp2_7[j] + exp2_8[j] + exp2_9[j] + exp2_10[j] + exp2_11[j] + exp2_12[j] + exp2_13[j] + exp2_14[j] + exp2_15[j]
    expenditures[i, r3[j]] <- expenditures[i, r3[j]] + exp3_1[j] + exp3_2[j] + exp3_3[j] + exp3_4[j] + exp3_5[j] + exp3_6[j] + exp3_7[j] + exp3_8[j] + exp3_9[j] + exp3_10[j] + exp3_11[j] + exp3_12[j] + exp3_13[j] + exp3_14[j] + exp3_15[j]
  }
}

expenditures <- data.frame(expenditures)
colnames(expenditures) <- c("New_England_expenditures", "Middle_Atlantic_expenditures", "East_North_Central_expenditures", "West_North_Central_expenditures", "South_Atlantic_expenditures", "East_South_Central_expenditures", "West_South_Central_expenditures", "Mountain_expenditures", "Pacific_expenditures")

expenditures



equipment <- vector()

for (record in sp) {
  a <- vector(mode="numeric", length=44)
  b <- vector(mode="numeric", length=44)
  a[1] <- as.numeric(substr(record, 407, 411))
  a[2] <- as.numeric(substr(record, 1398, 1402))
  a[3] <- as.numeric(substr(record, 324, 327))
  a[4] <- as.numeric(substr(record, 1271, 1274))
  a[5] <- as.numeric(substr(record, 328, 331))
  a[6] <- as.numeric(substr(record, 1275, 1278))
  a[7] <- as.numeric(substr(record, 332, 335))
  a[8] <- as.numeric(substr(record, 1279, 1282))
  a[9] <- as.numeric(substr(record, 336, 339))
  a[10] <- as.numeric(substr(record, 1283, 1286))
  a[11] <- as.numeric(substr(record, 340, 343))
  a[12] <- as.numeric(substr(record, 1287, 1290))
  a[13] <- as.numeric(substr(record, 344, 347))
  a[14] <- as.numeric(substr(record, 1291, 1294))
  a[15] <- as.numeric(substr(record, 412, 416))
  a[16] <- as.numeric(substr(record, 1403, 1407))
  a[17] <- as.numeric(substr(record, 417, 421))
  a[18] <- as.numeric(substr(record, 1408, 1412))
  a[19] <- as.numeric(substr(record, 422, 426))
  a[20] <- as.numeric(substr(record, 1413, 1417))
  
  a[21] <- 0
  a[22] <- 0
  
  b[23] <- as.numeric(substr(record, 302, 302))
  b[24] <- as.numeric(substr(record, 1185, 1185))
  a[23] <- as.numeric(substr(record, 427, 431))
  a[24] <- as.numeric(substr(record, 1418, 1422))
  
  b[25] <- as.numeric(substr(record, 305, 305))
  b[26] <- as.numeric(substr(record, 1188, 1188))
  a[25] <- as.numeric(substr(record, 432, 436))
  a[26] <- as.numeric(substr(record, 1423, 1427))
  
  b[27] <- as.numeric(substr(record, 308, 308))
  b[28] <- as.numeric(substr(record, 1191, 1191))
  a[27] <- as.numeric(substr(record, 437, 441))
  a[28] <- as.numeric(substr(record, 1428, 1432))
  
  b[29] <- as.numeric(substr(record, 311, 311))
  b[30] <- as.numeric(substr(record, 1194, 1194))
  a[29] <- as.numeric(substr(record, 442, 446))
  a[30] <- as.numeric(substr(record, 1433, 1437))
  
  b[31] <- as.numeric(substr(record, 314, 314))
  b[32] <- as.numeric(substr(record, 1197, 1197))
  a[31] <- as.numeric(substr(record, 348, 351))
  a[32] <- as.numeric(substr(record, 1295, 1298))
  
  b[33] <- as.numeric(substr(record, 317, 317))
  b[34] <- as.numeric(substr(record, 1200, 1200))
  a[33] <- as.numeric(substr(record, 447, 451))
  a[34] <- as.numeric(substr(record, 1438, 1442))
  
  b[35] <- as.numeric(substr(record, 320, 320))
  b[36] <- as.numeric(substr(record, 1203, 1203))
  a[35] <- as.numeric(substr(record, 452, 456))
  a[36] <- as.numeric(substr(record, 1443, 1447))
  
  a[37] <- as.numeric(substr(record, 1498, 1503))
  b[37] <- as.numeric(substr(record, 1235, 1235))
  
  a[38] <- as.numeric(substr(record, 1504, 1509))
  b[38] <- as.numeric(substr(record, 1236, 1236))
  
  a[39] <- as.numeric(substr(record, 1448, 1452))
  b[39] <- as.numeric(substr(record, 1242, 1242))
  
  a[40] <- as.numeric(substr(record, 1493, 1497))
  b[40] <- as.numeric(substr(record, 1245, 1245))
  
  a[41] <- as.numeric(substr(record, 1546, 1551))
  b[41] <- as.numeric(substr(record, 1256, 1256))
  
  a[42] <- as.numeric(substr(record, 1552, 1558))
  b[42] <- as.numeric(substr(record, 1239, 1239))
  
  a[43] <- as.numeric(substr(record, 1540, 1545))
  b[43] <- as.numeric(substr(record, 1248, 1248))
  
  a[44] <- as.numeric(substr(record, 1510, 1515))
  b[44] <- as.numeric(substr(record, 1253, 1253))
  
  for (j in 1:22) {
    b[j] <- 1
  }
  
  for (i in 1:44) {
    if (is.na(a[i])) {
      a[i] <- 0
    }
    if (is.na(b[i])) {
      b[i] <- 3
    }
    
    if (b[i] == 1) {
      b[i] <- 1
    }
    if (b[i] == 2) {
      b[i] <- 0
    }
    if (b[i] == 3) {
      b[i] <- 0.5
    }
  }
  
  equipment <- append(equipment, sum(a*b))
}


equipments <- matrix(0, nrow=3949, ncol=9)

for (j in 1:9) {
  equipments[,j] <- (days[,j]/rowSums(days))*equipment
}
equipments[is.nan(equipments)] <- 0
equipments <- data.frame(equipments)
colnames(equipments) <- c("New_England_equipments", "Middle_Atlantic_equipments", "East_North_Central_equipments", "West_North_Central_equipments", "South_Atlantic_equipments", "East_South_Central_equipments", "West_South_Central_equipments", "Mountain_equipments", "Pacific_equipments")

equipments



fishing <- cbind(personalinfo, trips, days, expenditures, equipments)

fishing


yearlywage <- 46640.94


#_NEW_ENGLAND

nef <- fishing %>%
  select(New_England_trips, wt, New_England_days, New_England_expenditures, New_England_equipments, from, income, sex, race, popden, fown) %>%
  filter(New_England_trips>0) %>%
  mutate(New_England_cost = (((New_England_days * yearlywage)/365) + New_England_expenditures + New_England_equipments)/New_England_trips) %>%
  mutate(onsitewt = (wt*New_England_trips))

nef

totaltrips <- sum(nef$New_England_trips*nef$wt)

ne <- glm(New_England_trips ~ New_England_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=nef, weights=wt)

nefconsumersurplus <- (-1/(coef(summary(ne))[2,1]))*(totaltrips)

nefconsumersurplus

#_MIDDLE_ATLANTIC

maf <- fishing %>%
  select(Middle_Atlantic_trips, wt, Middle_Atlantic_days, Middle_Atlantic_expenditures, Middle_Atlantic_equipments, from, income, sex, race, popden, fown) %>%
  filter(Middle_Atlantic_trips>0) %>%
  mutate(Middle_Atlantic_cost = (((Middle_Atlantic_days * yearlywage)/365) + Middle_Atlantic_expenditures + Middle_Atlantic_equipments)/Middle_Atlantic_trips) %>%
  mutate(onsitewt = (wt*Middle_Atlantic_trips))

maf

totaltrips <- sum(maf$Middle_Atlantic_trips*maf$wt)

ma <- glm(Middle_Atlantic_trips ~ Middle_Atlantic_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=maf, weights=wt)

mafconsumersurplus <- (-1/(coef(summary(ma))[2,1]))*(totaltrips)

mafconsumersurplus

#_EAST_NORTH_CENTRAL

encf <- fishing %>%
  select(East_North_Central_trips, wt, East_North_Central_days, East_North_Central_expenditures, East_North_Central_equipments, from, income, sex, race, popden, fown) %>%
  filter(East_North_Central_trips>0) %>%
  mutate(East_North_Central_cost = (((East_North_Central_days * yearlywage)/365) + East_North_Central_expenditures + East_North_Central_equipments)/East_North_Central_trips) %>%
  mutate(onsitewt = (wt*East_North_Central_trips))

encf

totaltrips <- sum(encf$East_North_Central_trips*encf$wt)

enc <- glm(East_North_Central_trips ~ East_North_Central_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=encf, weights=wt)

encfconsumersurplus <- (-1/(coef(summary(enc))[2,1]))*(totaltrips)

encfconsumersurplus

#_WEST_NORTH_CENTRAL

wncf <- fishing %>%
  select(West_North_Central_trips, wt, West_North_Central_days, West_North_Central_expenditures, West_North_Central_equipments, from, income, sex, race, popden, fown) %>%
  filter(West_North_Central_trips>0) %>%
  mutate(West_North_Central_cost = (((West_North_Central_days * yearlywage)/365) + West_North_Central_expenditures + West_North_Central_equipments)/West_North_Central_trips) %>%
  mutate(onsitewt = (wt*West_North_Central_trips))

wncf

totaltrips <- sum(wncf$West_North_Central_trips*wncf$wt)

wnc <- glm(West_North_Central_trips ~ West_North_Central_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=wncf, weights=wt)

wncfconsumersurplus <- (-1/(coef(summary(wnc))[2,1]))*(totaltrips)

wncfconsumersurplus

#_SOUTH_ATLANTIC

saf <- fishing %>%
  select(South_Atlantic_trips, wt, South_Atlantic_days, South_Atlantic_expenditures, South_Atlantic_equipments, from, income, sex, race, popden, fown) %>%
  filter(South_Atlantic_trips>0) %>%
  mutate(South_Atlantic_cost = (((South_Atlantic_days * yearlywage)/365) + South_Atlantic_expenditures + South_Atlantic_equipments)/South_Atlantic_trips) %>%
  mutate(onsitewt = (wt*South_Atlantic_trips))

saf

totaltrips <- sum(saf$South_Atlantic_trips*saf$wt)

sa <- glm(South_Atlantic_trips ~ South_Atlantic_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=saf, weights=wt)

safconsumersurplus <- (-1/(coef(summary(sa))[2,1]))*(totaltrips)

safconsumersurplus

#_EAST_SOUTH_CENTRAL

escf <- fishing %>%
  select(East_South_Central_trips, wt, East_South_Central_days, East_South_Central_expenditures, East_South_Central_equipments, from, income, sex, race, popden, fown) %>%
  filter(East_South_Central_trips>0) %>%
  mutate(East_South_Central_cost = (((East_South_Central_days * yearlywage)/365) + East_South_Central_expenditures + East_South_Central_equipments)/East_South_Central_trips) %>%
  mutate(onsitewt = (wt*East_South_Central_trips))

escf

totaltrips <- sum(escf$East_South_Central_trips*escf$wt)

esc <- glm(East_South_Central_trips ~ East_South_Central_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=escf, weights=wt)

escfconsumersurplus <- (-1/(coef(summary(esc))[2,1]))*(totaltrips)

escfconsumersurplus

#_WEST_SOUTH_CENTRAL

wscf <- fishing %>%
  select(West_South_Central_trips, wt, West_South_Central_days, West_South_Central_expenditures, West_South_Central_equipments, from, income, sex, race, popden, fown) %>%
  filter(West_South_Central_trips>0) %>%
  mutate(West_South_Central_cost = (((West_South_Central_days * yearlywage)/365) + West_South_Central_expenditures + West_South_Central_equipments)/West_South_Central_trips) %>%
  mutate(onsitewt = (wt*West_South_Central_trips))

wscf

totaltrips <- sum(wscf$West_South_Central_trips*wscf$wt)

wsc <- glm(West_South_Central_trips ~ West_South_Central_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=wscf, weights=wt)

wscfconsumersurplus <- (-1/(coef(summary(wsc))[2,1]))*(totaltrips)

wscfconsumersurplus

#_MOUNTAIN

mf <- fishing %>%
  select(Mountain_trips, wt, Mountain_days, Mountain_expenditures, Mountain_equipments, from, income, sex, race, popden, fown) %>%
  filter(Mountain_trips>0) %>%
  mutate(Mountain_cost = (((Mountain_days * yearlywage)/365) + Mountain_expenditures + Mountain_equipments)/Mountain_trips) %>%
  mutate(onsitewt = (wt*Mountain_trips))

mf

totaltrips <- sum(mf$Mountain_trips*mf$wt)

m <- glm(Mountain_trips ~ Mountain_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=mf, weights=wt)

mfconsumersurplus <- (-1/(coef(summary(m))[2,1]))*(totaltrips)

mfconsumersurplus

#_PACIFIC

pf <- fishing %>%
  select(Pacific_trips, wt, Pacific_days, Pacific_expenditures, Pacific_equipments, from, income, sex, race, popden, fown) %>%
  filter(Pacific_trips>0) %>%
  mutate(Pacific_cost = (((Pacific_days * yearlywage)/365) + Pacific_expenditures + Pacific_equipments)/Pacific_trips) %>%
  mutate(onsitewt = (wt*Pacific_trips))

pf

totaltrips <- sum(pf$Pacific_trips*pf$wt)

p <- glm(Pacific_trips ~ Pacific_cost + factor(from) + factor(race) + factor(popden), family="poisson", data=pf, weights=wt)

pfconsumersurplus <- (-1/(coef(summary(p))[2,1]))*(totaltrips)

pfconsumersurplus


# FINAL

consumersurpluses <- c(nefconsumersurplus, mafconsumersurplus, encfconsumersurplus, wncfconsumersurplus, safconsumersurplus, escfconsumersurplus, wscfconsumersurplus, mfconsumersurplus, pfconsumersurplus)
regions <- c("New_England", "Middle_Atlantic", "East_North_Central", "West_North_Central", "South_Atlantic", "East_South_Central", "West_South_Central", "Mountain", "Pacific")
finalconsumersurpluses <- data.frame(regions, consumersurpluses)


finalconsumersurpluses