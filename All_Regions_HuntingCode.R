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

hown <- vector()
for (record in sp) {
  hown <- append(hown, substr(record, 1221, 1221))
}

personalinfo <- data.frame(wt, from, income, sex, race, popden, hown)

personalinfo



trips <- matrix(0, nrow=3949, ncol=9)

for (i in seq_along(sp)) {
  r <- vector("integer" , 3)
  r[1] <- as.numeric(substr(sp[i], 1648, 1649))
  r[2] <- as.numeric(substr(sp[i], 1650, 1651))
  r[3] <- as.numeric(substr(sp[i], 1652, 1653))
  t1 <- vector("numeric" , 3)
  t1[1] <- as.numeric(substr(sp[i], 1786, 1789))
  t1[2] <- as.numeric(substr(sp[i], 1790, 1793))
  t1[3] <- as.numeric(substr(sp[i], 1794, 1797))
  t2 <- vector("numeric" , 3)
  t2[1] <- as.numeric(substr(sp[i], 1942, 1945))
  t2[2] <- as.numeric(substr(sp[i], 1946, 1949))
  t2[3] <- as.numeric(substr(sp[i], 1950, 1953))
  t3 <- vector("numeric" , 3)
  t3[1] <- as.numeric(substr(sp[i], 2086, 2089))
  t3[2] <- as.numeric(substr(sp[i], 2090, 2093))
  t3[3] <- as.numeric(substr(sp[i], 2094, 2097))
  t4 <- vector("numeric" , 3)
  t4[1] <- as.numeric(substr(sp[i], 2203, 2206))
  t4[2] <- as.numeric(substr(sp[i], 2207, 2210))
  t4[3] <- as.numeric(substr(sp[i], 2211, 2214))
  for (j in 1:3) {
    if (is.na(t1[j])) {
      t1[j] <- 0
    }
    if (is.na(t2[j])) {
      t2[j] <- 0
    }
    if (is.na(t3[j])) {
      t3[j] <- 0
    }
    if (is.na(t4[j])) {
      t4[j] <- 0
    }
    trips[i, r[j]] <- trips[i, r[j]] + t1[j] + t2[j] + t3[j] + t4[j]
  }
}

trips <- data.frame(trips)
colnames(trips) <- c("New_England_trips", "Middle_Atlantic_trips", "East_North_Central_trips", "West_North_Central_trips", "South_Atlantic_trips", "East_South_Central_trips", "West_South_Central_trips", "Mountain_trips", "Pacific_trips")

trips



days <- matrix(0, nrow=3949, ncol=9)

for (i in seq_along(sp)) {
  r <- vector("integer" , 3)
  r[1] <- as.numeric(substr(sp[i], 1648, 1649))
  r[2] <- as.numeric(substr(sp[i], 1650, 1651))
  r[3] <- as.numeric(substr(sp[i], 1652, 1653))
  d <- vector("numeric" , 3)
  d[1] <- as.numeric(substr(sp[i], 1654, 1656))
  d[2] <- as.numeric(substr(sp[i], 1657, 1659))
  d[3] <- as.numeric(substr(sp[i], 1660, 1662))
  for (j in 1:3) {
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
  r <- vector("integer" , 3)
  
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
  
  exp2_1 <- vector("numeric" , 3)
  exp2_2 <- vector("numeric" , 3)
  exp2_3 <- vector("numeric" , 3)
  exp2_4 <- vector("numeric" , 3)
  exp2_5 <- vector("numeric" , 3)
  exp2_6 <- vector("numeric" , 3)
  exp2_7 <- vector("numeric" , 3)
  exp2_8 <- vector("numeric" , 3)
  exp2_9 <- vector("numeric" , 3)
  exp2_10 <- vector("numeric" , 3)
  exp2_11 <- vector("numeric" , 3)
  exp2_12 <- vector("numeric" , 3)
  exp2_13 <- vector("numeric" , 3)
  
  exp3_1 <- vector("numeric" , 3)
  exp3_2 <- vector("numeric" , 3)
  exp3_3 <- vector("numeric" , 3)
  exp3_4 <- vector("numeric" , 3)
  exp3_5 <- vector("numeric" , 3)
  exp3_6 <- vector("numeric" , 3)
  exp3_7 <- vector("numeric" , 3)
  exp3_8 <- vector("numeric" , 3)
  exp3_9 <- vector("numeric" , 3)
  exp3_10 <- vector("numeric" , 3)
  exp3_11 <- vector("numeric" , 3)
  exp3_12 <- vector("numeric" , 3)
  exp3_13 <- vector("numeric" , 3)
  
  exp4_1 <- vector("numeric" , 3)
  exp4_2 <- vector("numeric" , 3)
  exp4_3 <- vector("numeric" , 3)
  exp4_4 <- vector("numeric" , 3)
  exp4_5 <- vector("numeric" , 3)
  exp4_6 <- vector("numeric" , 3)
  exp4_7 <- vector("numeric" , 3)
  exp4_8 <- vector("numeric" , 3)
  exp4_9 <- vector("numeric" , 3)
  exp4_10 <- vector("numeric" , 3)
  exp4_11 <- vector("numeric" , 3)
  exp4_12 <- vector("numeric" , 3)
  exp4_13 <- vector("numeric" , 3)
  
  for (j in 1:3) {
    r[j] <- as.numeric(substr(sp[i], 1646 + 2*j, 1647 + 2*j))
    
    exp1_1[j] <- as.numeric(substr(sp[i], (2268 + 7*j), (2274 + 7*j)))
    exp1_2[j] <- as.numeric(substr(sp[i], (2289 + 7*j), (2295 + 7*j)))
    exp1_3[j] <- as.numeric(substr(sp[i], (2310 + 7*j), (2316 + 7*j)))
    exp1_4[j] <- as.numeric(substr(sp[i], (2331 + 7*j), (2337 + 7*j)))
    exp1_5[j] <- as.numeric(substr(sp[i], (2352 + 7*j), (2358 + 7*j)))
    exp1_6[j] <- as.numeric(substr(sp[i], (2373 + 7*j), (2379 + 7*j)))
    exp1_7[j] <- as.numeric(substr(sp[i], (2394 + 7*j), (2400 + 7*j)))
    exp1_8[j] <- as.numeric(substr(sp[i], (2415 + 7*j), (2421 + 7*j)))
    exp1_9[j] <- as.numeric(substr(sp[i], (2436 + 7*j), (2442 + 7*j)))
    exp1_10[j] <- as.numeric(substr(sp[i], (2457 + 7*j), (2463 + 7*j)))
    exp1_11[j] <- as.numeric(substr(sp[i], (2478 + 7*j), (2484 + 7*j)))
    exp1_12[j] <- as.numeric(substr(sp[i], (2499 + 7*j), (2505 + 7*j)))
    exp1_13[j] <- as.numeric(substr(sp[i], (2520 + 7*j), (2526 + 7*j)))
    
    exp2_1[j] <- as.numeric(substr(sp[i], (2565 + 7*j), (2571 + 7*j)))
    exp2_2[j] <- as.numeric(substr(sp[i], (2586 + 7*j), (2592 + 7*j)))
    exp2_3[j] <- as.numeric(substr(sp[i], (2607 + 7*j), (2613 + 7*j)))
    exp2_4[j] <- as.numeric(substr(sp[i], (2628 + 7*j), (2634 + 7*j)))
    exp2_5[j] <- as.numeric(substr(sp[i], (2649 + 7*j), (2655 + 7*j)))
    exp2_6[j] <- as.numeric(substr(sp[i], (2670 + 7*j), (2676 + 7*j)))
    exp2_7[j] <- as.numeric(substr(sp[i], (2691 + 7*j), (2697 + 7*j)))
    exp2_8[j] <- as.numeric(substr(sp[i], (2712 + 7*j), (2718 + 7*j)))
    exp2_9[j] <- as.numeric(substr(sp[i], (2733 + 7*j), (2739 + 7*j)))
    exp2_10[j] <- as.numeric(substr(sp[i], (2754 + 7*j), (2760 + 7*j)))
    exp2_11[j] <- as.numeric(substr(sp[i], (2775 + 7*j), (2781 + 7*j)))
    exp2_12[j] <- as.numeric(substr(sp[i], (2796 + 7*j), (2802 + 7*j)))
    exp2_13[j] <- as.numeric(substr(sp[i], (2817 + 7*j), (2823 + 7*j)))
    
    exp3_1[j] <- as.numeric(substr(sp[i], (2862 + 7*j), (2868 + 7*j)))
    exp3_2[j] <- as.numeric(substr(sp[i], (2883 + 7*j), (2889 + 7*j)))
    exp3_3[j] <- as.numeric(substr(sp[i], (2904 + 7*j), (2910 + 7*j)))
    exp3_4[j] <- as.numeric(substr(sp[i], (2925 + 7*j), (2931 + 7*j)))
    exp3_5[j] <- as.numeric(substr(sp[i], (2946 + 7*j), (2952 + 7*j)))
    exp3_6[j] <- as.numeric(substr(sp[i], (2967 + 7*j), (2973 + 7*j)))
    exp3_7[j] <- as.numeric(substr(sp[i], (2988 + 7*j), (2994 + 7*j)))
    exp3_8[j] <- as.numeric(substr(sp[i], (3009 + 7*j), (3015 + 7*j)))
    exp3_9[j] <- as.numeric(substr(sp[i], (3030 + 7*j), (3036 + 7*j)))
    exp3_10[j] <- as.numeric(substr(sp[i], (3051 + 7*j), (3057 + 7*j)))
    exp3_11[j] <- as.numeric(substr(sp[i], (3072 + 7*j), (3078 + 7*j)))
    exp3_12[j] <- as.numeric(substr(sp[i], (3093 + 7*j), (3099 + 7*j)))
    exp3_13[j] <- as.numeric(substr(sp[i], (3114 + 7*j), (3120 + 7*j)))
    
    exp4_1[j] <- as.numeric(substr(sp[i], (3159 + 7*j), (3165 + 7*j)))
    exp4_2[j] <- as.numeric(substr(sp[i], (3180 + 7*j), (3186 + 7*j)))
    exp4_3[j] <- as.numeric(substr(sp[i], (3201 + 7*j), (3207 + 7*j)))
    exp4_4[j] <- as.numeric(substr(sp[i], (3222 + 7*j), (3228 + 7*j)))
    exp4_5[j] <- as.numeric(substr(sp[i], (3243 + 7*j), (3249 + 7*j)))
    exp4_6[j] <- as.numeric(substr(sp[i], (3264 + 7*j), (3270 + 7*j)))
    exp4_7[j] <- as.numeric(substr(sp[i], (3285 + 7*j), (3291 + 7*j)))
    exp4_8[j] <- as.numeric(substr(sp[i], (3306 + 7*j), (3312 + 7*j)))
    exp4_9[j] <- as.numeric(substr(sp[i], (3327 + 7*j), (3333 + 7*j)))
    exp4_10[j] <- as.numeric(substr(sp[i], (3348 + 7*j), (3354 + 7*j)))
    exp4_11[j] <- as.numeric(substr(sp[i], (3369 + 7*j), (3375 + 7*j)))
    exp4_12[j] <- as.numeric(substr(sp[i], (3390 + 7*j), (3396 + 7*j)))
    exp4_13[j] <- as.numeric(substr(sp[i], (3411 + 7*j), (3417 + 7*j)))
    
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
    
    if (is.na(exp4_1[j])) {
      exp4_1[j] <- 0
    }
    if (is.na(exp4_2[j])) {
      exp4_2[j] <- 0
    }
    if (is.na(exp4_3[j])) {
      exp4_3[j] <- 0
    }
    if (is.na(exp4_4[j])) {
      exp4_4[j] <- 0
    }
    if (is.na(exp4_5[j])) {
      exp4_5[j] <- 0
    }
    if (is.na(exp4_6[j])) {
      exp4_6[j] <- 0
    }
    if (is.na(exp4_7[j])) {
      exp4_7[j] <- 0
    }
    if (is.na(exp4_8[j])) {
      exp4_8[j] <- 0
    }
    if (is.na(exp4_9[j])) {
      exp4_9[j] <- 0
    }
    if (is.na(exp4_10[j])) {
      exp4_10[j] <- 0
    }
    if (is.na(exp4_11[j])) {
      exp4_11[j] <- 0
    }
    if (is.na(exp4_12[j])) {
      exp4_12[j] <- 0
    }
    if (is.na(exp4_13[j])) {
      exp4_13[j] <- 0
    }
    
    expenditures[i, r[j]] <- expenditures[i, r[j]] + exp1_1[j] + exp1_2[j] + exp1_3[j] + exp1_4[j] + exp1_5[j] + exp1_6[j] + exp1_7[j] + exp1_8[j] + exp1_9[j] + exp1_10[j] + exp1_11[j] + exp1_12[j] + exp1_13[j] + exp2_1[j] + exp2_2[j] + exp2_3[j] + exp2_4[j] + exp2_5[j] + exp2_6[j] + exp2_7[j] + exp2_8[j] + exp2_9[j] + exp2_10[j] + exp2_11[j] + exp2_12[j] + exp2_13[j] + exp3_1[j] + exp3_2[j] + exp3_3[j] + exp3_4[j] + exp3_5[j] + exp3_6[j] + exp3_7[j] + exp3_8[j] + exp3_9[j] + exp3_10[j] + exp3_11[j] + exp3_12[j] + exp3_13[j] + exp4_1[j] + exp4_2[j] + exp4_3[j] + exp4_4[j] + exp4_5[j] + exp4_6[j] + exp4_7[j] + exp4_8[j] + exp4_9[j] + exp4_10[j] + exp4_11[j] + exp4_12[j] + exp4_13[j]
  }
}

expenditures <- data.frame(expenditures)
colnames(expenditures) <- c("New_England_expenditures", "Middle_Atlantic_expenditures", "East_North_Central_expenditures", "West_North_Central_expenditures", "South_Atlantic_expenditures", "East_South_Central_expenditures", "West_South_Central_expenditures", "Mountain_expenditures", "Pacific_expenditures")

expenditures



equipment <- vector()

for (record in sp) {
  a <- vector(mode="numeric", length=44)
  b <- vector(mode="numeric", length=44)
  a[1] <- as.numeric(substr(record, 352, 356))
  a[2] <- as.numeric(substr(record, 1343, 1347))
  a[3] <- as.numeric(substr(record, 357, 361))
  a[4] <- as.numeric(substr(record, 1348, 1352))
  a[5] <- as.numeric(substr(record, 362, 366))
  a[6] <- as.numeric(substr(record, 1353, 1357))
  a[7] <- as.numeric(substr(record, 367, 371))
  a[8] <- as.numeric(substr(record, 1358, 1362))
  a[9] <- as.numeric(substr(record, 372, 376))
  a[10] <- as.numeric(substr(record, 1363, 1367))
  a[11] <- as.numeric(substr(record, 377, 381))
  a[12] <- as.numeric(substr(record, 1368, 1372))
  a[13] <- as.numeric(substr(record, 382, 386))
  a[14] <- as.numeric(substr(record, 1373, 1377))
  a[15] <- as.numeric(substr(record, 387, 391))
  a[16] <- as.numeric(substr(record, 1378, 1382))
  a[17] <- as.numeric(substr(record, 392, 396))
  a[18] <- as.numeric(substr(record, 1383, 1387))
  a[19] <- as.numeric(substr(record, 397, 401))
  a[20] <- as.numeric(substr(record, 1388, 1392))
  a[21] <- as.numeric(substr(record, 402, 406))
  a[22] <- as.numeric(substr(record, 1393, 1397))
  
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
    b[j] <- 2
  }
  
  for (i in 1:44) {
    if (is.na(a[i])) {
      a[i] <- 0
    }
    if (is.na(b[i])) {
      b[i] <- 3
    }
    
    if (b[i] == 1) {
      b[i] <- 0
    }
    if (b[i] == 2) {
      b[i] <- 1
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



hunting <- cbind(personalinfo, trips, days, expenditures, equipments)

hunting


yearlywage <- 46640.94


#_NEW_ENGLAND

neh <- hunting %>%
  select(New_England_trips, wt, New_England_days, New_England_expenditures, New_England_equipments, from, income, sex, race, popden, hown) %>%
  filter(New_England_trips>0) %>%
  mutate(New_England_cost = (((New_England_days * yearlywage)/365) + New_England_expenditures + New_England_equipments)/New_England_trips) %>%
  mutate(onsitewt = (wt*New_England_trips))

neh

totaltrips <- sum(neh$New_England_trips*neh$wt)

ne <- glm(New_England_trips ~ New_England_cost + factor(popden), family="poisson", data=neh, weights=wt)

nehconsumersurplus <- (-1/(coef(summary(ne))[2,1]))*(totaltrips)

nehconsumersurplus

#_MIDDLE_ATLANTIC

mah <- hunting %>%
  select(Middle_Atlantic_trips, wt, Middle_Atlantic_days, Middle_Atlantic_expenditures, Middle_Atlantic_equipments, from, income, sex, race, popden, hown) %>%
  filter(Middle_Atlantic_trips>0) %>%
  mutate(Middle_Atlantic_cost = (((Middle_Atlantic_days * yearlywage)/365) + Middle_Atlantic_expenditures + Middle_Atlantic_equipments)/Middle_Atlantic_trips) %>%
  mutate(onsitewt = (wt*Middle_Atlantic_trips))

mah

totaltrips <- sum(mah$Middle_Atlantic_trips*mah$wt)

ma <- glm(Middle_Atlantic_trips ~ Middle_Atlantic_cost + factor(popden), family="poisson", data=mah, weights=wt)

mahconsumersurplus <- (-1/(coef(summary(ma))[2,1]))*(totaltrips)

mahconsumersurplus

#_EAST_NORTH_CENTRAL

ench <- hunting %>%
  select(East_North_Central_trips, wt, East_North_Central_days, East_North_Central_expenditures, East_North_Central_equipments, from, income, sex, race, popden, hown) %>%
  filter(East_North_Central_trips>0) %>%
  mutate(East_North_Central_cost = (((East_North_Central_days * yearlywage)/365) + East_North_Central_expenditures + East_North_Central_equipments)/East_North_Central_trips) %>%
  mutate(onsitewt = (wt*East_North_Central_trips))

ench

totaltrips <- sum(ench$East_North_Central_trips*ench$wt)

enc <- glm(East_North_Central_trips ~ East_North_Central_cost + factor(popden), family="poisson", data=ench, weights=wt)

enchconsumersurplus <- (-1/(coef(summary(enc))[2,1]))*(totaltrips)

enchconsumersurplus

#_WEST_NORTH_CENTRAL

wnch <- hunting %>%
  select(West_North_Central_trips, wt, West_North_Central_days, West_North_Central_expenditures, West_North_Central_equipments, from, income, sex, race, popden, hown) %>%
  filter(West_North_Central_trips>0) %>%
  mutate(West_North_Central_cost = (((West_North_Central_days * yearlywage)/365) + West_North_Central_expenditures + West_North_Central_equipments)/West_North_Central_trips) %>%
  mutate(onsitewt = (wt*West_North_Central_trips))

wnch

totaltrips <- sum(wnch$West_North_Central_trips*wnch$wt)

wnc <- glm(West_North_Central_trips ~ West_North_Central_cost + factor(popden), family="poisson", data=wnch, weights=wt)

wnchconsumersurplus <- (-1/(coef(summary(wnc))[2,1]))*(totaltrips)

wnchconsumersurplus

#_SOUTH_ATLANTIC

sah <- hunting %>%
  select(South_Atlantic_trips, wt, South_Atlantic_days, South_Atlantic_expenditures, South_Atlantic_equipments, from, income, sex, race, popden, hown) %>%
  filter(South_Atlantic_trips>0) %>%
  mutate(South_Atlantic_cost = (((South_Atlantic_days * yearlywage)/365) + South_Atlantic_expenditures + South_Atlantic_equipments)/South_Atlantic_trips) %>%
  mutate(onsitewt = (wt*South_Atlantic_trips))

sah

totaltrips <- sum(sah$South_Atlantic_trips*sah$wt)

sa <- glm(South_Atlantic_trips ~ South_Atlantic_cost + factor(popden), family="poisson", data=sah, weights=wt)

sahconsumersurplus <- (-1/(coef(summary(sa))[2,1]))*(totaltrips)

sahconsumersurplus

#_EAST_SOUTH_CENTRAL

esch <- hunting %>%
  select(East_South_Central_trips, wt, East_South_Central_days, East_South_Central_expenditures, East_South_Central_equipments, from, income, sex, race, popden, hown) %>%
  filter(East_South_Central_trips>0) %>%
  mutate(East_South_Central_cost = (((East_South_Central_days * yearlywage)/365) + East_South_Central_expenditures + East_South_Central_equipments)/East_South_Central_trips) %>%
  mutate(onsitewt = (wt*East_South_Central_trips))

esch

totaltrips <- sum(esch$East_South_Central_trips*esch$wt)

esc <- glm(East_South_Central_trips ~ East_South_Central_cost + factor(popden), family="poisson", data=esch, weights=wt)

eschconsumersurplus <- (-1/(coef(summary(esc))[2,1]))*(totaltrips)

eschconsumersurplus

#_WEST_SOUTH_CENTRAL

wsch <- hunting %>%
  select(West_South_Central_trips, wt, West_South_Central_days, West_South_Central_expenditures, West_South_Central_equipments, from, income, sex, race, popden, hown) %>%
  filter(West_South_Central_trips>0) %>%
  mutate(West_South_Central_cost = (((West_South_Central_days * yearlywage)/365) + West_South_Central_expenditures + West_South_Central_equipments)/West_South_Central_trips) %>%
  mutate(onsitewt = (wt*West_South_Central_trips))

wsch

totaltrips <- sum(wsch$West_South_Central_trips*wsch$wt)

wsc <- glm(West_South_Central_trips ~ West_South_Central_cost + factor(popden), family="poisson", data=wsch, weights=wt)

wschconsumersurplus <- (-1/(coef(summary(wsc))[2,1]))*(totaltrips)

wschconsumersurplus

#_MOUNTAIN

mh <- hunting %>%
  select(Mountain_trips, wt, Mountain_days, Mountain_expenditures, Mountain_equipments, from, income, sex, race, popden, hown) %>%
  filter(Mountain_trips>0) %>%
  mutate(Mountain_cost = (((Mountain_days * yearlywage)/365) + Mountain_expenditures + Mountain_equipments)/Mountain_trips) %>%
  mutate(onsitewt = (wt*Mountain_trips))

mh

totaltrips <- sum(mh$Mountain_trips*mh$wt)

m <- glm(Mountain_trips ~ Mountain_cost + factor(popden), family="poisson", data=mh, weights=wt)

mhconsumersurplus <- (-1/(coef(summary(m))[2,1]))*(totaltrips)

mhconsumersurplus

#_PACIFIC

ph <- hunting %>%
  select(Pacific_trips, wt, Pacific_days, Pacific_expenditures, Pacific_equipments, from, income, sex, race, popden, hown) %>%
  filter(Pacific_trips>0) %>%
  mutate(Pacific_cost = (((Pacific_days * yearlywage)/365) + Pacific_expenditures + Pacific_equipments)/Pacific_trips) %>%
  mutate(onsitewt = (wt*Pacific_trips))

ph

totaltrips <- sum(ph$Pacific_trips*ph$wt)

p <- glm(Pacific_trips ~ Pacific_cost + factor(popden), family="poisson", data=ph, weights=wt)

phconsumersurplus <- (-1/(coef(summary(p))[2,1]))*(totaltrips)

phconsumersurplus


# FINAL

consumersurpluses <- c(nehconsumersurplus, mahconsumersurplus, enchconsumersurplus, wnchconsumersurplus, sahconsumersurplus, eschconsumersurplus, wschconsumersurplus, mhconsumersurplus, phconsumersurplus)
regions <- c("New_England", "Middle_Atlantic", "East_North_Central", "West_North_Central", "South_Atlantic", "East_South_Central", "West_South_Central", "Mountain", "Pacific")
finalconsumersurpluses <- data.frame(regions, consumersurpluses)


finalconsumersurpluses