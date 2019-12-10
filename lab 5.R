# Name: Faye Bandet
# Date: 9/26/19
# ISTA 116 Section B || Section Leader : Jacob Heller
# Lab Assignment 5
# Collaborator(s): Nick Ackerman

download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.RData")

#1
kobe.basket.prob <- prop.table(table(kobe$basket))
kobe.basket.prob
# H = 0.4360902, M = 0.5639098 

#2
kobe.basket.prob["H"]
# H = 0.4360902 

#3
shots = c(kobe$basket)[0:13]
calc_streak(shots)
# [1] 1 0 2 0 0 0 3 0
# calculating streaks is calculated by the number of hits in the data set.The misses are zeroes and the streaks are counted.

#4
ktable = table(calc_streak(c(kobe$basket)))
barplot(ktable)
# bar plot appeared with five bars, which decreases over the data set.

#5
H <- kobe.basket.prob[1]
M <- kobe.basket.prob[2]
newSamp <- sample(c("H", "M"), size = 133, prob = c(H, M), replace = TRUE)

#6
prop.table(table(newSamp))
kobe.basket.prob
# newSamp probability: H = 0.4285714, M = 0.5714286, compared to Kobe's basket probaability H = 0.4360902, M = 0.5639098. It does differ, and it should because they are different samples and they should be super close, the greater the data set makes it more accurate.

#7
newTable = table(calc_streak(c(newSamp)))
barplot(ktable)
barplot(newTable)
# The tables are different because the different streaks exists because there is random data in the sample.

#8
newBask = c(newSamp) [1:13]
tinyTab = table(calc_streak(newBask))
prop.table(ktable)
prop.table(tinyTab)
# They are actually pretty different between my results and Nick's results. This is because it is random. 
# I got   0   1   2 
#        0.7 0.2 0.1 
#0          1          2          3          4 
#0.51315789 0.31578947 0.07894737 0.07894737 0.01315789 

#9
mySamp = replicate(100,{
  nextSamp = sample(c("H", "M"), size = 133, prob = kobe.basket.prob, replace = TRUE);
  probs = prop.table(table(calc_streak(c(nextSamp))));
  prob.0 = probs["0"] 
})
summary(mySamp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4531  0.5276  0.5677  0.5652  0.6083  0.6739 
# The sample missed about 56% of shots, maximum was 67%. 

#10
treeSamp = replicate(100,{
    nextSamp = sample(c("H", "M"), size = 133, prob = kobe.basket.prob, replace = TRUE);
      probs = prop.table(table(calc_streak(c(nextSamp))));
      prob.3 = probs["3"] 
      })
summary(treeSamp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.01191 0.02985 0.04546 0.04607 0.05984 0.10667  5 
# The sample missed about 4.6% of shots, 5 data sets had no streaks of three.
# Kobe's probability is not typical. He is more likely to get streaks at about 7.9% compared to the random simulated streak player.