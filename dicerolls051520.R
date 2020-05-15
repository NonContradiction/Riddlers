#We install a package with functions for discrete uniform numbers: 
#install.packages("extraDistr")
library(extraDistr)
#and check it out
rdunif(5, 1, 20)

########################
#We start with SIMULATIONS
#########################

#We define funtions for the different styles of rolls: 
advantageroll <- function() {
  x <- rdunif(1, 1, 20)
  y <- rdunif(1, 1, 20)
  z <- max(x, y)
  return(z)
}

disadvantageroll <- function() {
  x <- rdunif(1, 1, 20)
  y <- rdunif(1, 1, 20)
  z <- min(x, y)
  return(z)
}

doaroll <- function(i) {
  x <- advantageroll()
  y <- advantageroll()
  z <- min(x, y)
  return(z)  
}

aodroll <- function(i) {
  x <- disadvantageroll()
  y <- disadvantageroll()
  z <- max(x, y)
  return(z)  
}

plainroll <- function(i) {
  x <- rdunif(1, 1, 20)
  return(x)
}

#We simulate 100,000 different rolls of each style: 
firstpass <- sapply(1:100000, aodroll)
secondpass <- sapply(1:100000, doaroll)
zeropass <- sapply(1:100000, plainroll)

#some descriptive statistics: 
mean(firstpass)
mean(secondpass)
mean(zeropass)

median(firstpass)
median(secondpass)
median(zeropass)

table(firstpass)
table(secondpass)
table(zeropass)


#GGPLOT way of plotting histograms of simulation results: 
library(ggplot2)
require(reshape2)
df <- melt(data.frame(firstpass, secondpass, zeropass))


colors <- c("Disadvantage of Advantage" = "green", "Advantage of Disadvantage" = "red", "R20" = "blue")

ggplot(df,aes(x=value)) + 
  geom_histogram(aes(fill = "Advantage of Disadvantage"), data=subset(df,variable == 'firstpass'), binwidth = 1, alpha = 0.2) +
  geom_histogram(aes(fill = "Disadvantage of Advantage"), data=subset(df,variable == 'secondpass'),binwidth = 1, alpha = 0.2) +
  geom_histogram(aes(fill = "R20"), data=subset(df,variable == 'zeropass'),binwidth = 1, alpha = 0.2) +
  ggtitle("Overlaid Histograms of AOD, DOA, and D20 Roll Simulations: 100,000 Each") + 
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold")
  ) +
  labs(y= "Counts", x = "D20 Roll Values", color = "Legend") +
  scale_color_manual(values = colors)

##########################################
#COMPUTATIONS
##########################

#We look at some expectations
#We compute the expected value of a disadvantaged roll by weighting an average by its probability
compexdis <- function(i) {
  j <- i - 1
  #First we compute the weights by enumerating the outcomes (we divide by 400 later)
  counts <- (20 - j) + (20 - j) - 1
  #Then weight the value (i) by them
  expectation <- counts * i
  return(expectation)
}

sumsbyindex <- sapply(1:20, compexdis)
sumsbyindex
plot(sumsbyindex)

#We now divide the entire sum by 400 by the transitive property: 
ExpectationDis <- sum(sumsbyindex)/400
ExpectationDis

#We compute the expected value of an advantaged roll by weighting an average by its probability
compexadv <- function(i) {
  j <- 20 - i
  #First we compute the weights by enumerating the outcomes (we divide by 400 later)
  counts <- (20 - j) + (20 - j) - 1
  #Then weight the value (i) by them
  expectation <- counts * i
  return(expectation)
}

sumsbyindex <- sapply(1:20, compexadv)
sumsbyindex
plot(sumsbyindex)

#We now divide the entire sum by 400 by the transitive property: 
ExpectationAdv <- sum(sumsbyindex)/400
ExpectationAdv
#Note that it's symmetric about the mean of a D20 roll

##########################
#Expected Value For ADVANTAGE OF DISADVANTAGE
#We use a brute force method that looks at the whole sample space of (20 * 20) * (20 * 20) possibilities
#There is more elegant code for DOA
#I could not get it, symmetric though it was, to work for DOA
#Such is life

twentynums <- 1:20
twentynums

brutegrid <- expand.grid(twentynums, twentynums)
#View(brutegrid)

brutegrid$disVal <- apply(brutegrid, 1, function(x) min(x))
#View(brutegrid)

fullongrid <- expand.grid(brutegrid$disVal, brutegrid$disVal)
#View(fullongrid)

fullongrid$advVal <- apply(fullongrid, 1, function(x) max(x))
#View(fullongrid)

answergen <- function(i) {
  myanswer <- table(fullongrid$advVal)[[i]] * i
  return(myanswer)
}

thissequence <- sapply(1:20, answergen)
thissequence

ExpecAOD <- sum(thissequence)/160000
ExpecAOD
#Accords with our sample mean--good!

##################
#And for DOA: 
countsadv <- function(i) {
  j <- 20 - i
  #First we compute the weights by enumerating the outcomes (we divide by 400 later)
  counts <- (20 - j) + (20 - j) - 1
  return(counts)
}
advcounts <- sapply(1:20, countsadv)
advcounts
#Want 400: 
sum(advcounts)

DOAgrid <- expand.grid(advcounts, advcounts)
DOAgrid$minVal <- apply(DOAgrid, 1, function(x) min(x))
DOAgrid$counts <- DOAgrid$Var1 * DOAgrid$Var2
#View(DOAgrid)
#Want 160,000: 
sum(DOAgrid$counts)
#DOAgrid[which(DOAgrid$minVal==33), 4]

compexdoa <- function(i) {
  #First we compute the weights by enumerating the outcomes (we divide by 160,000 later)
  #Note that we can use the discounts weights as a proxy for the roll value: 
  myindex <- advcounts[[i]]
  thissum <- sum(DOAgrid[which(DOAgrid$minVal==myindex), 4])
  #and we go ahead and weight it: 
  weighted <- thissum * i
  return(weighted)
}

counts_DOA <- sapply(1:20, compexdoa)
counts_DOA

ExpecDOA <- sum(counts_DOA)/160000
ExpecDOA
#Also accords with our sample mean
#sweet!

#Extra Credit: 
#Done using the simulation data: 
colors <- c("Disadvantage of Advantage" = "green", "Advantage of Disadvantage" = "red", "R20" = "blue")

ggplot(df, aes(x=value)) + 
  stat_ecdf(data=subset(df,variable == 'firstpass'), aes(col = "Advantage of Disadvantage"), geom = "step") + 
  stat_ecdf(data=subset(df,variable == 'secondpass'), aes(col = "Disadvantage of Advantage"), geom = "step") + 
  stat_ecdf(data=subset(df,variable == 'zeropass'), aes(col = "R20"), geom = "step") +
  ggtitle("Empirical Cumulative Distributions Of AOD, DOA, And D20 Roll Simulations: 100,000 Each") + 
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold")
  ) +
  labs(y= "% Of Rolls", x = "D20 Roll Values", color = "Legend", 
  caption = "The color line that is the *lowest* indicates the roll style with the greatest probability of rolling *above* that value; 
       the best chance of rolling equal to or greater than some value is given by the lowest color line to the immediate left of that value's point on the x-axis") + 
  scale_color_manual(values = colors)

#Done with theoretical data: 

#Pure D20: 
d20 <- 1:20
d20probs <- d20/20
d20probs <- rev(d20probs)
d20probs

#Disadvantage of Advantage: 
compcountsdoa <- function(i) {
  #First we compute the weights by enumerating the outcomes (we divide by 160,000 later)
  #Note that we can use the discounts weights as a proxy for the roll value: 
  myindex <- advcounts[[i]]
  thissum <- sum(DOAgrid[which(DOAgrid$minVal==myindex), 4])
  return(thissum)
}

purecounts_DOA <- sapply(1:20, compcountsdoa)
purecounts_DOA
purecounts_DOA2 <- rev(purecounts_DOA)
purecounts_DOA2
ecdfDOA <- cumsum(purecounts_DOA2)/sum(purecounts_DOA2)
ecdfDOA <- rev(ecdfDOA)
ecdfDOA

#Advantage of Disadvantage: 
thissequencemodded <- table(fullongrid$advVal)[1:20]
thissequence2 <- rev(thissequencemodded)
thissequence2
ecdfAOD <- cumsum(thissequence2)/sum(thissequence2)
ecdfAOD
ecdfAOD <- rev(ecdfAOD)
ecdfAOD
ecdfAOD

Ndf <- data.frame(d20probs, ecdfDOA, ecdfAOD)
Ndf

Ndf$N = 1:20

colors <- c("Disadvantage of Advantage" = "green", "Advantage of Disadvantage" = "red", "R20" = "blue")

ggplot() +
  geom_step(data=Ndf, mapping=aes(x=N, y=d20probs, col = 'R20'), linetype=1) +
  geom_step(data=Ndf, mapping=aes(x=N, y=ecdfDOA, col = 'Disadvantage of Advantage'), linetype=1) +
  geom_step(data=Ndf, mapping=aes(x=N, y=ecdfAOD, col = 'Advantage of Disadvantage'), linetype=1) +
  ggtitle("Theoretical Cumulative Distributions Of AOD, DOA, And D20 Rolls: % Of Rolls >= The Index N") + 
  theme(
    plot.title = element_text(color = "black", size = 12, face = "bold")
  ) + 
  guides(fill=TRUE) +
  labs(y= "% Of Rolls", x = "D20 Roll Values", color = "Legend", 
    caption = "For a given index's point on the x-axis, the colored line whose lower step intersects that line
       above the other two represents the distribution with the highest % of rolls equal to or above the value of that index, 
       and therefore represents the distribtion with the highest probability of rolling at or above that index.")+
  scale_color_manual(values = colors)

########################
#APPENDIX
###################

#Lame, base R plot method: 
#And we plot with a little help from this walkthrough: 
#https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/

hg1 <- hist(firstpass, breaks = pretty(0:20, n = 22), plot = FALSE)
hg2 <- hist(secondpass, breaks = pretty(0:20, n = 22), plot = FALSE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(hg1, col = c1, main = "Comparing Histograms of 100,000 Simulations Each", xlab = "D20 Results")
plot(hg2, col = c2, add = TRUE)

#And this code, I could not get to work: 

countsdis <- function(i) {
  j <- i - 1
  #First we compute the weights by enumerating the outcomes (we divide by 400 later)
  counts <- (20 - j) + (20 - j) - 1
  return(counts)
}
discounts <- sapply(1:20, countsdis)
discounts
sum(discounts)

AODgrid <- expand.grid(discounts, discounts)
AODgrid$maxVal <- apply(AODgrid, 1, function(x) max(x))
AODgrid$counts <- AODgrid$Var1 * AODgrid$Var2
View(AODgrid)
#Want 160,000: 
sum(AODgrid$counts)
#AODgrid[which(AODgrid$maxVal==33), 4]

compexaod <- function(i) {
  #First we compute the weights by enumerating the outcomes (we divide by 160,000 later)
  #Note that we can use the discounts weights as a proxy for the roll value: 
  myindex <- discounts[[i]]
  thissum <- sum(AODgrid[which(AODgrid$maxVal==myindex), 4])
  #and we go ahead and weight it: 
  weighted <- thissum * i
  return(weighted)
}

counts_AOD <- sapply(1:20, compexaod)
counts_AOD

ExpecAOD <- sum(counts_AOD)/160000
ExpecAOD


