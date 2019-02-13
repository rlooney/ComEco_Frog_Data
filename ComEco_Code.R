#Getting the data I want

data <- read.csv("DriftFence.csv")

install.packages(c("tidyr", "devtools"))
library(tidyr)
library(devtools)
library(ggplot2)


names(data)
df<-subset(data, select=c("Year","Count","Common.name","County"))
head(df)
df


df <- aggregate(df$Count, list(df$Year, df$Common.name), sum)
colnames(df)[colnames(df)=="Group.1"] <- "Year"
colnames(df)[colnames(df)=="Group.2"] <- "Species"
colnames(df)[colnames(df)=="x"] <- "Count"
head(df)
df1 <- data.frame("var1"=df$Species=="American Toad",
                  "var2"=df$Year,
                  "var3"=df$Count)
#------------------------------------------------------------

#Plot Count by Year for each species
# I want to find counts for each speces for each year???

ggplot(data = df, mapping = aes(x = Year, y = Count, color = Species)) +
  geom_line()
ggplot(data = df, mapping = aes(x = Year, y = Count,color=Species)) +
  geom_line() +
  facet_wrap(~Species)


#----------------------------------------------------------------
# AMERICAN TOAD
Am_toad <- read.csv("American_Toad.csv", fileEncoding="UTF-8-BOM")

#PLOT THE DATA
Aplot<- plot( Count ~ Year, data = Am_toad,  type = "o", las = 1)

#Calculating lambda from counts & histogram of lambdas
counts = Am_toad$Count
Al = counts[-1]/counts[-length(counts)]
round(Al, 2)
hist(Al, breaks = 20, main = "American Toad Histogram of lambdas")

#Extract the mean and stdev of ln(lambda).
mean(log(Al)) #NOTICE: r is close to 0, but slightly above:
  #pop is stable, but variance is large)
sd(log(Al))
 
#Generate 50 projected lambda values:
set.seed(2)
sim.Al = rlnorm(50,meanlog = mean(log(Al)), sdlog=sd(log(Al)))
round(sim.Al,2)

#Projecting population for over 20 years
  #Single Simulation


time = 20
N0 = 22
N = vector(length = time)
N[1] = N0
sim.Al = rlnorm(time, meanlog = mean(log(Al)), sdlog = sd(log(Al)))
for (t in 2:time) {
  N[t] = N[t - 1] * sim.Al[t - 1]
}
par(mar = c(4, 4, 1, 4))
plot(1:(time), N, type = "o", las = 1, xaxt = "n")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2018, 2023, 
                                                    2028, 2033, 2037))






set.seed(2)
sims = 5
outmat = sapply(1:sims, function(x) {
  time = 21
  N0 = 22
  N = vector(length = time)
  N[1] = N0
  sim.Al = rlnorm(time, meanlog = mean(log(Al)), sdlog = sd(log(Al)))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.Al[t - 1]
  }
  N
})
par(mar = c(4, 4, 1, 4))
matplot(1:time, outmat, type = "l", las = 1, lty = 5, ylim = c(0, 
                                                               200), ylab = "N", xaxt = "n", xlab = "Year")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2003, 2008, 
                                                    2013, 2018, 2023))


set.seed(2)
sims = 1000
outmat = sapply(1:sims, function(x) {
  time = 101
  N0 = 22
  N = vector(length = time)
  N[1] = N0
  sim.Al = rlnorm(time, meanlog = mean(log(Al)), sdlog = sd(log(Al)))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.Al[t - 1]
  }
  N
})

dim(outmat)
minpop = apply(outmat, 2, function(x) min(x) < 2)
sum(minpop + 0)/sims  #proportion of columns with TRUE\
#68% chance pop will go extinct within 100 years due purely to stochasticity

#----------------------------------------------------------------------\
#EASTERN NEWT
E_Newt<- read.csv("Eastern_Newt.csv",fileEncoding="UTF-8-BOM")

#PLOT THE DATA
Eplot<- plot( Count ~ Year, data = E_Newt,  type = "o", las = 1)

#Calculating lambda from counts & histogram of lambdas
counts = E_Newt$Count
El = counts[-1]/counts[-length(counts)]
round(El, 2)
hist(El, breaks = 20, main = "Eastern Newt Histogram of lambdas")

#Extract the mean and stdev of ln(lambda).
mean(log(El)) #NOTICE: r is close to 0, but slightly above:
#pop is stable, but variance is large)
sd(log(El))

#Generate 50 projected lambda values:
set.seed(2)
sim.El = rlnorm(50,meanlog = mean(log(El)), sdlog=sd(log(El)))
round(sim.El,2)

#Projecting population for over 20 years
#Single Simulation


time = 20
N0 = 29
N = vector(length = time)
N[1] = N0
sim.El = rlnorm(time, meanlog = mean(log(El)), sdlog = sd(log(El)))
for (t in 2:time) {
  N[t] = N[t - 1] * sim.El[t - 1]
}
par(mar = c(4, 4, 1, 4))
E<- plot(1:(time), N, type = "o", las = 1, xaxt = "n")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2018, 2023, 
                                                    2028, 2033, 2037))






set.seed(2)
sims = 5
outmat = sapply(1:sims, function(x) {
  time = 21
  N0 = 29
  N = vector(length = time)
  N[1] = N0
  sim.El = rlnorm(time, meanlog = mean(log(El)), sdlog = sd(log(El)))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.El[t - 1]
  }
  N
})
par(mar = c(4, 4, 1, 4))
matplot(1:time, outmat, type = "l", las = 1, lty = 5, ylim = c(0, 200), ylab = "N", 
        xaxt = "n", xlab = "Year")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2003, 2008, 
                                                    2013, 2018, 2023))


set.seed(2)
sims = 1000
outmat = sapply(1:sims, function(x) {
  time = 101
  N0 = 43
  N = vector(length = time)
  N[1] = N0
  sim.Al = rlnorm(time, meanlog = mean(log(Al)), sdlog = sd(log(Al)))
  for (t in 2:time) {
    N[t] = N[t - 1] * sim.Al[t - 1]
  }
  N
})

dim(outmat)
minpop = apply(outmat, 2, function(x) min(x) < 2)
sum(minpop + 0)/sims  #proportion of columns with TRUE\
#68% chance pop will go extinct within 100 years due purely to stochasticity