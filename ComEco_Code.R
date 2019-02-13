#Raymond Looney
# Graduate Presentation Community Ecology: Exctinction Risk of Vermont Amphibians
# 13 February, 2019


install.packages(c("tidyr", "devtools"))
library(tidyr)
library(devtools)
library(dplyr)
library(ggplot2)

data <- read.csv("DriftFence.csv")




names(data)
df<-subset(data, select=c("Year","Count","Common.name","County"))
head(df)


df <- aggregate(df$Count, list(df$Year, df$Common.name), sum)
colnames(df)[colnames(df)=="Group.1"] <- "Year"
colnames(df)[colnames(df)=="Group.2"] <- "Species"
colnames(df)[colnames(df)=="x"] <- "Count"
head(df)

 df<- df %>%
  filter(Species != "Blue-spotted Salamander group") %>%
  filter(Species != "Common Gartersnake") %>%
  filter(Species != "Gray Treefrog") %>%
  filter(Species != "Northern Dusky Salamander") %>%
  filter(Species != "Northern Two-lined Salamander") %>%
  filter(Species != "Nothing found") %>%
  filter(Species != "Pickerel Frog") %>%
  filter(Species != "Spring Salamander") %>%
  filter(Species != "Unidentified egg mass") %>%
  filter(Species != "Unidentified salamander")
  



#------------------------------------------------------------

#Plot Count by Year for each species
# I want to find counts for each speces for each year???
ggplot(data = df, mapping = aes(x = Year, y = Count, color = Species)) +
  geom_line() 

ggplot(data = df, mapping = aes(x = Year, y = Count,color=Species)) +
  geom_line() +
  facet_wrap(~Species)


#-------------------------------------------------------------------------------------------
#American Toad

#READ DATA
A_Toad<- read.csv("American_Toad.csv",fileEncoding="UTF-8-BOM")
head(A_Toad)


#PLOT THE DATA
ggplot(data= A_Toad, mapping= aes(x=Year,y=Count,color="red")) +
         geom_line()

#CALCULATE LAMBDA FROM COUNTS

counts=A_Toad$Count
Al = counts[-1]/counts[-length(counts)]
round(Al,2)

#HISTOGRAM OF LAMBDAS
hist(Al,breaks=30,main="American Toad Lambdas")


#EXTRACT MEAN AND STDEV

mean(log(Al)) #0.096
# very close to 0, but slightly above it = pop is stable @ the moment, but variance is large

sd(log(Al)) #1.27

set.seed(264)       
sim.Al = rlnorm(50,meanlog=mean(log(Al)),sdlog = sd(log(Al)))
round(sim.Al,2)


#PROJECTING POPULATION FOR 20 YEARS

#single simulation (2018-2038)
set.seed(50)
time=21
N0=22
N=vector(length=time)
N[1]=N0
sim.Al = rlnorm(time,meanlog = mean(log(Al)),sdlog=sd(log(Al)))
for (t in 2:time) {
  N[t] = N[t - 1] * sim.Al[t - 1]
}
par(mar=c(4,4,1,4))
plot(1:(time), N, type = "o", las = 1, xaxt = "n")
axis(side=1,at=c(1,6,11,16,20),labels=c(2018,2024,2028,2034,2038))     

#MULTIPLE SIMULATIONS

set.seed(50)
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
                                                               250), ylab = "N", xaxt = "n", xlab = "Year")
axis(side = 1, at = c(1, 6, 11, 16, 20), labels = c(2018, 2024, 
                                                    2028, 2034, 2038))

#WHAT IS THE EXTINCTION RISK

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

# which columns have at least one value less than 2?

minpop = apply(outmat, 2, function(x) min(x) < 2)
sum(minpop + 0)/sims  #proportion of columns with TRUE

#64% Chance population will go extinct within 100 years


#------------------------------------------------------------------------------------------
# Eastern Newt
