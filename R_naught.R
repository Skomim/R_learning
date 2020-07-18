install.packages("R0")
#Loading package
library(MASS)
library(R0)

##### Germany data ################

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany
data(Germany.1918)
Germany.1918
str(Germany.1918)
## check.incid will extract names from the vector and coerce them as dates
check.incid(Germany.1918)
## Had Germany.1918 not have names() set, output would have been with index dates
## To force such an output, we here impose t=1:126.
## Erasing names(Germany.1918) would have produced the same
## If so, then the epid$t vector returned will be replacement values.
check.incid(Germany.1918, t=1:126)
## You can also choose not to provide a complete date vector, but to only
## indicated the first day of the observation, and the number of days between each
## observation. In this example we will assume a time step of 7 days.
check.incid(Germany.1918, date.first.obs="1918-01-01", time.step=7)
## Finally, if no names() are available for the dataset and date.first.obs is not provided,
## setting time.step to any integer value will generate a t vector starting
## from 1 and incrementing by the time.step parameter.


######## H1N1 ###################
# Data taken from traced cases of H1N1 viruses.
data(H1N1.serial.interval)
est.GT(serial.interval=H1N1.serial.interval)

# The same result can be achieved with two vectors of dates of onset.
# Here we use the same data, but trick the function into thinking onset dates are all "0".
data(H1N1.serial.interval)
est.GT(infector.onset.dates=rep(0,length(H1N1.serial.interval)),
       infectee.onset.dates=H1N1.serial.interval)


##### H2N2 #####################
## Woodall reported an attack rate of 0.31 in a population of 1732 during
## the 1957 H2N2 influenza pandemic ('Age and Asian Influenza, 1957', BMJ, 1958)
est.R0.AR(pop.size=1732, AR=0.31)
# Reproduction number estimate using Attack Rate method
# R : 1.19698[ 1.179606 , 1.215077 ]
est.R0.AR(AR=0.31)
# Reproduction number estimate using Attack Rate method.
# R : 1.19698
est.R0.AR(pop.size=1732, incid=31)
# Reproduction number estimate using Attack Rate method
# R : 1.009057[ 1.005873 , 1.012269 ]
est.R0.AR(pop.size=1732, incid=c(2,3,4,7,4,2,4,5))
# Reproduction number estimate using Attack Rate method
# R : 1.009057[ 1.005873 , 1.012269 ]
est.R0.AR(pop.size=1732, incid=c(2,3,0,7,4,2,0,5))
# Reproduction number estimate using Attack Rate method
# R : 1.006699[ 1.003965 , 1.009453 ]

##### Estimate R from exponential growth #############
data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))
est.R0.EG(Germany.1918, mGT, begin=1, end=27)
## Reproduction number estimate using Exponential Growth
## R : 1.525895[ 1.494984 , 1.557779 ]

##### Estimate the reproduction number by maximum likelihood ###########
## Data is taken from paper by Nishiura for key transmission parameters of an institutional
## outbreak during the 1918 influenza pandemic in Germany)
data(Germany.1918)
mGT<-generation.time("gamma", c(2.45, 1.38))
est.R0.ML(Germany.1918, mGT, begin=1, end=27, range=c(0.01,50))
# Reproduction number estimate using Maximum Likelihood method.
# R : 1.307222[ 1.236913 , 1.380156 ]
res=est.R0.ML(Germany.1918, mGT, begin=1, end=27, range=c(0.01,50))
plot(res)
## no change in R with varying range
## (dates here are the same index as before. Just to illustrate different use)
est.R0.ML(Germany.1918, mGT, begin="1918-09-29", end="1918-10-25", range=c(0.01,100))
# Reproduction number estimate using Maximum Likelihood method.
# R : 1.307249[ 1.236913 , 1.380185 ]

##### Estimate the time dependent reproduction number using a Bayesian approach #########
## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)
data(Germany.1918)
mGT <- generation.time("gamma", c(3,1.5))
SB <- est.R0.SB(Germany.1918, mGT)
## Results will include "most likely R(t)" (ie. the R(t) value for which the computed probability
## is the highest), along with 95% CI, in a data.frame object
SB
# Reproduction number estimate using Real Time Bayesian method.
# 0 0 2.02 0.71 1.17 1.7 1.36 1.53 1.28 1.43 ...
SB$Rt.quant
# Date R.t. CI.lower. CI.upper.

## "Plot" will provide the most-likely R value at each time unit, along with 95CI
plot(SB)
## "Plotfit" will show the complete distribution of R for 9 time unit throughout the outbreak
plotfit(SB)

##### Estimate the time dependent reproduction number ######
## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)
data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))
TD <- est.R0.TD(Germany.1918, mGT, begin=1, end=126, nsim=100)
# Warning messages:
# 1: In est.R0.TD(Germany.1918, mGT) : Simulations may take several minutes.
# 2: In est.R0.TD(Germany.1918, mGT) : Using initial incidence as initial number of cases.
TD
# Reproduction number estimate using Time-Dependent method.
# 2.322239 2.272013 1.998474 1.843703 2.019297 1.867488 1.644993 1.553265 1.553317 1.601317 ...
## An interesting way to look at these results is to agregate initial data by longest time unit,
## such as weekly incidence. This gives a global overview of the epidemic.
TD.weekly <- smooth.Rt(TD, 7)
TD.weekly
# Reproduction number estimate using Time-Dependant method.
# 1.878424 1.580976 1.356918 1.131633 0.9615463 0.8118902 0.8045254 0.8395747 0.8542518 0.8258094..
plot(TD.weekly)

##### Estimate R0 for one incidence dataset using several methods ######
