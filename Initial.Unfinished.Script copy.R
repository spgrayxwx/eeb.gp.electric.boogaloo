library(deSolve)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(GGally)
library(caTools)
#For this trial, we will use the year 2000 in Nigeria
print(Ni.IPR.by.Year[1, ])
#Data for years 2000-2024 available
#We will use the Incidence.per.1000 to set the initial
#human infection rate and the ATv27.3 to adjust the
#general monthly air temperature mean to the annual
#air temperature mean
Monthly.Avg.Incidence <- c(NA)
Monthly.Avg.Incidence[1:12] <- c(11057)
Monthly.Avg.Incidence[1] <- (Ni.IPR.by.Year[1, 2]/4000)

NG2000Test <- data.frame('Avg Temp/Month', 'Biting Rate',
                         'Per-Bite Transmission Rate',
                         Monthly.Avg.Incidence)
print(Ni.IPR.by.Year[1, 4]) #then, use output in previous line
NG2000Test$X.Avg.Temp.Month. <- NG.Observed.Monthly.Mean.Air.Temp[1:12]*0.9863063

#Adjust first temp dependent variables
NG2000Test$X.Biting.Rate. <- Biting.RatepDaypTemp(NG2000Test$X.Avg.Temp.Month.)
NG2000Test$X.Per.Bite.Transmission.Rate. <- Vector.CompetencepTemp(NG2000Test$X.Avg.Temp.Month.)

#Initial tot human pop set, find inf and sus numbers
NG2000Test$Inf.Hum.Pop <- Monthly.Avg.Incidence[1]*Tot.Hum.Pop
NG2000Test$Sus.Hum..Pop <- Tot.Hum.Pop-NG2000Test$Inf.Hum.Pop
Mos2HumRatio <- 3
Tot.Mos..Pop <- Tot.Hum.Pop*Mos2HumRatio

#Get initial inf and sus mosquito pop
NG2000Test$Inf.Mos.Pop <- Tot.Mos..Pop*Mean.Sporozoite.Rate
NG2000Test$Sus.Mos.Pop <- Tot.Mos..Pop-NG2000Test$Inf.Mos.Pop

#ok so rn I.Hum.Pop output is .1 of predictions
#I thought it worked earlier but rn Imma prop it

NG2000Test$Inf.Hum.Pop <- 


#delete below btw
print(Ni.IPR.by.Year[1, 4]) #then, use output in previous line
NG2000Test$X.Avg.Temp.Month. <- NG.Observed.Monthly.Mean.Air.Temp[1:12]*0.9863063

I.Hum.Modd <- function(Biting.RatepDay, pBiteMtoHIProb, I.Mos.Pop, S.Hum.Pop, Tot.Hum.Pop) {
  I.Hums <- ((Biting.RatepDay*pBiteMtoHIProb*I.Mos.Pop*S.Hum.Pop)/(0.1*Tot.Hum.Pop))
  return(c(I.Hums))
}

#For Loop
for (Inf.Hum.Pop in 2:12) {
  if(NG2000Test[1] != NG2000Test[i-1]){
    print(1)
  }else{
    print(0)
  }
}

#for (i in 2:nrow(NG2000Test)) {
  #prev = NG2000Test[i-1, ]
  #NG2000Test$Inf.Hum.Pop <- I.Hum.Modd(prev$X.Biting.Rate., prev$X.Per.Bite.Transmission.Rate.
    #                                   prev$Inf.Mos.Pop, prev$Sus.Hum..Pop, Prev$Tot.Hum.Pop)
  #NG2000Test$Inf.Mos.Pop <- I.Mosq.Modd(prev$X.Biting.Rate., prev$X.Per.Bite.Transmission.Rate.
   #                                    prev$Inf.Hum.Pop, prev$Sus.Mos.Pop, prev$Tot.Hum.Pop)
  #
#}
  
  #I.Hum.Modd(NG2000Test$X.Biting.Rate., NG2000Test$X.Per.Bite.Transmission.Rate., NG2000Test$Inf.Mos.Pop,
            # NG2000Test$Sus.Hum..Pop, NG2000Test$Tot.Hum.Pop)
 # I.Mosq.Modd(NG2000Test$X.Biting.Rate., NG2000Test$X.Per.Bite.Transmission.Rate., NG2000Test$Inf.Hum.Pop,
             #NG2000Test$Sus.Mos.Pop, prev$Tot.Hum.Pop) 
#}

for (i in 2:12) {
  print(c(Puppy))
}

