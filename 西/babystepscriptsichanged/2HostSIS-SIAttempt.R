## script based on my previous script
## basically adding the previous equations that we will be using
## and playing around with implementing them
## also, let's do this one on a weekly scale rather than
## monthly or daily changes
# biting rate * 7, inf probability is =, 
# birth and mortality rate * 7
library(deSolve)
library(ggplot2)

#the 2 base non-ODE models
I.Mosq.Mod <- function(Biting.RatepDay, pBiteHtoMIProb, I.Hum.Pop, S.Mosq.Pop, Tot.Hum.Pop) {
  I.Mosqs <- ((Biting.RatepDay*pBiteHtoMIProb*I.Hum.Pop*S.Mosq.Pop)/Tot.Hum.Pop)
  return(c(I.Mosqs))
}

I.Hum.Mod <- function(Biting.RatepDay, pBiteMtoHIProb, I.Mosq.Pop, S.Hum.Pop, Tot.Hum.Pop) {
  I.Hums <- (Biting.RatepDay*pBiteMtoHIProb*I.Mosq.Pop*(S.Hum.Pop/Tot.Hum.Pop))
  return(c(I.Hums))
}

# parameters
Mos2HumRatio <- 3
Tot.Hum.Pop <- 2000000 #2mil
Tot.Mos.Pop <- Mos2HumRatio*Tot.Hum.Pop #3 mosquitoes per Hum (likely to change)
Mean.Initial.Sporozite.Rate <- 0.031 #avg % of mos initially infected

#note: only female mosquitoes can transmit malaria

#non-temp dependent toy values
biting.rate.p.day <- 0.24

#temp dependent parameters to actually use to test
Biting.Rate.p.DaybyTemp <- function(Temp) {
  Bite.Rate <- (-0.170+Temp*0.0167)
  return(c(Bite.Rate))
}
Vector.Competence.bTemp <- function(Temp) {
  pBitetoIProb <- (-0.00360*Temp^2+0.1786*Temp-1.62)
  return(c(pBitetoIProb))
}
SimpleMLifeSpanvTempM <- function(Temp) {
  MLifeSpan <- (1.461899-0.03151*Temp)
  return(c(MLifeSpan)) #maybe make this mortality probability based on day?)
}
# ^ how do I turn this equation into mortality rate?
#Shapiro 2017 has an equation for this

#what factors will impact mosquito demography
# + are directly tied to temperature?
#num. of eggs laid, juvenile development rate
#adult mortality rate, % of eggs that hatch
#blood feeding may be measured as well/has a reported
#effect on survival of adult females depending on temps
#vertical transmission rates? from mom -> offspring
#which will create mosquitoes infected upon birth
#probabilities of egg laying depending on
#which clutch # it is
Mos.Birth.RatebDay <- function(Temp) {
  MosBirthpDay <- 1*2
  return(c(MosBirthpDay))
}
EggsLaidpClutchbTemp <- function(Temp) {
  EggsLaidpClutch <- 1*2
  return(c(EggsLaidpClutchbTemp))
}
  
PropSurvivingEggsbyTemp <- function(Temp) {
  PropSurvivingEggs <- 1*2
  return(c(PropSurvivingEggs))
}

# consider adding proportion of females existing in
#stasis depending on temperature conditions

