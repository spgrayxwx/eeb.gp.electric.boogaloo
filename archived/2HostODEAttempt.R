##pure gold
##this will *not* be elegant
library(deSolve)
library(ggplot2)
library(tidyr)

##lesjustsay peanits
# as per line 33ish, it assumes a virgin pop, but
# does the math work out for that? can this be avoided?
# does it really assume a virgin pop? how do we assume
# initial susceptible human numbers?

#using 2011 temps of Nigeria from climate knowledge portal
DayTemp <- c(1, 2, 3)
DayTemp[0:30] <- c(24.63)
DayTemp[31:61] <- c(28.71)
DayTemp[62:93] <- c(30.33)
DayTemp[94:124] <- c(30.55)
DayTemp[125:156] <- c(29.54)
DayTemp[157:187] <- c(27.80)
DayTemp[188:219] <- c(26.41)
DayTemp[220:250] <- c(25.39)
DayTemp[251:282] <- c(25.89)
DayTemp[283:313] <- c(26.70)
DayTemp[314:345] <- c(26.23)
DayTemp[345:366] <- c(28) #sorry but December is just really short :/

## temp-dependent equations for parameters
#aka BiteRatePerDay of Mos -> Hum
DailyBiteRatebyTemp <- function(Temp) {
  BRPD <- (-0.170+Temp*0.0167)
  return(c(BRPD)) #bite rate p day
}

#aka chance of a bite resulting in infection
Vector.Competence.bTemp <- function(Temp) {
  ProbB2I <- (-0.00360*Temp^2+0.1786*Temp-1.62)
  return(c(ProbB2I)) #probability of bite -> infection
}

#Initial.Mos.Infection.Rate
InitMosIR = 0.0314 #based on literature
# Total Hum Pop
Total.Humans = 2000000
Initial.Total.Mosquitoes = Total.Humans*3
Initial.Inf.Mosquitoes = 3*Total.Humans*InitMosIR
Initial.Sus.Mosquitoes = Initial.Total.Mosquitoes - Initial.Inf.Mosquitoes
Initial.Inf.Humans = DailyBiteRatebyTemp(DayTemp[1]) * Vector.Competence.bTemp(DayTemp[1]) * 
  Initial.Inf.Mosquitoes * 10 #do I divide by total humans? again, I am stupid.
Initial.Sus.Humans = Total.Humans - Initial.Inf.Humans

#this is bc im bad btw
BiteRate = DailyBiteRatebyTemp(DayTemp)
VectorCompetence = Vector.Competence.bTemp(DayTemp)
Total.Humans = Total.Humans
Total.Mosquitoes = Initial.Total.Mosquitoes
Inf.Mosquitoes = Initial.Inf.Mosquitoes
Sus.Mosquitoes = Initial.Sus.Mosquitoes
Inf.Humans = Initial.Inf.Humans
Sus.Humans = Initial.Sus.Humans

parameters <- c(
  Total.Humans = Total.Humans # restating bc i'm dumb
)

state <- c(
  Inf.Mosquitoes = Initial.Inf.Mosquitoes,
  Sus.Mosquitoes = Initial.Sus.Mosquitoes,
  Inf.Humans = Initial.Inf.Humans,
  Sus.Humans = Initial.Sus.Humans
)

# it's ODEing time 8) help
times <- seq(from = 0, to = 365, by = 1)

TimenTemp <- data.frame(times, DayTemp)
print(TimenTemp) #make sure they're =

gamma <- 1/92
muM <- 1/14

Human.Mosquito.SIS.SI.Model <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    Temp_t <- DayTemp[round(t) + 1]
    
    BiteRate_t <- DailyBiteRatebyTemp(Temp_t)
    VectorCompetence_t <- Vector.Competence.bTemp(Temp_t)
    
    NM <- Sus.Mosquitoes + Inf.Mosquitoes
    NH <- Sus.Humans + Inf.Humans
    
    lambda_M <- BiteRate_t * VectorCompetence_t * Inf.Humans / NH
    lambda_H <- BiteRate_t * VectorCompetence_t * Inf.Mosquitoes / NH
    
    #toy mosquito pop replacement
    birthsM <- muM * NM
    deaths.SM <- muM * Sus.Mosquitoes
    deaths.IM <- muM * Inf.Mosquitoes
    
    dSM <-  birthsM - lambda_M * Sus.Mosquitoes - deaths.SM
    dIM <-  lambda_M * Sus.Mosquitoes - deaths.IM
    
    dSH <- -lambda_H * Sus.Humans + gamma * Inf.Humans
    dIH <-  lambda_H * Sus.Humans - gamma * Inf.Humans
    
    return(list(c(dSM, dIM, dSH, dIH)))
  })
}

output <- ode(y = state, times = times, func = Human.Mosquito.SIS.SI.Model, parms = parameters)

output_df <- as.data.frame(output)
output_df$Temp <- DayTemp[output_df$time + 1]

output_long <- pivot_longer(output_df, cols = -time, names_to = "Compartment", values_to = "Population")
  ggplot(output_long, aes(x = time, y = Population, color = Compartment)) +
  geom_line() +
  theme_minimal()