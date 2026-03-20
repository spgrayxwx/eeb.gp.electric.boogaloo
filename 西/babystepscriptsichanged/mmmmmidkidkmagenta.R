#hi imagine temp stays the same
# so, we will change the mosquito
# 


# coding birthrate
# Briere 1 model: r(T) = q * T * (T - Tmin) * sqrt(Tmax - T), for Tmin < T < Tmax
# Example check: briere(28, 0.18, 25, 31)
briere <- function(temp, q_val, tmin, tmax) {
  if (temp <= tmin || temp >= tmax) {
    return(0)} else {
      return(q_val * temp * (temp - tmin) * sqrt(tmax - temp))
    }
}
t_min=25
t_max=31
briere(28, 0.18, 25, 31)
rm(list = ls())
q_val <- as.numeric(0.18)
temp <- as.numeric(28)
tmin <- as.numeric(25)
tmax <- as.numeric(31)
print(q_val)
print(temp)
print(tmin)
print(tmax)
print(class(q_val))
print(class(temp))
print(class(tmin))
print(class(tmax))
birth_rate <- q_val * temp * (temp - tmin) * sqrt(tmax - temp)
print(birth_rate)
library(rTPC)
library(deSolve)
library(ggplot2)
library(tidyr)
#define temperatures and parameters
temp_values <- seq(0,40,by=1)
tmin_val <- 25
tmax_val <- 31
a_val <- 0.0001
b_val <- 0.5 #Corresponds to 1/2 in the Briere 1 model
#calculate development rates
# Calling the Briere 2 function from the rTPC package.
# We name every argument (tmin=, tmax=, a=, b=) so R matches them correctly
# regardless of the order they appear in the function definition.
rates <- briere2_1999(temp=temp_values, tmin=tmin_val, tmax=tmax_val, a=a_val, b=b_val)
#View results (first few)
head(rates)
##lesjustsay esnoopy from the peanits
# as per line 33ish, it assumes a virgin pop, but
# does the math work out for that? can this be avoided?
# does it really assume a virgin pop? how do we assume
# initial susceptible human numbers?
#using 2011 temps of Nigeria from climate knowledge portal
# CHANGE from draft: draft used DayTemp <- c(1,2,3) and DayTemp[0:30] (buggy).
#   R ignores index 0 in assignment, so [0:30] only fills indices 1-30 (January lost a day).
#   November [314:345] and December [345:366] also overlapped at index 345, overwriting one day.
# FIX: pre-allocate 366 zeros with numeric(366), then fill using correct 1-based month ranges.
# numeric(366) creates a vector of 366 zeros -- one slot per day (day 0 through day 365)
# We then fill each slot with the average monthly temperature for that month.
# The bracket notation [1:31] means "indices 1 through 31" (R counts from 1, not 0).
# Each monthly temperature is repeated across every day of that month.
DayTemp <- numeric(366)
DayTemp[1:31]   <- c(24.63)  # January   (31 days)
DayTemp[32:59]  <- c(28.71)  # February  (28 days, 2011 not a leap year)
DayTemp[60:90]  <- c(30.33)  # March     (31 days)
DayTemp[91:120] <- c(30.55)  # April     (30 days)
DayTemp[121:151]<- c(29.54)  # May       (31 days)
DayTemp[152:181]<- c(27.80)  # June      (30 days)
DayTemp[182:212]<- c(26.41)  # July      (31 days)
DayTemp[213:243]<- c(25.39)  # August    (31 days)
DayTemp[244:273]<- c(25.89)  # September (30 days)
DayTemp[274:304]<- c(26.70)  # October   (31 days)
DayTemp[305:334]<- c(26.23)  # November  (30 days)
DayTemp[335:366]<- c(28)     # December  (32 entries to fill the remaining slots)
times <- seq(from = 0, to = 365, by = 1)
# CHANGE from draft: draft used DayTemp[round(t)+1] directly inside the ODE function.
#   ODE solvers take fractional time steps (e.g. t=1.3), so rounding can cause sudden
#   temperature jumps that confuse the solver and produce inaccurate results.
# FIX: approxfun() builds a smooth interpolation function from the times/DayTemp vectors.
#   rule=2 means: if the solver asks for a time outside 0-365, return the nearest edge value.
TempFun <- approxfun(times, DayTemp, rule = 2)
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
#Temp dependent mosquito life span equation (linear fit over shorter scale)
SimpleMMortalityRatevTempM <- function(Temp) {
  MMortalitypDay <- (1/(44.44 + -0.943852*Temp))
  return(c(MMortalitypDay))
} #let's instead replace this with some
# kind of mortality rate

# Temperature-dependent mosquito birth rate using the Briere 1 thermal model.
# The Briere 1 formula is: r(T) = q * T * (T - Tmin) * sqrt(Tmax - T)
#   - T    : current temperature (degrees C)
#   - q    : a fitted scaling constant that controls the peak rate
#   - Tmin : minimum temperature below which mosquitoes cannot reproduce
#   - Tmax : maximum temperature above which mosquitoes cannot reproduce
#
# Species: Anopheles gambiae
# Parameters from Mordecai et al. 2013 (egg-to-adult development rate proxy):
#   q=0.000111, Tmin=14.7, Tmax=34.0
# Adjust q to scale to per-capita birth rate as needed.
MosquitoBirthRatebyTemp <- function(Temp) {
  q    <- 0.000111
  tmin <- 14.7
  tmax <- 34.0
  # Outside the viable temperature range the birth rate is zero
  if (Temp <= tmin || Temp >= tmax) {
    return(0)
  } else {
    # Inside the range, apply the Briere 1 formula
    return(q * Temp * (Temp - tmin) * sqrt(tmax - Temp))
  }
}
#Initial.Mos.Infection.Rate
InitMosIR = 0.0314 #based on literature
# Total Hum Pop
scaleH <- 1e6
scaleM <- 1e6
# PCC = post chat change
# PCC changing # of mosquitoes so that dynamics are less aggressive
Total.Humans = 2
Initial.Total.Mosquitoes = 2 # PCC Total.Humans*1
Initial.Inf.Mosquitoes = 2 * InitMosIR
Initial.Sus.Mosquitoes = Initial.Total.Mosquitoes - Initial.Inf.Mosquitoes
Initial.Inf.Humans = 0.1 * Total.Humans #for now
Initial.Sus.Humans = Total.Humans - Initial.Inf.Humans
#this is bc im bad btw
BiteRate = DailyBiteRatebyTemp(DayTemp)
VectorCompetence = Vector.Competence.bTemp(DayTemp)
beta = 0.18 # infection probability
Total.Humans = Total.Humans
Total.Mosquitoes = Initial.Total.Mosquitoes
Inf.Mosquitoes = Initial.Inf.Mosquitoes
Sus.Mosquitoes = Initial.Sus.Mosquitoes
Inf.Humans = Initial.Inf.Humans
Sus.Humans = Initial.Sus.Humans
parameters <- c(
  Total.Humans = Total.Humans, # restating bc im dumb
  Total.Mosquitoes = Initial.Total.Mosquitoes
)
state <- c(
  Inf.Mosquitoes = Initial.Inf.Mosquitoes,
  Sus.Mosquitoes = Initial.Sus.Mosquitoes,
  Inf.Humans = Initial.Inf.Humans,
  Sus.Humans = Initial.Sus.Humans
)
# its ODEing time 8) help
TimenTemp <- data.frame(times, DayTemp)
print(TimenTemp) #make sure they are equal
#PCC gamma attempt change
gamma <- 1/92
muM <- SimpleMMortalityRatevTempM(Temp_t)
Human.Mosquito.SIS.SI.Model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    Temp_t <- c(24)
    BiteRate_t <- DailyBiteRatebyTemp(Temp_t)
    VectorCompetence_t <- Vector.Competence.bTemp(Temp_t)
    NM <- Sus.Mosquitoes + Inf.Mosquitoes
    NH <- Sus.Humans + Inf.Humans
    # CHANGE from draft: draft was missing beta (infection probability) from both lambdas.
    #   Without beta, a single bite always causes infection regardless of pathogen biology.
    # FIX: multiply by beta (set to 0.18 above) to represent the probability that an
    #   infectious bite actually results in a new infection.
    lambda_M <- BiteRate_t * VectorCompetence_t * beta * Inf.Humans / NH
    lambda_H <- BiteRate_t * VectorCompetence_t * beta * Inf.Mosquitoes / NH
    # FIX: births now use MosquitoBirthRatebyTemp() -- a Briere 1 curve for A. gambiae
    #   that rises and falls with temperature (peaks ~28C, zero outside 14.7-34C).
    #   Deaths now use SimpleMLifeSpanvTempM() -- a linear fit so mosquitoes die faster
    #   at higher temperatures, also consistent with A. gambiae biology.
    # Temperature-dependent birth rate: the Briere function gives a per-capita rate,
    # so we multiply by NM (total mosquito population) to get the total number of
    # new mosquitoes born this time step. All newborns enter the Susceptible class.
    birthsM <- MosquitoBirthRatebyTemp(Temp_t) * NM
    deaths.SM <- 1.775*muM * Sus.Mosquitoes
    deaths.IM <- 1.775*muM * Inf.Mosquitoes
    dSM <-  birthsM - lambda_M * Sus.Mosquitoes - deaths.SM
    dIM <-  lambda_M * Sus.Mosquitoes - deaths.IM
    dSH <- -lambda_H * Sus.Humans + gamma * Inf.Humans
    dIH <-  lambda_H * Sus.Humans - gamma * Inf.Humans
    # CHANGE from draft: draft returned c(dSM, dIM, dSH, dIH) but state was defined as
    #   c(Inf.Mosquitoes, Sus.Mosquitoes, Inf.Humans, Sus.Humans).
    #   deSolve silently matches derivatives to state slots by POSITION, not name, so the
    #   wrong derivatives were being applied to the wrong compartments -- a silent bug.
    # FIX: return order matches state order exactly: Inf.Mos, Sus.Mos, Inf.Hum, Sus.Hum.
    return(list(c(dIM, dSM, dIH, dSH)))
  })
}
# CHANGE from draft: draft used ode() with no method specified (defaults to lsoda).
#   lsoda can struggle with "stiff" systems -- equations where some compartments change
#   much faster than others (common in disease models with very different time scales).
# FIX: method="bdf" (Backward Differentiation Formula) is designed for stiff systems
#   and is more stable for this type of model.
output <- ode(y = state, times = times, func = Human.Mosquito.SIS.SI.Model, parms = parameters, method = "bdf")
output_df <- as.data.frame(output)
output_df$Temp <- DayTemp[output_df$time + 1]
output_long <- pivot_longer(output_df, cols = -time, names_to = "Compartment", values_to = "Population")
ggplot(output_long, aes(x = time, y = Population, color = Compartment)) +
  geom_line() +
  theme_classic() #or do minimal for lines