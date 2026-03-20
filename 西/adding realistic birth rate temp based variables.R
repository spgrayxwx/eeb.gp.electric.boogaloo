##trying to fix birth rate

EggToAdultSurvival <- function(Temp) {
  tmin <- 15
  tmax <- 34.6
  
  if (Temp <= tmin || Temp >= tmax) {
    0
  } else {
    return(-0.008122 * (Temp - 24.8)^2 + 0.78)
  }
}
#check w/ plot
temps <- seq(10,35,0.1)
plot(temps, sapply(temps, EggToAdultSurvival), type="l",
     xlab="Temperature (°C)",
     ylab="Egg to Adult Survival Probablity")


EggsPFemalePDay <- function(Temp) {
    tmin <- 15.2
    tmax <- 40.8
    
    if (Temp <= tmin || Temp>= tmax) {
      0
    } else {
      return(-0.1404 * (Temp - 28)^2 + 23)
    }
}
#check w/ plot
temps <- seq(10,45,0.1)
plot(temps, sapply(temps, EggsPFemalePDay), type="l",
     xlab="Temperature (°C)",
     ylab="Eggs Laid per Female per Day")

birthsM <- EggsPFemalePDay(Temp_t) *
  EggToAdultSurvival(Temp_t) *
  MosquitoBirthRatebyTemp(Temp_t) *
  0.5 * (Inf.Mosquitoes + Sus.Mosquitoes) #assuming half female population
