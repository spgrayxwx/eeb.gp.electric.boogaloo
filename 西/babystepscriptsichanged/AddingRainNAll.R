library(deSolve)
library(ggplot2)
library(tidyr)

# Nigeria 2011 temperature data
DayTemp01 <- numeric(366)
DayTemp01[1:31]    <- 25.26
DayTemp01[32:59]   <- 27.56
DayTemp01[60:90]   <- 29.73
DayTemp01[91:120]  <- 30.36
DayTemp01[121:151] <- 29.23
DayTemp01[152:181] <- 27.45
DayTemp01[182:212] <- 25.82
DayTemp01[213:243] <- 25.1
DayTemp01[244:273] <- 25.7
DayTemp01[274:304] <- 26.65
DayTemp01[305:334] <- 26.55
DayTemp01[335:366] <- 25.33
Historical.Temp.AnnualMean <- mean(DayTemp01)
#set our desired mean annual temp + divide by historical
#to get 
Dummy.Annual.Mean.Temp <- 28.5
RunVsHistoricalAnnualMeanTemp <- Dummy.Annual.Mean.Temp / Historical.Temp.AnnualMean

DayTemp <- DayTemp01 * RunVsHistoricalAnnualMeanTemp

times <- seq(from = 0, to = 365, by = 1)

# Smoothed temperature function
TempSpline <- smooth.spline(times, DayTemp, spar = 0.6)
#adding stochasticity via noise
generate_temp_noise <- function(times, variability = 1.5, phi = 0.7) {
  base_temp <- predict(TempSpline, times)$y
  n <- length(times)
  noise <- numeric(n)
  noise[1] <- rnorm(1,0,variability)
  for(i in 2:n){
    noise[i] <- phi * noise[i-1] + rnorm(1,0,variability)
  }
  noise <- noise - mean(noise)
  base_temp + noise
}
Temp.w.Noise <- generate_temp_noise(times)
TempFun <- approxfun(times, Temp.w.Noise, rule = 2)

#adding rain as well
Rainfall01 <- numeric(366)
Rainfall01[1:31]    <- 6.28 / 31
Rainfall01[32:59]   <- 11.52 / 28
Rainfall01[60:90]   <- 28.58 / 31
Rainfall01[91:120]  <- 58.47 / 30
Rainfall01[121:151] <- 108.01 / 31
Rainfall01[152:181] <- 138.73 / 30
Rainfall01[182:212] <- 185.01 / 31
Rainfall01[213:243] <- 235.81 / 31
Rainfall01[244:273] <- 182.14 / 30
Rainfall01[274:304] <- 105.5 / 31
Rainfall01[305:334] <- 23.83 / 30
Rainfall01[335:366] <- 6.33 / 31

#smooth precip line
RainfallSpline <- smooth.spline(times, Rainfall01, spar = 0.6)
rain_smoothed <- predict(RainfallSpline, times)$y
RainFun <- approxfun(times, predict(RainfallSpline, times)$y, rule = 2)
Rain_min <- min(predict(RainfallSpline, times)$y)
Rain_max <- max(predict(RainfallSpline, times)$y)

# Temperature-dependent bite rate
DailyBiteRatebyTemp <- function(Temp) {
  pmax(-0.170 + Temp * 0.0167, 0)
}

# Temperature-dependent vector competence
Vector.Competence.bTemp <- function(Temp) {
  pmax(-0.00360 * Temp^2 + 0.1786 * Temp - 1.62, 0)
}

# Temperature-dependent mosquito mortality rate
SimpleMMortalityRatevTempM <- function(Temp) {
  1 / pmax(44.44 - 0.943852 * Temp, 1)
}

# Temperature-dependent mosquito birth rate
MosquitoBirthRatebyTemp <- function(Temp) {
  q <- 0.000111
  tmin <- 14.7
  tmax <- 34.0
  
  if (Temp <= tmin || Temp >= tmax) {
    0
  } else {
    q * Temp * (Temp - tmin) * sqrt(tmax - Temp)
  }
}

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

#Comprehensive birth rate by temp term
CompBirthRateByTemp <- function(Temp) { 
  MosquitoBirthRatebyTemp(Temp) * EggsPFemalePDay(Temp) *
    EggToAdultSurvival(Temp) * MosquitoBirthRatebyTemp(Temp) *
    0.5 #assuming half female population
}

#breeding sites available based on rainfall
BreedingSiteProp <- function(Rain) {
  Rain_scaled <- (Rain - Rain_min) / (Rain_max - Rain_min)
  0.43 + (1 - 0.43) * Rain_scaled
}

# Initial conditions
InitMosIR <- 0.0314
Total.Humans <- 2
Initial.Total.Mosquitoes <- Total.Humans * 1.5 #range of 0.5-3/1.5 4 dry season

#we expect 10% of pop to be infected on average, but we start during the off
#season so we'll use 0.05 or 1/2 of it
Initial.Inf.Mosquitoes <- Initial.Total.Mosquitoes * InitMosIR
Initial.Sus.Mosquitoes <- Initial.Total.Mosquitoes - Initial.Inf.Mosquitoes
Initial.Inf.Humans <- 0.04 * Total.Humans
Initial.Sus.Humans <- Total.Humans - Initial.Inf.Humans

#adjusted beta until mean infection matched the expected/reported for that year
beta <- 0.142
gamma <- 1 / 92
muM <- SimpleMMortalityRatevTempM

# Carrying capacity for mosquito logistic growth
K_base <- 5 * Total.Humans #10 mil assuming full breeding site saturation
parameters <- c(
  Total.Humans = Total.Humans,
  K_base = K_base
)

state <- c(
  Inf.Mosquitoes = Initial.Inf.Mosquitoes,
  Sus.Mosquitoes = Initial.Sus.Mosquitoes,
  Inf.Humans = Initial.Inf.Humans,
  Sus.Humans = Initial.Sus.Humans
)

# ODE model
Human.Mosquito.SIS.SI.Model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    Temp_t <- TempFun(t)
    Rain_t <- RainFun(t)
    Breeding_t <- BreedingSiteProp(Rain_t)
    K_t <- K_base * Breeding_t
    BiteRate_t <- DailyBiteRatebyTemp(Temp_t)
    VectorCompetence_t <- Vector.Competence.bTemp(Temp_t)
    
    NM <- Sus.Mosquitoes + Inf.Mosquitoes
    NH <- Sus.Humans + Inf.Humans
    
    lambda_M <- BiteRate_t * VectorCompetence_t * beta * Inf.Humans / NH
    lambda_H <- BiteRate_t * VectorCompetence_t * beta * Inf.Mosquitoes / NH
    
    # Softer logistic mosquito birth term
    rM_t <- MosquitoBirthRatebyTemp(Temp_t) * EggsPFemalePDay(Temp_t) * 0.09 #arbitrarily makes it smaller + more accurate *
    EggToAdultSurvival(Temp_t) * 0.5 #assuming half female population
    birthsM <- rM_t * NM * (1 - NM/K_t)
    
    # Mosquito deaths
    deaths.SM <- muM(Temp_t) * Sus.Mosquitoes
    deaths.IM <- muM(Temp_t) * Inf.Mosquitoes
    
    # Differential equations
    dIM <- lambda_M * Sus.Mosquitoes - deaths.IM
    dSM <- birthsM - lambda_M * Sus.Mosquitoes - deaths.SM
    dIH <- lambda_H * Sus.Humans - gamma * Inf.Humans
    dSH <- -lambda_H * Sus.Humans + gamma * Inf.Humans
    
    list(c(dIM, dSM, dIH, dSH))
  })
}

# Solve model
output <- ode(
  y = state,
  times = times,
  func = Human.Mosquito.SIS.SI.Model,
  parms = parameters,
  method = "bdf"
)

output_df <- as.data.frame(output)

# Add temperature and birth rate to output
output_df$Temp <- TempFun(output_df$time)
output_df$BirthRate <- sapply(output_df$Temp, MosquitoBirthRatebyTemp)

# Scale Temp and BirthRate for combined plot
temp_scale <- max(output_df$Temp, na.rm = TRUE)
birth_scale <- max(output_df$BirthRate, na.rm = TRUE)
pop_scale <- max(
  output_df$Inf.Mosquitoes,
  output_df$Sus.Mosquitoes,
  output_df$Inf.Humans,
  output_df$Sus.Humans,
  na.rm = TRUE
)

output_df$Temp_scaled <- output_df$Temp / temp_scale * pop_scale
output_df$BirthRate_scaled <- output_df$BirthRate / birth_scale * pop_scale

#Population data for plotting, temp removed
output_long <- pivot_longer(
  output_df,
  cols = c("Inf.Mosquitoes", "Sus.Mosquitoes", "Inf.Humans", "Sus.Humans"),
  names_to = "Compartment",
  values_to = "Population"
)

# Combined plot: populations + temperature + birth rate
ggplot() +
  geom_line(
    data = output_long,
    aes(x = time, y = Population, color = Compartment),
    linewidth = 1.3
  ) +
  #geom_line(
  # data = output_df,
  #aes(x = time, y = Temp_scaled, color = "Temperature (°C)"),
  #linewidth = 1.2,
  #linetype = "dashed"
  #) +
  #geom_line(
  # data = output_df,
  #aes(x = time, y = BirthRate_scaled, color = "Birth Rate"),
  #linewidth = 1.2,
  #linetype = "dotdash"
  #) +
  #scale_y_continuous(
  #name = "Population in Millions",
  #sec.axis = sec_axis(
  # ~ . / pop_scale,
  #name = "Temperature / Birth Rate (scaled)"
  #)
  #) +
  scale_color_manual(
    breaks = c( #keeps Temp and Birth Rate next to each other in legend 4 readability
      #"Temperature (°C)",
      #"Birth Rate",
      "Inf.Humans",
      "Inf.Mosquitoes",
      "Sus.Humans",
      "Sus.Mosquitoes"
    ),
    values = c(
      "Inf.Humans" = "slateblue3",
      "Inf.Mosquitoes" = "red3",
      "Sus.Humans" = "royalblue1",
      "Sus.Mosquitoes" = "sienna2" #,
      #"Temperature (°C)" = "black",
      #"Birth Rate" = "cadetblue2"
    ), 
    labels = c(
      "Inf.Humans" = "Infected Humans",
      "Inf.Mosquitoes" = "Infected Mosquitoes",
      "Sus.Humans" = "Susceptible Humans",
      "Sus.Mosquitoes" = "Susceptible Mosquitoes"#,
      #"Temperature (°C)" = "Temperature (°C), Scaled",
      #"Birth Rate" = "Mosquito Birth Rate, Scaled"
    )
  ) +
  theme_classic() +
  labs(
    title = "Human-Mosquito SIS-SI Model",
    x = "Time (days)",
    color = "Legend"
  )

# Separate smoothed temperature plot
temp_df <- data.frame(
  time = times,
  Temp = TempFun(times)
)

ggplot(temp_df, aes(x = time, y = Temp)) +
  geom_line(linewidth = 1.4) +
  theme_classic() +
  labs(
    title = "Smoothed Temperature Over Time",
    x = "Time (days)",
    y = "Temperature (°C)"
  )

# Separate birth rate vs temperature plot
temp_range <- seq(10, 36, by = 0.1)

birth_df <- data.frame(
  Temperature = temp_range,
  BirthRate = sapply(temp_range, CompBirthRateByTemp)
)

ggplot(birth_df, aes(x = Temperature, y = BirthRate)) +
  geom_line(linewidth = 1.4, color = "cadetblue2") +
  theme_classic() +
  labs(
    title = "Mosquito Rate of Births Surviving to Adulthood vs Temperature",
    x = "Temperature (°C)",
    y = "Births of Eventual Adults per Day per Female"
  )

#visualizing death rate (only useful for our purposes)
death_df <- data.frame(
  Temperature = temp_range,
  DeathRate = sapply(temp_range, muM)
)

ggplot(death_df, aes(x = Temperature, y = DeathRate)) +
  geom_line(linewidth = 1.4, color = "cadetblue2") +
  theme_classic() +
  labs(
    title = "Adult Mosquito Death Rate vs Temperature (°C)"
  )
