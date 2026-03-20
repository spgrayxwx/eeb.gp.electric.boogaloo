# I removed the multiple runs from this
library(deSolve)
library(ggplot2)
library(tidyr)
library(patchwork)
library(dplyr)

CustomTemp <- 27
CustomK <- 3 #do 1/2 of desired capacity
CustomMHRatio <- 1

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
Dummy.Annual.Mean.Temp <- CustomTemp
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

EggsPFemalePDay <- function(Temp) {
  tmin <- 15.2
  tmax <- 40.8
  
  if (Temp <= tmin || Temp>= tmax) {
    0
  } else {
    return(-0.1404 * (Temp - 28)^2 + 23)
  }
}

#Comprehensive birth rate by temp term
CompBirthRateByTemp <- function(Temp) { 
  MosquitoBirthRatebyTemp(Temp) * EggsPFemalePDay(Temp) *
    EggToAdultSurvival(Temp) * 0.5 #assuming half female population so 1/2 of eggs r female
  #but also all Sus's and Inf's are female
}

#breeding sites available based on rainfall
BreedingSiteProp <- function(Rain) {
  Rain_scaled <- (Rain - Rain_min) / (Rain_max - Rain_min)
  0.43 + (1 - 0.43) * Rain_scaled
}

# Initial conditions
InitMosIR <- 0.0314
Total.Humans <- 2
Initial.Total.Mosquitoes <- Total.Humans * CustomMHRatio #range of 0.5-3/1.5 4 dry season

#we expect 10% of pop to be infected on average, but we start during the off
#season so we'll use 0.05 or 1/2 of it
Initial.Inf.Mosquitoes <- Initial.Total.Mosquitoes * InitMosIR
Initial.Sus.Mosquitoes <- Initial.Total.Mosquitoes - Initial.Inf.Mosquitoes
Initial.Inf.Humans <- 0.04 * Total.Humans
Initial.Sus.Humans <- Total.Humans - Initial.Inf.Humans

#adjusted beta until mean infection matched the expected/reported for that year
beta_m <- 0.25
beta_h <- 0.125
gamma <- 1 / 92
muM <- SimpleMMortalityRatevTempM

# Carrying capacity for mosquito logistic growth
K_base <- CustomK * Total.Humans #10 mil assuming full breeding site saturation
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
    
    lambda_M <- BiteRate_t * VectorCompetence_t * beta_m * Inf.Humans / NH
    lambda_H <- BiteRate_t * VectorCompetence_t * beta_h * Inf.Mosquitoes / NH
    
    # Softer logistic mosquito birth term
    rM_t <- CompBirthRateByTemp(Temp_t) * 0.45 #arbitrarily makes it smaller + more accurate *
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
output_df$Rain <- RainFun(output_df$time)
output_df$MosqPrev <- output_df$Inf.Mosquitoes /
  (output_df$Inf.Mosquitoes + output_df$Sus.Mosquitoes)
output_df$HumanPrev <- output_df$Inf.Humans /
  (output_df$Inf.Humans + output_df$Sus.Humans)

# Scale Temp and BirthRate for combined plot
temp_scale <- max(output_df$Temp, na.rm = TRUE)
rain_scale <- max(output_df$Rain, na.rm = TRUE)
pop_scale <- max(
  output_df$Inf.Mosquitoes,
  output_df$Sus.Mosquitoes,
  output_df$Inf.Humans,
  output_df$Sus.Humans,
  na.rm = TRUE
)


output_df$Temp_scaled <- output_df$Temp / temp_scale * pop_scale
output_df$Rain_scaled <- output_df$Rain / rain_scale * pop_scale
prev_scale <- max(output_df$MosqPrev, output_df$HumanPrev)
output_df$Temp_scaled_prev <- (output_df$Temp / max(output_df$Temp)) * prev_scale
output_df$Rain_scaled_prev <- (output_df$Rain / max(output_df$Rain)) * prev_scale
#create my Prevalance in Species v Temp + Rainfall plotting data frame
prev_long <- pivot_longer(
  output_df,
  cols = c("MosqPrev", "HumanPrev"),
  names_to = "Type",
  values_to = "Prevalence"
)


#Population data for plotting, temp removed
initial_ratio <- CustomMHRatio
initial_K <- CustomK * 2
mean_temp <- CustomTemp

output_long <- pivot_longer(
  output_df,
  cols = c("Inf.Mosquitoes", "Sus.Mosquitoes", "Inf.Humans", "Sus.Humans"),
  names_to = "Compartment",
  values_to = "Population"
)

# plot: populations v time
print(ggplot() +
  geom_line(
    data = output_long,
    aes(x = time, y = Population, color = Compartment),
    linewidth = 1.3
  ) +
    geom_line(
      data = output_df,
      aes(x = time, y = Temp_scaled, color = "Temperature"),
      linewidth = 0.4,
      alpha = 0.8
    ) +
    
    geom_line(
      data = output_df,
      aes(x = time, y = Rain_scaled, color = "Rainfall"),
      linewidth = 0.9,
      linetype = "dotted",
      alpha = 0.8
    ) +
    scale_color_manual(
      values = c(
        "Inf.Humans" = "slateblue3",
        "Inf.Mosquitoes" = "red3",
        "Sus.Humans" = "royalblue1",
        "Sus.Mosquitoes" = "sienna2",
        "Temperature" = "grey40",
        "Rainfall" = "steelblue3"
      ),
      breaks = c(
        "Inf.Humans",
        "Inf.Mosquitoes",
        "Sus.Humans",
        "Sus.Mosquitoes",
        "Temperature",
        "Rainfall"
      ),
      labels = c(
        "Inf.Humans" = "Infected Humans",
        "Inf.Mosquitoes" = "Infected Mosquitoes",
        "Sus.Humans" = "Susceptible Humans",
        "Sus.Mosquitoes" = "Susceptible Mosquitoes",
        "Temperature" = "Temperature",
        "Rainfall" = "Rainfall"
      )
    ) +
  theme_classic() +
  labs(
    title = "Human–Mosquito SIS–SI Model",
    subtitle = paste(
      "Max Carrying Capacity in Millions:", round(initial_K,2),
      "| Initial Mosquito to Human Ratio:", round(initial_ratio,2),
      "| Annual Mean Temperature:", round(mean_temp,2), "°C"
    ),
    x = "Time (days)",
    color = "Legend"
  ))