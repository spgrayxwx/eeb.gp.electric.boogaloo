#so we will try to make a temp set generator
#ideally w/ variability of sorts
#i.e. some years will have more highs
#and lows than others

#base (d); making this based on Nigeria climatology 1991-2020 via world bank CCKP
#changed the basic one to 01
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

Historical.Temp.AnnualMean <- mean(DayTemp01) # based on base, [1] 27.0479
#set our desired mean annual temp + divide by historical
#to get 
Dummy.Annual.Mean.Temp <- 28.5
RunVsHistoricalAnnualMeanTemp <- Dummy.Annual.Mean.Temp / Historical.Temp.AnnualMean

DayTemp <- DayTemp01 * RunVsHistoricalAnnualMeanTemp

times <- seq(from = 0, to = 365, by = 1)

# Smoothed temperature function
TempSpline <- smooth.spline(times, DayTemp, spar = 0.6)

#adding stochasticity via noise
#keep variability between 1 and 2 for accuracy
generate_temp_noise <- function(times, variability = 0.1, phi = 0.7) {
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
TempSeries <- generate_temp_noise(times)
TempFun <- approxfun(times, TempSeries, rule = 2)

# Separate smoothed temperature plot
temp_df <- data.frame(
  time = times,
  Temp = TempFun(times)
)

#temp plot
ggplot(temp_df, aes(x = time, y = Temp)) +
  geom_line(linewidth = 1.4) +
  theme_classic() +
  labs(
    title = "Smoothed Temperature Over Time",
    x = "Time (days)",
    y = "Temperature (°C)"
  )