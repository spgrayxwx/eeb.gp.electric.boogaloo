#adding rainfall in w/ stochasticity
#will affect carrying capacity but not yet
#also made using climate change knowledge portal
#of world bank

# 1991-2020 avg annual precipitation p day in mm
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
#restating time to test stuff here
times <- seq(from = 0, to = 365, by = 1)

#smooth precip line
RainfallSpline <- smooth.spline(times, Rainfall01, spar = 0.6)
rain_smoothed <- predict(RainfallSpline, times)$y
RainFun <- approxfun(times, predict(RainfallSpline, times)$y, rule = 2)
Rain_min <- min(predict(RainfallSpline, times)$y)
Rain_max <- max(predict(RainfallSpline, times)$y)
BreedingSiteProp <- function(Rain) {
  Rain_scaled <- (Rain - Rain_min) / (Rain_max - Rain_min)
  0.43 + (1 - 0.43) * Rain_scaled
}

#precipitation data frame
rain_df <- data.frame(
  time = times,
  Rain = rain_smoothed
)

#plotting rainfall per day
ggplot(rain_df, aes(x = time, y = Rain)) +
  geom_line(linewidth = 1.4) +
  theme_classic() +
  labs(
    title = "Smoothed Rainfall Over Time",
    x = "Time (days)",
    y = "Rainfall (mm)"
  )

#ok, so now, let's assume that rainy season represents
#the rainiest months and dry seasons represent the dryest months
#and use the proportions of Spatial and temporal distribution of Anopheles mosquito's larvae and its determinants in two urban sites in Tanzania with different malaria transmission levels
#Mathania et al 2020
#avg of 2.333 breeding sites in rainiest season to 1 in dry season
#assume 2000 breeding sites in wet season to be max K (5 * Initial.Mosquito.Pop
#which starts in dry season btw
#essentially, only ~43% of breeding sites will be available
#it will thus affect K/the carrying capacity and birth rate directly
#available breeding sites can be a proportion which affects K
#1 = full wet season, full saturation so K is as large as possible
#5 * Initial Mosquitoes * 1


