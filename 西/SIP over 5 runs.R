#SIP from multi runs
runs <- 10
all_runs <- list()
for(j in 1:runs){
  Temp.w.Noise <- generate_temp_noise(times)
  TempFun <- approxfun(times, Temp.w.Noise, rule = 2)
  output <- ode(
    y = state,
    times = times,
    func = Human.Mosquito.SIS.SI.Model,
    parms = parameters,
    method = "bdf"
  )
  df <- as.data.frame(output)
  df$Temp <- TempFun(df$time)
  df$Rain <- RainFun(df$time)
  df$MosqPrev <- df$Inf.Mosquitoes /
    (df$Inf.Mosquitoes + df$Sus.Mosquitoes)
  df$HumanPrev <- df$Inf.Humans /
    (df$Inf.Humans + df$Sus.Humans)
  df$run <- j
  all_runs[[j]] <- df
}
all_runs_df <- do.call(rbind, all_runs)

avg_df <- all_runs_df %>%
  group_by(time) %>%
  summarise(
    Inf.Mosquitoes = mean(Inf.Mosquitoes),
    Sus.Mosquitoes = mean(Sus.Mosquitoes),
    Inf.Humans = mean(Inf.Humans),
    Sus.Humans = mean(Sus.Humans),
    Temp = mean(Temp),
    Rain = mean(Rain),
    MosqPrev = mean(MosqPrev),
    HumanPrev = mean(HumanPrev)
  )

avg_long <- pivot_longer(
  avg_df,
  cols = c("Inf.Mosquitoes","Sus.Mosquitoes",
           "Inf.Humans","Sus.Humans"),
  names_to = "Compartment",
  values_to = "Population"
)

#actual graph
seasonal_prev <- all_runs_df %>%
  group_by(time) %>%
  summarise(
    mean_mosq = mean(MosqPrev),
    sd_mosq   = sd(MosqPrev),
    mean_human = mean(HumanPrev),
    sd_human   = sd(HumanPrev),
    Temp = mean(Temp),
    Rain = mean(Rain)
  )

temp_scaled <- seasonal_prev$Temp / max(seasonal_prev$Temp) * max(seasonal_prev$mean_mosq)
rain_scaled <- seasonal_prev$Rain / max(seasonal_prev$Rain) * max(seasonal_prev$mean_mosq)
ymax <- max(
  seasonal_prev$mean_human + seasonal_prev$sd_human,
  seasonal_prev$mean_mosq + seasonal_prev$sd_mosq
)

ymin <- 0
yrange <- ymax - ymin
seasonal_prev$Temp_scaled <- 
  (seasonal_prev$Temp - min(seasonal_prev$Temp)) /
  (max(seasonal_prev$Temp) - min(seasonal_prev$Temp)) * yrange + ymin
seasonal_prev$Rain_scaled <- 
  (seasonal_prev$Rain - min(seasonal_prev$Rain)) /
  (max(seasonal_prev$Rain) - min(seasonal_prev$Rain)) * yrange + ymin

print(ggplot(seasonal_prev, aes(x = time)) +
  geom_ribbon(
    aes(ymin = mean_mosq - sd_mosq,
        ymax = mean_mosq + sd_mosq),
    fill = "red3",
    alpha = 0.25
  ) +
  geom_ribbon(
    aes(ymin = mean_human - sd_human,
        ymax = mean_human + sd_human),
    fill = "slateblue3",
    alpha = 0.25
  ) +
  geom_line(
    aes(y = mean_mosq, color = "Mosquito prevalence"),
    linewidth = 1.4
  ) +
  geom_line(
    aes(y = mean_human, color = "Human prevalence"),
    linewidth = 1.4
  ) +
    geom_line(
      aes(y = Temp_scaled, color = "Temperature"),
      linewidth = 0.4
    ) +
    geom_line(
      aes(y = Rain_scaled, color = "Rainfall"),
      linetype = "dotted",
      linewidth = 1
    ) +
    scale_color_manual(
      values = c(
        "Mosquito prevalence" = "red3",
        "Human prevalence" = "slateblue3",
        "Temperature" = "grey40",
        "Rainfall" = "steelblue3"
      ),
      breaks = c(
        "Mosquito prevalence",
        "Human prevalence",
        "Temperature",
        "Rainfall"
      )
    ) +
  theme_classic() +
  labs(
    title = "Seasonal Infection Prevalence with Climate Drivers",
    subtitle = paste(
      "Max Carrying Capacity in Millions:", round(initial_K,2),
      "| Initial Mosquito to Human Ratio:", round(initial_ratio,2),
      "| Annual Mean Temperature:", round(mean_temp,2), "°C"
    ),
    x = "Time (days)",
    y = "Proportion Infected",
    color = ""
  ))
print(mean(output_df$HumanPrev))