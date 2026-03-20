# plot: populations v time
ggplot() +
  geom_line(
    data = output_long,
    aes(x = time, y = Population, color = Compartment),
    linewidth = 1.3
  ) +
  
  # Temperature (subtle dotted gray)
  geom_line(
    data = output_df,
    aes(x = time, y = Temp_scaled),
    color = "grey40",
    linewidth = 0.9,
    linetype = "dotted",
    alpha = 0.6
  ) +
  
  # Rainfall (subtle dotted blue)
  geom_line(
    data = output_df,
    aes(x = time, y = Rain_scaled),
    color = "steelblue3",
    linewidth = 0.9,
    linetype = "dotted",
    alpha = 0.6
  ) +
  
  scale_color_manual(
    values = c(
      "Inf.Humans" = "slateblue3",
      "Inf.Mosquitoes" = "red3",
      "Sus.Humans" = "royalblue1",
      "Sus.Mosquitoes" = "sienna2"
    ),
    labels = c(
      "Inf.Humans" = "Infected Humans",
      "Inf.Mosquitoes" = "Infected Mosquitoes",
      "Sus.Humans" = "Susceptible Humans",
      "Sus.Mosquitoes" = "Susceptible Mosquitoes"
    )
  ) +
  theme_classic() +
  labs(
    title = "Human-Mosquito SIS-SI Model",
    x = "Time (days)",
    color = "Legend"
  )