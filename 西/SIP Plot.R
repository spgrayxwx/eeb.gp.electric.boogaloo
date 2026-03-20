ggplot() +
  geom_line(
    data = prev_long,
    aes(x = time, y = Prevalence, color = Type),
    linewidth = 1.4
  ) +
  geom_line(
    data = output_df,
    aes(x = time, y = Temp_scaled_prev),
    color = "grey40",
    linetype = "dotted",
    linewidth = 0.9,
    alpha = 0.6
  ) +
  geom_line(
    data = output_df,
    aes(x = time, y = Rain_scaled_prev),
    color = "steelblue3",
    linetype = "dotted",
    linewidth = 0.9,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = c(
      "MosqPrev" = "red3",
      "HumanPrev" = "slateblue3"
    ),
    labels = c(
      "MosqPrev" = "% Infected Mosquitoes",
      "HumanPrev" = "% Infected Humans"
    )
  ) +
  theme_classic() +
  labs(
    title = "Seasonal Infection Prevalence with Temperature and Rainfall",
    x = "Time (days)",
    y = "Proportion Infected",
    color = "Legend"
  )