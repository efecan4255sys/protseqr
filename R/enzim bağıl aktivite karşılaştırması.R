lab_data <- data.frame(
  sample_id = paste0("S_", 1:12),
  type = rep(c("WT", "Mutant"), each = 6),
  pH = rep(c(5.0, 7.0, 9.0), times = 4),
  absorbance_1 = c(0.25, 0.85, 0.40, 0.28, 0.82, 0.38,
                   0.15, 0.45, 0.70, 0.18, 0.42, 0.68),
  absorbance_2 = c(0.27, 0.89, 0.42, 0.26, 0.80, 0.35,
                   0.14, 0.48, 0.74, 0.17, 0.40, 0.71)
)



p <- ggplot(summary_data, aes(x = factor(pH), y = avg_activity, fill = type)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black") +
  geom_errorbar(
    aes(ymin = avg_activity - sd_activity, ymax = avg_activity + sd_activity),
    position = position_dodge(0.8),
    width = 0.2
  ) +
  scale_fill_manual(values = c("WT" = "#2c7bb6", "Mutant" = "#d7191c")) +
  labs(
    title = "Enzim Bağıl Aktivite Karşılaştırması",
    x = "pH Seviyesi",
    y = "Ortalama Absorbans (OD)",
    fill = "Enzim Türü"
  ) +
  theme_classic()

print(p)
