library(ggplot2)

# Create comparison data
comparison_data <- data.frame(
  Metric = rep(c("Word Count", "Character Count", "Unique Words"), 2),
  Level = rep(c("High-Detail", "Low-Detail"), each = 3),
  Value = c(
    mean(df_with_clusters$word_count[df_with_clusters$documentation_level == "High-Detail"]),
    mean(df_with_clusters$char_count[df_with_clusters$documentation_level == "High-Detail"]),
    mean(df_with_clusters$unique_words[df_with_clusters$documentation_level == "High-Detail"]),
    mean(df_with_clusters$word_count[df_with_clusters$documentation_level == "Low-Detail"]),
    mean(df_with_clusters$char_count[df_with_clusters$documentation_level == "Low-Detail"]),
    mean(df_with_clusters$unique_words[df_with_clusters$documentation_level == "Low-Detail"])
  )
)

# Normalize for comparison (scale to 0-100)
comparison_data <- comparison_data %>%
  group_by(Metric) %>%
  mutate(Normalized_Value = (Value / max(Value)) * 100)

ggplot(comparison_data, aes(x = Metric, y = Normalized_Value, fill = Level)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("High-Detail" = "#154733", "Low-Detail" = "#D3E5D3")) +
  geom_text(aes(label = round(Value, 0)), position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  labs(
    title = "Documentation Metrics: High-Detail vs Low-Detail",
    x = "",
    y = "Normalized Value (0-100)",
    fill = "Documentation Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("metrics_comparison.png", width = 10, height = 6, dpi = 300)




# Step 1: Create summary_clusters
summary_clusters <- df_with_clusters %>%
  group_by(documentation_level) %>%
  summarise(
    mean_word_count = mean(word_count, na.rm = TRUE),
    mean_char_count = mean(char_count, na.rm = TRUE),
    mean_unique_words = mean(unique_words, na.rm = TRUE),
    n = n()
  )

# Step 2: View the summary
print(summary_clusters)

# Step 3: Now you can use it for the radar chart
library(fmsb)

# Prepare data - get max values for normalization
max_word <- max(df_with_clusters$word_count, na.rm = TRUE)
max_char <- max(df_with_clusters$char_count, na.rm = TRUE)
max_unique <- max(df_with_clusters$unique_words, na.rm = TRUE)

# Get values for each cluster
high_detail <- summary_clusters %>% filter(documentation_level == "High-Detail")
low_detail <- summary_clusters %>% filter(documentation_level == "Low-Detail")

# Create radar chart data frame
# First two rows are max and min for the chart
radar_data <- data.frame(
  Word_Count = c(100, 0, 
                 (high_detail$mean_word_count / max_word) * 100,
                 (low_detail$mean_word_count / max_word) * 100),
  Char_Count = c(100, 0,
                 (high_detail$mean_char_count / max_char) * 100,
                 (low_detail$mean_char_count / max_char) * 100),
  Unique_Words = c(100, 0,
                   (high_detail$mean_unique_words / max_unique) * 100,
                   (low_detail$mean_unique_words / max_unique) * 100)
)

# Create the radar chart
radarchart(radar_data, 
           pcol = c("#154733", "#F96167"),  # Colors for High-Detail and Low-Detail
           plwd = 3,                         # Line width
           plty = 1,                         # Line type
           cglcol = "grey",                  # Grid color
           cglty = 1,                        # Grid line type
           axislabcol = "grey",              # Axis label color
           title = "Documentation Characteristics by Cluster")

# Add legend
legend("topright", 
       legend = c("High-Detail", "Low-Detail"), 
       col = c("#154733", "#F96167"), 
       lty = 1, 
       lwd = 3,
       bty = "n")

# Method 1: Save as PNG (Recommended for PowerPoint)
png("radar_chart_documentation.png", width = 800, height = 800, res = 150)

# Create the radar chart
radarchart(radar_data, 
           pcol = c("#154733", "#F96167"),
           pfcol = c(rgb(21/255, 71/255, 51/255, 0.3), 
                     rgb(249/255, 97/255, 103/255, 0.3)),
           plwd = 3,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 25),
           title = "Documentation Characteristics by Cluster")

legend("topright", 
       legend = c("High-Detail", "Low-Detail"), 
       col = c("#154733", "#F96167"), 
       lty = 1, lwd = 3, bty = "n")

dev.off()  # Close the PNG device

# Method 2: Save as PDF (Better for print quality)
pdf("radar_chart_documentation.pdf", width = 8, height = 8)

radarchart(radar_data, 
           pcol = c("#154733", "#F96167"),
           pfcol = c(rgb(21/255, 71/255, 51/255, 0.3), 
                     rgb(249/255, 97/255, 103/255, 0.3)),
           plwd = 3,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 25),
           title = "Documentation Characteristics by Cluster")

legend("topright", 
       legend = c("High-Detail", "Low-Detail"), 
       col = c("#154733", "#F96167"), 
       lty = 1, lwd = 3, bty = "n")

dev.off()

# Method 3: Higher resolution PNG for publication
png("radar_chart_high_res.png", width = 1200, height = 1200, res = 300)

radarchart(radar_data, 
           pcol = c("#154733", "#F96167"),
           pfcol = c(rgb(21/255, 71/255, 51/255, 0.3), 
                     rgb(249/255, 97/255, 103/255, 0.3)),
           plwd = 3,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 25),
           title = "Documentation Characteristics by Cluster")

legend("topright", 
       legend = c("High-Detail", "Low-Detail"), 
       col = c("#154733", "#F96167"), 
       lty = 1, lwd = 3, bty = "n")

dev.off()

print("Radar charts saved successfully!")


