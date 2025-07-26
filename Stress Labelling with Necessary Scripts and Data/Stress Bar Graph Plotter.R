# -------------------------------
# LOAD LIBRARIES
# -------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)

# -------------------------------
# LOAD AND PREP DATA
# -------------------------------
df <- read_csv("Predicted_Activity_With_Stress_Label.csv")

# -------------------------------
# CLEAN DAY LABELS
# -------------------------------
df <- df %>%
  mutate(Day_clean = str_extract(Day_label, "^[A-Za-z]+"))

# -------------------------------
# SUMMARIZE DATA
# -------------------------------
df_summary <- df %>%
  filter(Predicted_Activity == "Office/Home", Stress_Label %in% c("Stressed", "Not Stressed")) %>%
  group_by(Participant, Day_clean, Stress_Label) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Participant, Day_clean) %>%
  mutate(Proportion = N / sum(N)) %>%
  ungroup()

# -------------------------------
# BAR PLOT: ONE PAGE PER PARTICIPANT
# -------------------------------
bar_colors <- c("Stressed" = "#FF4D53", "Not Stressed" = "lightblue")
participants <- unique(df_summary$Participant)

pdf("Stress_Proportion_Barplots_By_Participant.pdf", width = 8, height = 4)
for (p in participants) {
  p_data <- df_summary %>% filter(Participant == p)
  gg <- ggplot(p_data, aes(x = Day_clean, y = Proportion, fill = Stress_Label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5, color = "black") +
    geom_text(aes(label = paste0(round(Proportion*100), "%")),
              position = position_dodge(width = 0.6), vjust = -0.6, size = 5, fontface = "bold") +
    scale_fill_manual(values = bar_colors, name = "Stress Label: ") +
    labs(
      title = paste0("Proportion of Stress States: ", p),
      x = "Day Type", y = "Percent of Intervals",
      fill = "Stress Label: "
    ) +
    theme_classic(base_size = 16) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.1)) +
    theme(
      axis.text.x = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold"),          # Bold x-axis title
      axis.title.y = element_text(face = "bold"),          # Bold y-axis title
      legend.title = element_text(face = "bold", size = 14),  # Bold legend title
      legend.position = "top"
    )
  print(gg)
}
dev.off()
