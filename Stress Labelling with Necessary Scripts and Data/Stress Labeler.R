library(tidyverse)

# Load data
df <- read_csv("Predicted_Activity_ForPredic_CVflow.csv")

# Calculate 2*SD(HR_Baseline) per participant and day type
df <- df %>%
  group_by(Participant, Day_label) %>%
  mutate(
    HR_Normalized_SD = sd(HR_Normalized, na.rm = TRUE),
    Threshold = 2 * HR_Normalized_SD
  ) %>%
  ungroup()

# Apply stress labeling logic
df <- df %>%
  mutate(
    Stress_Label = case_when(
      Predicted_Activity == "Office/Home" & HR_Normalized > Threshold ~ "Stressed",
      Predicted_Activity == "Office/Home" & HR_Normalized <= Threshold ~ "Not Stressed",
      TRUE ~ "Not Applicable"
    )
  )

write_csv(df, "Predicted_Activity_With_Stress_Label.csv")

# View a sample of the result
head(df %>% select(Participant, Day_label, Predicted_Activity, HR_Normalized, HR_Baseline, Threshold, Stress_Label))
