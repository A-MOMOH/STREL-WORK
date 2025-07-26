library(tidyverse)

# Load your data
df <- read_csv("Predicted_Activity_With_Stress_Label.csv")

# Extract clean day type
df <- df %>%
  mutate(Day_clean = str_extract(Day_label, "^[A-Za-z]+"))

# Define your physical activity labels
physical_labels <- c("Physical Activity")  # Add more as needed, e.g. "Walking", "Running"

# Calculate summaries per participant, per day type
pa_summary <- df %>%
  mutate(is_PA = Predicted_Activity %in% physical_labels) %>%
  group_by(Participant, Day_clean) %>%
  summarise(
    PA_Percent = mean(is_PA, na.rm = TRUE) * 100,  # % intervals that are physical activity
    PA_Intensity_Cadence = mean(Cadence[is_PA], na.rm = TRUE),  # mean cadence for PA intervals
    PA_Intensity_Speed = mean(Speed[is_PA], na.rm = TRUE)       # mean speed for PA intervals
  ) %>%
  ungroup()

# Merge back with original data
df_out <- df %>%
  left_join(pa_summary, by = c("Participant", "Day_clean"))

# Write to new CSV
write_csv(df_out, "Predicted_Activity_With_PA_Summary.csv")

# Preview the new columns
head(df_out %>% select(Participant, Day_label, Day_clean, Predicted_Activity, PA_Percent, PA_Intensity_Cadence, PA_Intensity_Speed))
