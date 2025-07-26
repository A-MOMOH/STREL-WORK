# Load necessary library
library(tidyverse)

# Read the data
df <- read_csv("Activity_Stress_data_N24.csv")

# Select columns and remove "Unknown" and "Sleeping" activities
df_subset <- df %>%
  select(Participant, Time, Date, Day, HR, HR_Normalized, Speed, Cadence, HR_Baseline, Activity4) %>%
  filter(Activity4 != "Unknown", Activity4 != "Sleeping") %>%
  mutate(
    Modified_Activity = case_when(
      Activity4 %in% c("Work", "NonWork") ~ "Office/Home",
      Activity4 %in% c("Biking", "Walking", "Running") ~ "Physical Activity",
      TRUE ~ as.character(Activity4)
    )
  )

# View a sample of the result
head(df_subset %>% select(Activity4, Modified_Activity))

# Write to a new CSV file
write_csv(df_subset, "Extracted_Columns_Activity_Sleep.csv")

# Print a message
cat("New CSV file 'Extracted_Columns_Activity_Sleep.csv' created with selected columns and no 'Unknown' or 'Sleeping' activities.\n")
