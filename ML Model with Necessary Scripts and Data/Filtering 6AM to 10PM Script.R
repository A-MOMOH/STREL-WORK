library(tidyverse)
library(lubridate)

df <- read_csv("Extracted_Columns_Activity_Sleep_ForPredic.csv")

df <- df %>%
  mutate(
    Time_clean = str_replace_all(Time, "\\s+", " "),
    Time_dt = parse_date_time(Time_clean, orders = "mdY HM")
  )

# Filter: 6:00 <= time <= 22:00 (10:00 PM)
df_filtered <- df %>%
  filter(
    (hour(Time_dt) > 6 & hour(Time_dt) < 22) |                   # strictly between 7AM and 9:59PM
      (hour(Time_dt) == 6 & minute(Time_dt) >= 0) |              # all times in the 6AM hour
      (hour(Time_dt) == 22 & minute(Time_dt) == 0)               # exactly 10:00PM
  ) %>%
  select(-Time_clean, -Time_dt)

write_csv(df_filtered, "Filtered_Columns_Activity_Sleep_6AM_to_10PM.csv")

cat("Filtered data (6AM to 10PM inclusive) saved as 'Filtered_Columns_Activity_Sleep_6AM_to_10PM.csv'\n")
