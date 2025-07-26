# -------------------------------
# LOAD LIBRARIES
# -------------------------------
library(tidyverse)
library(lubridate)
library(grid)
library(patchwork)
library(cowplot)

# -------------------------------
# LOAD AND PREP DATA
# -------------------------------
df <- read_csv("Predicted_Activity_ForPredic_CVflow.csv") %>%
  mutate(
    Time = suppressWarnings(ymd_hms(Time, quiet = TRUE)),
    Day_clean = str_extract(Day_label, "^[A-Za-z]+"),
    Day_clean = ifelse(Day_clean %in% c("PR", "MD", "RD", "PS"), Day_clean, NA),
    Day = factor(Day_clean, levels = c("PR", "MD", "PS", "RD")),  # Original levels for data processing
    Activity = factor(Predicted_Activity,
                      levels = c("Office/Home", "Driving", "Physical Activity")),
    Participant = factor(Participant)
  ) %>%
  filter(!is.na(HR_Normalized), !is.na(Speed), !is.na(Cadence), !is.na(Day), !is.na(Time)) %>%
  arrange(Participant, Day, Time) %>%
  group_by(Participant) %>%
  mutate(
    TimeHour = as.numeric(difftime(Time, as.Date(Time), units = "hours"))
  ) %>%
  ungroup()

# -------------------------------
# COLOR DEFINITIONS
# -------------------------------
activity_colors <- c(
  "Office/Home" = "#A0A0A0",
  "Driving" = "orange",
  "Physical Activity" = "#006000"
)

line_colors <- c(
  "HR_Normalized [BPM]" = "red",
  "Speed [km/h]" = "blue",
  "Cadence [CPM]" = "black"
)

# -------------------------------
# ACTIVITY KEY FUNCTION
# -------------------------------
make_activity_key_absolute <- function(activity_colors, box_lines = 2.0, font_size = 22, hpad_npc = 0.12) {
  n <- length(activity_colors)
  x0 <- 0.01
  gg <- ggdraw()
  for (i in seq_along(activity_colors)) {
    color <- activity_colors[i]
    lab <- names(activity_colors)[i]
    gg <- gg +
      draw_grob(grid::rectGrob(
        x = unit(x0, "npc"),
        y = unit(0.5, "npc"),
        width = unit(box_lines, "lines"),
        height = unit(box_lines, "lines"),
        gp = grid::gpar(fill = color, col = "black", lwd = 1.5)
      )) +
      draw_label(lab, x = x0 + 0.08, y = 0.5,
                 hjust = 0, vjust = 0.5, fontface = "bold", size = font_size, color = "black")
    labw <- nchar(lab) * 0.015
    x0 <- x0 + 0.11 + labw + hpad_npc
  }
  gg
}

# Helper function to convert and order day labels for display
convert_day_labels <- function(day) {
  # First convert to new labels
  new_labels <- case_when(
    day == "MD" ~ "CD",
    day == "PR" ~ "WD1",
    day == "PS" ~ "WD2",
    day == "RD" ~ "ND",
    TRUE ~ day
  )
  # Then factor with desired order
  factor(new_labels, levels = c("WD1", "CD", "WD2", "ND"))
}

# -------------------------------
# PLOT SIGNAL PANEL (Dynamic xmax_add)
# -------------------------------
plot_signal_panel <- function(pid, df, signal, color, activity_colors, min_val, max_val, show_day_labels = FALSE, show_x_lab = TRUE) {
  df_part <- df %>% filter(Participant == pid)
  if (nrow(df_part) == 0) return(NULL)
  
  # Calculate median time difference between points (in hours)
  time_diffs <- diff(df_part$TimeHour)
  median_diff <- median(time_diffs, na.rm = TRUE)
  xmax_add <- median_diff * 1.01  # Slightly larger than median interval
  
  df_part <- df_part %>% mutate(SignalValue = .data[[signal]])
  
  # Create rectangles that adapt to the data frequency
  df_rects <- df_part %>%
    group_by(Day) %>%
    mutate(
      time_diff = c(median_diff, diff(TimeHour)),
      xstart = TimeHour,
      xend = pmin(TimeHour + time_diff, 24)
    ) %>%
    ungroup()
  
  # Convert day labels for display and order them
  df_part <- df_part %>% mutate(Day_display = convert_day_labels(as.character(Day)))
  df_rects <- df_rects %>% mutate(Day_display = convert_day_labels(as.character(Day)))
  
  ggplot(df_part, aes(x = TimeHour, y = SignalValue, group = Day)) +
    geom_rect(
      data = df_rects,
      aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Activity),
      inherit.aes = FALSE, alpha = 0.9, color = NA
    ) +
    geom_line(color = color, size = 1.1, show.legend = FALSE, na.rm = TRUE) +
    facet_wrap(~Day_display, ncol = 1, strip.position = "left") +
    scale_fill_manual(values = activity_colors, name = NULL) +
    scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = c("00", sprintf("%02d", seq(2, 22, by = 2)), "00"),
      expand = c(0, 0), limits = c(0, 24)
    ) +
    scale_y_continuous(limits = c(min_val, max_val), expand = c(0, 0.1)) +
    labs(x = if (show_x_lab) "Time [h]" else NULL, y = NULL) +
    theme(
      panel.background = element_rect(fill = "white", color = "black", size = 2.0),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 1.5),
      panel.spacing = unit(1.2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y.left = if (show_day_labels) 
        element_text(size = 18, margin = margin(r = 12), face = "bold", angle = 0, hjust = 1) 
      else element_blank(),
      axis.text.x = element_text(face = "bold", size = 15),
      axis.text.y.left = element_text(size = 14, color = "black", hjust = 1, vjust = 0.5),
      axis.ticks.y.left = element_line(color = "black", size = 1.5),
      axis.line.y.left = element_line(color = "black", size = 1.3),
      axis.ticks.length.y.left = unit(0.3, "cm"),
      axis.title.x = element_text(face = "bold", size = 17),
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.title = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
}

# -------------------------------
# PARTICIPANT PANEL FUNCTION
# -------------------------------
plot_participant_panel <- function(pid, df, activity_colors, line_colors) {
  df_part <- df %>% filter(Participant == pid)
  if (nrow(df_part) == 0) return(NULL)
  
  calculate_limits <- function(x) {
    r <- range(x, na.rm = TRUE)
    c(r[1] - 0.05*diff(r), r[2] + 0.05*diff(r))
  }
  
  min_max <- list(
    HR_Normalized = calculate_limits(df_part$HR_Normalized),
    Speed = calculate_limits(df_part$Speed),
    Cadence = calculate_limits(df_part$Cadence)
  )
  
  p_HR <- plot_signal_panel(pid, df, "HR_Normalized", line_colors[1], activity_colors, 
                            min_max$HR_Normalized[1], min_max$HR_Normalized[2], 
                            show_day_labels = TRUE, show_x_lab = TRUE)
  p_Speed <- plot_signal_panel(pid, df, "Speed", line_colors[2], activity_colors, 
                               min_max$Speed[1], min_max$Speed[2], 
                               show_day_labels = FALSE, show_x_lab = TRUE)
  p_Cadence <- plot_signal_panel(pid, df, "Cadence", line_colors[3], activity_colors, 
                                 min_max$Cadence[1], min_max$Cadence[2], 
                                 show_day_labels = FALSE, show_x_lab = TRUE)
  
  title_plot <- ggdraw() +
    draw_label(paste0("Participant: ", pid), 
               fontface = "bold", size = 26, x = 0.5, hjust = 0.5, vjust = 0.5) +
    theme(plot.margin = margin(0, 0, 5, 0))
  
  activity_key_label <- ggdraw() +
    draw_label("Activity Key:", fontface = "bold", size = 26, x = 0, hjust = 0, vjust = 0.5)
  activity_key_plot <- make_activity_key_absolute(activity_colors)
  
  activity_key_row <- plot_grid(
    NULL, activity_key_label, activity_key_plot, NULL,
    ncol = 4, rel_widths = c(1.6, 1.2, 4.0, 1.6), align = "h"
  )
  
  signal_names_row <- plot_grid(
    ggdraw() + draw_label("Normalized HR [BPM]", fontface = "bold", size = 18, x = 0.5, hjust = 0.5),
    ggdraw() + draw_label("Speed [km/h]", fontface = "bold", size = 18, x = 0.5, hjust = 0.5),
    ggdraw() + draw_label("Cadence [CPM]", fontface = "bold", size = 18, x = 0.5, hjust = 0.5),
    ncol = 3
  )
  
  panels <- plot_grid(p_HR, p_Speed, p_Cadence, 
                      ncol = 3, align = "h", axis = "tb",
                      rel_widths = c(1.1, 1, 1))
  
  # Convert day labels for summary display and maintain order
  df_summary <- df_part %>%
    group_by(Day) %>%
    summarise(
      MeanHR = round(mean(HR_Normalized, na.rm = TRUE), 1),
      MaxSpeed = round(max(Speed, na.rm = TRUE), 2),
      MeanCadence = round(mean(Cadence, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(Day_display = convert_day_labels(as.character(Day))) %>%
    arrange(Day_display)  # Sort by the display order
  
  summary_text <- paste(
    paste0(df_summary$Day_display, ": Mean Normalized HR = ", df_summary$MeanHR,
           ", Mean Cadence = ", df_summary$MeanCadence,
           ", Max Speed = ", df_summary$MaxSpeed),
    collapse = "   ||   "
  )
  
  footer <- ggdraw() +
    draw_label(summary_text, size = 8.2, fontface = "bold", x = 0.5, hjust = 0.5, vjust = 0.5)
  
  plot_grid(
    title_plot,
    activity_key_row,
    signal_names_row,
    panels,
    footer,
    ncol = 1,
    rel_heights = c(0.12, 0.12, 0.10, 1.7, 0.08)
  )
}

# -------------------------------
# GENERATE PDF OUTPUT
# -------------------------------
participants <- unique(df$Participant)
pdf("Predicted_Activity_ForPredic_CVflow.pdf", width = 18, height = 9)
for (pid in participants) {
  plot_obj <- plot_participant_panel(pid, df, activity_colors, line_colors)
  if (!is.null(plot_obj)) print(plot_obj)
}
dev.off()