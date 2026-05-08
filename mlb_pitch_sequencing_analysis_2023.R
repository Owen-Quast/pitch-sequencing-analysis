# ==========================================
# Pitch Sequencing Project
# Statcast Pitch Context Analysis
# ==========================================

# Author: Owen Quast
# Data Source: MLB Statcast via baseballr
# Season: 2023
# Goal: Analyze pitch sequencing effects on whiff probability

# -------------------------
# 1) Packages
# -------------------------
install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(missing) > 0) install.packages(missing)
}

install_if_missing(c("baseballr","tidyverse","igraph","ggraph","scales"))

library(baseballr)
library(tidyverse)
library(igraph)
library(ggraph)
library(scales)

# -------------------------
# 2) Download Statcast Data (only once)
# -------------------------
data_path <- "statcast_2023.rds"

if(!file.exists(data_path)){
  message("Downloading Statcast data...")
  statcast_data <- statcast_search(
    start_date = "2023-03-30",
    end_date   = "2023-10-01"
  )
  saveRDS(statcast_data, data_path)
} else {
  message("Loading saved Statcast data")
  statcast_data <- readRDS(data_path)
}

statcast_data <- as_tibble(statcast_data)

# -------------------------
# 3) Keep Needed Columns
# -------------------------
pitch_data <- statcast_data %>%
  select(
    game_pk,
    at_bat_number,
    pitch_number,
    pitch_type,
    release_speed,
    description
  )

# -------------------------
# 4) Build Pitch Context
# -------------------------
pitch_data <- pitch_data %>%
  group_by(game_pk, at_bat_number) %>%
  arrange(pitch_number) %>%
  mutate(
    prev_pitch = lag(pitch_type),
    next_pitch = lead(pitch_type),
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked"
    )
  ) %>%
  ungroup()

# Remove first and last pitches of each at-bat — no full sequence context available
pitch_data <- pitch_data %>%
  filter(!is.na(prev_pitch), !is.na(next_pitch))

# ==========================================
# PART 1
# Fastball Whiff Rate by Previous Pitch
# ==========================================

fastball_seq <- pitch_data %>%
  filter(pitch_type == "FF") %>%        # FIX: use pitch_type (current pitch = FF)
  group_by(prev_pitch) %>%
  summarize(
    whiff_rate = mean(whiff, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 100)

# Baseline: overall FF whiff rate from mid-sequence pitches
ff_baseline <- pitch_data %>%
  filter(pitch_type == "FF") %>%
  summarize(avg_whiff = mean(whiff, na.rm = TRUE)) %>%
  pull(avg_whiff)

ggplot(fastball_seq, aes(x = reorder(prev_pitch, whiff_rate), y = whiff_rate)) +
  geom_col(fill = "gray40") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  geom_hline(yintercept = ff_baseline, linetype = "dashed", color = "red") +
  labs(
    title = "Fastball Whiff Rate by Previous Pitch",
    subtitle = paste0("Red dashed line = league average fastball whiff rate (", round(ff_baseline, 3), ")"),
    x = "Previous Pitch",
    y = "Whiff Rate"
  ) +
  theme_minimal()

# ==========================================
# PART 2
# Raw Pitch Sequence Whiff Rate Heatmap
# ==========================================

min_sequence <- 200

sequence_summary <- pitch_data %>%
  group_by(prev_pitch, pitch_type) %>%       # FIX: group by prev_pitch and pitch_type (current pitch)
  summarize(
    whiff_rate = mean(whiff, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  rename(next_pitch = pitch_type) %>%
  filter(count >= min_sequence)

ggplot(sequence_summary,
       aes(x = prev_pitch,
           y = next_pitch,
           fill = whiff_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = count), size = 3, color = "white") +  # FIX: add count labels for transparency
  scale_fill_viridis_c(option = "plasma", labels = percent_format(accuracy = 1)) +
  labs(
    title = "Pitch Sequencing Whiff Rate",
    subtitle = "Previous Pitch (X) → Next Pitch (Y) | Cell label = sample size",
    x = "Previous Pitch",
    y = "Next Pitch",
    fill = "Whiff Rate"
  ) +
  theme_minimal()

# ==========================================
# PART 3
# Setup Value (Whiff Above Baseline)
# ==========================================

# FIX: Baseline computed separately from the full mid-sequence pool
# This avoids the circular reference where a pitch type influences its own baseline
baseline_next <- pitch_data %>%
  group_by(pitch_type) %>%
  summarize(
    baseline_whiff = mean(whiff, na.rm = TRUE),
    .groups = "drop"
  )

# Sequence whiff rates: prev_pitch → current pitch (pitch_type)
sequence_setup <- pitch_data %>%
  group_by(prev_pitch, pitch_type) %>%
  summarize(
    whiff_rate = mean(whiff, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  left_join(baseline_next, by = "pitch_type") %>%
  rename(next_pitch = pitch_type) %>%
  mutate(setup_value = whiff_rate - baseline_whiff) %>%
  filter(count >= 150)

# FIX: Color scale limits expanded to capture SI→CH at +0.07
ggplot(sequence_setup,
       aes(x = prev_pitch,
           y = next_pitch,
           fill = setup_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = count), size = 3) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-0.05, 0.08),    # FIX: expanded upper limit to show SI→CH at +0.07
    oob = scales::squish,
    labels = function(x) sprintf("%+.2f", x)
  ) +
  labs(
    title = "Pitch Sequencing Setup Value (Whiff Above Baseline)",
    subtitle = "Cell label = sample size (n ≥ 150) | Positive = beats pitch's baseline whiff rate",
    x = "Previous Pitch",
    y = "Next Pitch",
    fill = "Setup Value"
  ) +
  theme_minimal()

# ==========================================
# PART 4: Network Graph (Setup Value)
# ==========================================

edges <- sequence_setup %>%
  transmute(
    from = prev_pitch,
    to   = next_pitch,
    setup_value,
    count
  )

min_effect <- 0.01

edges_plot <- edges %>%
  filter(abs(setup_value) >= min_effect)

message("Edges in network: ", nrow(edges_plot))

pitch_order <- c("FF", "SI", "FC", "SL", "ST", "CU", "CH")

edges_plot <- edges_plot %>%
  mutate(
    from = factor(from, levels = pitch_order),
    to   = factor(to,   levels = pitch_order)
  )

vertices <- tibble(name = pitch_order)

g <- graph_from_data_frame(
  d = edges_plot,
  vertices = vertices,
  directed = TRUE
)

# FIX: Color scale expanded to ±0.08 so SI→CH at +0.07 is fully visible and not clipped
ggraph(g, layout = "circle") +
  geom_edge_link(
    aes(
      width = sqrt(count) * abs(setup_value),
      alpha = abs(setup_value),
      color = setup_value
    ),
    arrow = arrow(length = unit(3, "mm")),
    end_cap = circle(3, "mm")
  ) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), vjust = -1.1, size = 3) +
  scale_edge_width(range = c(0.3, 3.0), guide = "none") +
  scale_edge_alpha(range = c(0.35, 1), name = "|Setup Value|", guide = "none") +
  scale_edge_color_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-0.05, 0.08),    # FIX: matches Part 3 scale, SI→CH fully visible
    oob = scales::squish,
    name = "Setup Value"
  ) +
  labs(
    title = "Pitch Sequencing Network (Setup Value)",
    subtitle = paste0(
      "Blue = improves next-pitch whiffs; Red = reduces them | ",
      "Filters: count ≥ 150, |setup| ≥ ", min_effect
    )
  ) +
  coord_cartesian(clip = "off")
