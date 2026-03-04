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

install_if_missing(c("baseballr","tidyverse"))

library(baseballr)
library(tidyverse)

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
  
  saveRDS(statcast_data,data_path)
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
    
    # whiff definition
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked"
    )
  ) %>%
  ungroup()

# Remove rows without sequence context
pitch_data <- pitch_data %>%
  filter(!is.na(prev_pitch), !is.na(next_pitch))

# ==========================================
# PART 1
# Fastball Whiff Rate by Previous Pitch
# ==========================================

fastball_seq <- pitch_data %>%
  filter(next_pitch == "FF") %>%
  group_by(prev_pitch) %>%
  summarize(
    whiff_rate = mean(whiff, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(count >= 100)

# Compute baseline fastball whiff rate
ff_baseline <- pitch_data %>%
  filter(next_pitch == "FF") %>%
  summarize(avg_whiff = mean(whiff, na.rm = TRUE)) %>%
  pull(avg_whiff)

# Plot
ggplot(fastball_seq, aes(x = reorder(prev_pitch, whiff_rate), y = whiff_rate)) +
  geom_col(fill = "gray40") +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  geom_hline(yintercept = ff_baseline, linetype = "dashed", color = "red") +
  labs(
    title = "Fastball Whiff Rate by Previous Pitch",
    subtitle = paste("Red dashed line = league average fastball whiff rate (", round(ff_baseline,3), ")", sep=""),
    x = "Previous Pitch",
    y = "Whiff Rate"
  ) +
  theme_minimal()

# ==========================================
# PART 2
# Raw Pitch Sequence Whiff Rate
# ==========================================

min_sequence <- 200

sequence_summary <- pitch_data %>%
  group_by(prev_pitch, next_pitch) %>%
  summarize(
    whiff_rate = mean(whiff),
    count = n(),
    .groups="drop"
  ) %>%
  filter(count >= min_sequence)

# Heatmap
ggplot(sequence_summary,
       aes(x=prev_pitch,
           y=next_pitch,
           fill=whiff_rate)) +
  geom_tile(color="white") +
  scale_fill_viridis_c() +
  labs(
    title="Pitch Sequencing Whiff Rate",
    subtitle="Previous Pitch → Next Pitch",
    x="Previous Pitch",
    y="Next Pitch",
    fill="Whiff Rate"
  ) +
  theme_minimal()

# ==========================================
# PART 3
# Setup Value (Whiff Above Baseline)
# ==========================================

# Baseline whiff rate for each pitch type
baseline_next <- pitch_data %>%
  group_by(next_pitch = pitch_type) %>%
  summarize(
    baseline_whiff = mean(whiff),
    .groups="drop"
  )

# Calculate setup value
sequence_setup <- pitch_data %>%
  group_by(prev_pitch, next_pitch = pitch_type) %>%
  summarize(
    whiff_rate = mean(whiff),
    count = n(),
    .groups="drop"
  ) %>%
  left_join(baseline_next, by="next_pitch") %>%
  mutate(
    setup_value = whiff_rate - baseline_whiff
  )

min_cell <- 150

sequence_setup_filtered <- sequence_setup %>%
  filter(count >= min_cell)

# Setup value heatmap
ggplot(sequence_setup_filtered,
       aes(x=prev_pitch,
           y=next_pitch,
           fill=setup_value)) +
  geom_tile(color="white") +
  geom_text(aes(label=count),size=3) +
  scale_fill_gradient2(
    low="red",
    mid="white",
    high="blue",
    midpoint=0
  ) +
  labs(
    title="Pitch Sequencing Setup Value (Whiff Above Baseline)",
    subtitle=paste0("Cell label = sample size (n ≥ ",min_cell,")"),
    x="Previous Pitch",
    y="Next Pitch",
    fill="Setup Value"
  ) +
  theme_minimal()

# ==========================================
# PART 4: Network Graph (Setup Value)
# ==========================================

# Packages for network plotting
if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
if (!requireNamespace("ggraph", quietly = TRUE)) install.packages("ggraph")

library(igraph)
library(ggraph)

# ---- Assumes you already have: sequence_setup_filtered ----
# Required cols: prev_pitch, next_pitch, setup_value, count

# 1) Build edges table
edges <- sequence_setup_filtered %>%
  transmute(
    from = prev_pitch,
    to   = next_pitch,
    setup_value,
    count
  )

# 2) Filters (tune these)
min_edge_count <- 150   # reliability threshold
min_effect     <- 0.01  # 1% whiff above/below baseline

edges_plot <- edges %>%
  filter(count >= min_edge_count) %>%
  filter(abs(setup_value) >= min_effect)

message("Edges in network: ", nrow(edges_plot))

# 3) Force a logical circle order (MATCH THESE TO YOUR LABELS)
# If your nodes are still codes (FF, SI, etc), use the code version below instead.
pitch_order <- c(
  "FF",
  "SI",
  "FC",
  "SL",
  "ST",
  "CU",
  "CH"
)
# If your graph uses codes (FF, SI...), use this instead:
# pitch_order <- c("FF","SI","FC","SL","ST","CU","CH")

# Apply order (keeps circle layout in this order)
edges_plot <- edges_plot %>%
  mutate(
    from = factor(from, levels = pitch_order),
    to   = factor(to,   levels = pitch_order)
  )

# 4) Build graph WITH explicit vertex order (this forces circle order)
vertices <- tibble(name = pitch_order)

g <- graph_from_data_frame(
  d = edges_plot,
  vertices = vertices,
  directed = TRUE
)

# 5) Plot (circle layout)
p_net <- ggraph(g, layout = "circle") +
  geom_edge_link(
    aes(
      width = sqrt(count) * abs(setup_value),  # impact-weighted thickness
      alpha = abs(setup_value),
      color = setup_value
    ),
    arrow = arrow(length = unit(3, "mm")),
    end_cap = circle(3, "mm")
  ) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), vjust = -1.1, size = 3) +
  
  # Scales
  scale_edge_width(range = c(0.3, 3.0), guide = "none") +
  scale_edge_alpha(range = c(0.35, 1), name = "|Setup Value|") +
  scale_edge_color_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-0.04, 0.04),     # clamp for readability
    oob = scales::squish,
    name = "Setup Value"
  ) +
  
  labs(
    title = "Pitch Sequencing Network (Setup Value)",
    subtitle = paste0(
      "Blue = improves next-pitch whiffs; Red = reduces them | ",
      "Filters: count ≥ ", min_edge_count, ", |setup| ≥ ", min_effect
    )
  ) +
  coord_cartesian(clip = "off")


print(p_net)
