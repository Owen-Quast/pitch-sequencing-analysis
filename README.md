MLB Pitch Sequencing Analysis (2023)
Analyzing how pitch sequencing affects whiff rates using MLB Statcast data. The central question: does the pitch thrown before a given pitch meaningfully change how effective that pitch is at generating swings and misses?

Project Goals

Quantify how the previous pitch in a sequence affects the whiff rate of the next pitch
Build a "Setup Value" metric — whiff rate above or below a pitch type's baseline — to isolate sequencing effects from raw pitch quality
Visualize sequencing strategy across the league using network graphs and heatmaps


Key Findings

Setup pitch has a meaningful impact on fastball (FF) whiff rate. League average FF whiff rate was 11.1%, but this varied from ~7.5% when preceded by a sweeper (ST) or slider (SL) up to ~17.5% when preceded by a cutter (FC) — a swing of over 10 percentage points depending purely on sequence context.
SI → CH is the strongest sequencing edge in the dataset. A sinker followed by a changeup produced the highest positive Setup Value of any sequence meeting the minimum sample threshold (n=264), showing the changeup benefits significantly from sinker velocity contrast.
FC → FF is the most damaging sequence a pitcher can use. With n=231, a cutter into a four-seam fastball showed the largest negative Setup Value in the heatmap — suggesting that the similar velocity and movement profile between cutters and fastballs telegraphs the pitch and actively suppresses whiffs.
The sequencing network is dominated by a single story. After filtering for effect size and sample reliability, the SI → CH edge stands alone as the most impactful sequence — thick, high-Setup Value, and clearly separated from the field. Most other high-volume sequences (particularly those leading into FF) trended negative.


Note: These findings reflect 2023 league-wide trends. Individual pitcher results will vary significantly.


Analysis Structure
PartDescription1Fastball whiff rate broken down by previous pitch type2Raw whiff rate heatmap across all pitch-to-pitch sequences3Setup Value heatmap (whiff rate vs. each pitch's league baseline)4Directed network graph of sequencing strategy weighted by Setup Value

Methodology
Whiff is defined as swinging_strike or swinging_strike_blocked per Statcast event codes.
Setup Value is calculated as:
Setup Value = Whiff Rate(prev_pitch → next_pitch) − Baseline Whiff Rate(next_pitch)
Baseline whiff rate is computed across all instances of a pitch type, regardless of sequence context. Positive Setup Value means a sequence produces more whiffs than that pitch earns on average; negative means it underperforms.
Minimum sample size thresholds applied: n ≥ 100 (Part 1), n ≥ 200 (Part 2), n ≥ 150 (Parts 3–4).
First and last pitches of each at-bat are excluded — no full sequence context is available for those pitches.

Data Source
MLB Statcast (2023 regular season: March 30 – October 1) via the baseballr R package.

Tools Used

R
baseballr — Statcast data retrieval
tidyverse — data wrangling
ggplot2 — visualization
igraph / ggraph — network graph


Author
Owen Quast
