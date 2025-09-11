# ======================================================================
# STABILITY RADAR AREA USING SPEARMAN RANK COEFF
# With (all) Weights or pie radar angle perturbation 
# ======================================================================
# Load library
library(openxlsx)
library(dplyr)
library(ggplot2)
library(forcats)

# Load data (for example: ASEAN_Research_Indicators.xlsx)
data <- read.xlsx(file.choose())

# Select data
df <- data[, c(2, 8:12)]  # Column 2 = Country, 8:12 = normalized indicators values 

# ======================================================================
# Baseline equal weights (pie angles)
n_indicators <- ncol(df) - 1
baseline_weights <- rep(1 / n_indicators, n_indicators)   # weights of same proportion
baseline_theta   <- 2 * pi * baseline_weights             # in radian

# Function for pie radar AREA only
# ======================================================================
# PIE-radar Area with specific angles for each sector
# r: vector radius (score of indicators)
# theta: vector of pie angles (radian), with sum(theta)=2*pi
radar_area_theta <- function(r, theta) {
  stopifnot(length(r) == length(theta))
  # Pie Area : 0.5 * r_i^2 * theta_i
  sum(0.5 * (r^2) * theta)
}
# ======================================================================

# Monte Carlo simulation parameters
set.seed(123)
n_sim <- 1000
countries <- df$Country

# Prepare storage for results
rank_matrix_area  <- matrix(NA, nrow = length(countries), ncol = n_sim,
                            dimnames = list(countries, paste0("Sim", 1:n_sim)))

# Calculate Baseline metrics (with equal weights or angles)
# r = indicator scores; theta = 2*pi*(1/N) for all sectors
baseline_radii <- as.matrix(df[, -1])
baseline_area  <- apply(baseline_radii, 1, function(r) radar_area_theta(r, baseline_theta))
baseline_rank_area  <- rank(-baseline_area,  ties.method = "min")

# Monte Carlo loop (Angles perturbation)
# ======================================================================
for (i in 1:n_sim) {
  # Perturb angles with weight ±20%, normalize to sum=1 then convert to radian
  perturb_factors <- runif(n_indicators, 0.8, 1.2)
  weights_theta   <- baseline_weights * perturb_factors
  weights_theta   <- weights_theta / sum(weights_theta)
  theta_sim       <- 2 * pi * weights_theta   # angle per sector (radian)
  
  # Calculate Area with perturbed angle - theta_sim
  sim_area  <- apply(baseline_radii, 1, function(r) radar_area_theta(r, theta_sim))
  
  # Store ranks
  rank_matrix_area[,  i] <- rank(-sim_area,  ties.method = "min")
}
# ======================================================================

# Stability analysis
summarize_stability <- function(rank_matrix, baseline_rank) {
  mean_ranks <- rowMeans(rank_matrix)
  sd_ranks   <- apply(rank_matrix, 1, sd)
  spearman_corr <- sapply(1:ncol(rank_matrix), function(j) {
    suppressWarnings(cor(baseline_rank, rank_matrix[, j], method = "spearman"))
  })
  data.frame(
    Country        = countries,
    Baseline_Rank  = baseline_rank,
    Mean_Rank      = round(mean_ranks, 2),
    Rank_SD        = round(sd_ranks, 2),
    Spearman_Min   = round(min(spearman_corr), 3),
    Spearman_Mean  = round(mean(spearman_corr), 3),
    Spearman_Max   = round(max(spearman_corr), 3)
  ) %>% arrange(Baseline_Rank)
}

area_stability  <- summarize_stability(rank_matrix_area,  baseline_rank_area)

# Print Output
cat("\n=== Area Stability (Weight/Angle Perturbation) ===\n")
print(area_stability)

# ======================================================================
# VISUALIZATION

# Summary Mean Rank ± SD (sorted by baseline rank)
plot_stability_summary <- function(stab_df, title, digits = 2, y_nudge = 0.25) {
  stab_df %>%
    mutate(Country = fct_reorder(Country, Baseline_Rank, .desc = TRUE)) %>%
    ggplot(aes(y = Country, x = Mean_Rank)) +
    geom_errorbarh(aes(xmin = Mean_Rank - Rank_SD, xmax = Mean_Rank + Rank_SD),
                   height = 0.2) +
    geom_point(size = 2) +
    geom_text(
      aes(label = formatC(Mean_Rank, format = "f", digits = digits)),
      nudge_y = y_nudge,
      vjust = 0,
      hjust = 0.5,
      size = 4
    ) +
    scale_y_discrete(expand = expansion(mult = c(0.08, 0.18))) +
    labs(y = NULL, x = "Mean Rank (± SD)", title = title) +
    theme_minimal(base_size = 11) +
    theme(plot.title.position = "plot") +
    theme(panel.border = element_rect(colour="grey", fill=NA, linewidth=0.6)) +
    theme(
      axis.text.x  = element_text(size = 11),
      axis.text.y  = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )
}

p_area_summary  <- plot_stability_summary(area_stability,  "Area Rank Stability (Mean ± SD)")
print(p_area_summary)

cat("\n=== Area Stability (Weight/Angle Perturbation) ===\n")
print(area_stability)

