# ======================================================================
# SENSITIVITY RADAR AREA USING MEAN ABSOLUTE RANK DIFFERENCE (MARD) 
# With Weights or pie radar Angle perturbation One-at-a-Time (OAT)
# ======================================================================
# Load Library
library(openxlsx)
library(dplyr)
library(ggplot2)

# Load data (for example: ASEAN_Research_Indicators.xlsx)
data <- read.xlsx(file.choose())

# Select data
df <- data[, c(2, 8:12)]  # Column 2 = Country, 8:12 = normalized indicators values 

# ======================================================================
# Baseline equal weights (pie angles)
n_indicators <- ncol(df) - 1
baseline_weights <- rep(1 / n_indicators, n_indicators)   # weights of same proportion
baseline_theta   <- 2 * pi * baseline_weights             # in radian

# Function for pie radar Area
# ======================================================================
radar_area_theta <- function(r, theta) {
  stopifnot(length(r) == length(theta))
  sum(0.5 * (r^2) * theta)
}

# ======================================================================
# Monte Carlo simulation parameters
n_sim <- 1000
countries <- df$Country

# Prepare storage for results
rank_matrix_area  <- matrix(NA, nrow = length(countries), ncol = n_sim,
                            dimnames = list(countries, paste0("Sim", 1:n_sim)))

# Baseline metrics (equal weights/angles)
baseline_radii <- as.matrix(df[, -1])
baseline_area  <- apply(baseline_radii, 1, function(r) radar_area_theta(r, baseline_theta))
baseline_rank_area  <- rank(-baseline_area,  ties.method = "min")

# ======================================================================
# Function MARD: Mean Absolute Rank Difference
mard <- function(baseline_rank, sim_rank) {
  mean(abs(baseline_rank - sim_rank))
}

mean_mard_area_by_idx  <- numeric(n_indicators)

# One Indicator-at-a-time Simulation 
set.seed(123)
for (idx in 1:n_indicators) {
  mard_area_vec  <- numeric(n_sim)
  
  # Monte Carlo loop
  for (i in 1:n_sim) {
    weights_theta <- baseline_weights
    weights_theta[idx] <- weights_theta[idx] * runif(1, 0.5, 1.5)
    weights_theta      <- weights_theta / sum(weights_theta)
    theta_sim          <- 2 * pi * weights_theta
    
    # Calculate Area with perturbed angle
    sim_area  <- apply(baseline_radii, 1, function(r) radar_area_theta(r, theta_sim))
    
    # Calculate ranks
    ranks_area_sim  <- rank(-sim_area,  ties.method = "min")
    
    # Store MARD 
    mard_area_vec[i]  <- mard(baseline_rank_area,  ranks_area_sim)
  }
  
  # Calculate average MARD for each indicator idx
  mean_mard_area_by_idx[idx]  <- mean(mard_area_vec,  na.rm = TRUE)
}

per_indicator_mard <- data.frame(
  Indicator          = colnames(df)[-1],
  Mean_MARD_Area     = round(mean_mard_area_by_idx,  3)
)

cat("\n=== Mean MARD per Indicators Perturbation (OAT) — AREA only ===\n")
print(per_indicator_mard)

# ======================================================================
# VISUALIZATION

df_area_plot <- per_indicator_mard %>%
  mutate(Indicator = factor(Indicator, levels = Indicator))

p_area_mard <- ggplot(df_area_plot, aes(x = Indicator, y = Mean_MARD_Area)) +
  geom_col(width = 0.55, fill = "steelblue") +
  geom_text(aes(label = round(Mean_MARD_Area, 2)), vjust = -0.4, size = 4) +
  labs(
    title = "Sensitivity by Indicator (MARD) — AREA",
    x = "Indicator",
    y = "Mean Absolute Rank Difference (MARD)",
    scale_x_discrete(expand = expansion(mult = c(0.03, 0.03)))
  ) +
  coord_cartesian(ylim = c(0, max(df_area_plot$Mean_MARD_Area) * 1.15)) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
    panel.border = element_rect(colour="grey", fill=NA, linewidth=0.6)
  ) +
  theme(
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

print(p_area_mard)
