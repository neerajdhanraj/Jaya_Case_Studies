# Load necessary libraries
library(Jaya)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)

# Define Objectives
carbon_emissions <- function(x) {
  wind <- x[1]
  solar <- x[2]
  hydro <- x[3]
  storage <- x[4]
  emissions <- (wind * 0.02 + solar * 0.01 + hydro * 0.005 + storage * 0.01)
  total_emissions <- emissions * (wind + solar + hydro + storage)
  return(total_emissions)
}

cost <- function(x) {
  wind <- x[1]
  solar <- x[2]
  hydro <- x[3]
  storage <- x[4]
  capital_costs <- wind * 2 + solar * 1.5 + hydro * 3 + storage * 2.5
  operational_costs <- wind * 0.5 + solar * 0.3 + hydro * 0.7 + storage * 0.4
  total_cost <- (capital_costs + operational_costs) * (wind + solar + hydro + storage)
  return(total_cost)
}

reliability <- function(x) {
  wind <- x[1]
  solar <- x[2]
  hydro <- x[3]
  storage <- x[4]
  reliability_index <- -(wind * 0.8 + solar * 0.7 + hydro * 0.9 + storage * 0.95)  # Negative for maximization
  return(reliability_index)
}

# Constraints
constraints <- list(
  function(x) sum(x) - 70,         # Renewable contribution >= 70%
  function(x) -x[1] + 10,          # Wind >= 10%
  function(x) x[1] - 40,           # Wind <= 40%
  function(x) -x[2] + 10,          # Solar >= 10%
  function(x) x[2] - 40,           # Solar <= 40%
  function(x) -x[3] + 10,          # Hydro >= 10%
  function(x) x[3] - 40,           # Hydro <= 40%
  function(x) -x[4] + 10,          # Storage >= 10%
  function(x) x[4] - 40            # Storage <= 40%
)

# Optimization Parameters
lower_bounds <- c(10, 10, 10, 10)
upper_bounds <- c(40, 40, 40, 40)

# Run Multi-Objective Optimization with Jaya
result <- jaya_multi(
  objectives = list(carbon_emissions, cost, reliability),
  lower = lower_bounds,
  upper = upper_bounds,
  popSize = 100,
  maxiter = 100,
  n_var = 4,
  constraints = constraints,
  adaptive_pop = TRUE,
  min_popSize = 50,
  max_popSize = 200,
  tolerance = 1e-3,
  patience = 15
)

# Extract Pareto Front
pareto_front <- as.data.frame(result$Pareto_Front)
colnames(pareto_front) <- c("Wind", "Solar", "Hydro", "Storage", "Emissions", "Cost", "Reliability")

# Convert Pareto Front columns to numeric
pareto_front$Emissions <- as.numeric(unlist(pareto_front$Emissions))
pareto_front$Cost <- as.numeric(unlist(pareto_front$Cost))
pareto_front$Reliability <- as.numeric(unlist(pareto_front$Reliability))



# Assuming `pareto_front` is the dataframe with results from the optimization
# Columns: "Wind", "Solar", "Hydro", "Storage", "Emissions", "Cost", "Reliability"

pareto_front <- pareto_front %>%
  mutate(across(c(Emissions, Cost, Reliability, Wind, Solar, Hydro, Storage), as.numeric))

# Create a column for total contribution of all decision variables (optional)
pareto_front <- pareto_front %>%
  mutate(Total_Contribution = Wind + Solar + Hydro + Storage)

# Plot the 3D Pareto front with color representing total contribution (or any variable)
plot1 <- plot_ly(
  data = pareto_front,
  x = ~Emissions,  # X-axis: Emissions
  y = ~Cost,       # Y-axis: Cost
  z = ~Reliability, # Z-axis: Reliability
  color = ~Total_Contribution, # Color by total contribution of decision variables
  marker = list(size = 5), # Marker size
  type = "scatter3d",      # 3D scatter plot
  mode = "markers"         # Plot markers only
) %>%
  layout(
    title = "3D Pareto Front of Energy System Optimization (Colored by Total Contribution)",
    scene = list(
      xaxis = list(title = "Emissions"),
      yaxis = list(title = "Cost"),
      zaxis = list(title = "Reliability")
    )
  )

# Display the plot
plot1




### Plot 1: Total Contribution

# Calculate total contribution
pareto_front$Total <- rowSums(pareto_front[, c("Wind", "Solar", "Hydro", "Storage")])

# 3D Pareto Front (Total Contribution)
fig_total <- plot_ly(
  data = pareto_front,
  x = ~Emissions, y = ~Cost, z = ~Reliability,
  color = ~Total,
  colors = c("blue", "green", "yellow"),
  marker = list(size = 5),
  type = "scatter3d", mode = "markers"
) %>%
  layout(
    title = "3D Pareto Front of Energy System Optimization (Total Contribution)",
    scene = list(
      xaxis = list(title = "Emissions"),
      yaxis = list(title = "Cost"),
      zaxis = list(title = "Reliability")
    )
  )

fig_total




### Plot 2: Wind Contribution

# 3D Pareto Front (Wind Contribution)
fig_wind <- plot_ly(
  data = pareto_front,
  x = ~Emissions, y = ~Cost, z = ~Reliability,
  color = ~Wind,
  colors = c("blue", "green", "yellow"),
  marker = list(size = 5),
  type = "scatter3d", mode = "markers"
) %>%
  layout(
    title = "3D Pareto Front of Energy System Optimization (Colored by Wind Contribution)",
    scene = list(
      xaxis = list(title = "Emissions"),
      yaxis = list(title = "Cost"),
      zaxis = list(title = "Reliability")
    )
  )

fig_wind




### Plot 3: Solar Contribution

# 3D Pareto Front (Solar Contribution)
fig_solar <- plot_ly(
  data = pareto_front,
  x = ~Emissions, y = ~Cost, z = ~Reliability,
  color = ~Solar,
  colors = c("blue", "green", "yellow"),
  marker = list(size = 5),
  type = "scatter3d", mode = "markers"
) %>%
  layout(
    title = "3D Pareto Front of Energy System Optimization (Colored by Solar Contribution)",
    scene = list(
      xaxis = list(title = "Emissions"),
      yaxis = list(title = "Cost"),
      zaxis = list(title = "Reliability")
    )
  )

fig_solar




### Plot 4: Hydro Contribution

# 3D Pareto Front (Hydro Contribution)
fig_hydro <- plot_ly(
  data = pareto_front,
  x = ~Emissions, y = ~Cost, z = ~Reliability,
  color = ~Hydro,
  colors = c("blue", "green", "yellow"),
  marker = list(size = 5),
  type = "scatter3d", mode = "markers"
) %>%
  layout(
    title = "3D Pareto Front of Energy System Optimization (Colored by Hydro Contribution)",
    scene = list(
      xaxis = list(title = "Emissions"),
      yaxis = list(title = "Cost"),
      zaxis = list(title = "Reliability")
    )
  )

fig_hydro
