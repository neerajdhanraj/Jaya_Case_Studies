# Install required packages if needed
if (!require("DEoptim")) install.packages("DEoptim")
if (!require("GA")) install.packages("GA")
if (!require("pso")) install.packages("pso")
if (!require("Jaya")) install.packages("Jaya")

library(DEoptim)
library(GA)
library(pso)
library(Jaya)

# Define other optimization methods
run_DE <- function(fun, lower, upper) {
  result <- DEoptim(fn = fun, lower = lower, upper = upper, control = DEoptim.control(NP = 50, itermax = 500, trace = FALSE))
  result$optim$bestval
}

run_GA <- function(fun, lower, upper) {
  result <- ga(type = "real-valued", fitness = function(x) -fun(x), lower = lower, upper = upper, maxiter = 500, popSize = 50)
  -result@fitnessValue
}

run_SA <- function(fun, lower, upper) {
  result <- optim(par = (lower + upper) / 2, fn = fun, method = "SANN", control = list(maxit = 500))
  result$value
}

run_PSO <- function(fun, lower, upper) {
  result <- psoptim(par = (lower + upper) / 2, fn = fun, lower = lower, upper = upper, control = list(maxit = 500, s = 50))
  result$value
}

run_NelderMead <- function(fun, lower, upper) {
  result <- optim(par = (lower + upper) / 2, fn = fun, method = "Nelder-Mead", control = list(maxit = 500))
  result$value
}

# Define a function to evaluate all optimizers on each test function
test_single_objective_all <- function(fun, lower, upper) {
  jaya_result <- jaya(fun = fun, lower = lower, upper = upper, popSize = 50, maxiter = 500, n_var = length(lower))
  de_result <- run_DE(fun, lower, upper)
  ga_result <- run_GA(fun, lower, upper)
  sa_result <- run_SA(fun, lower, upper)
  pso_result <- run_PSO(fun, lower, upper)
  nm_result <- run_NelderMead(fun, lower, upper)

  # Return only achieved results
  return(list(
    Jaya = jaya_result$Best,
    DifferentialEvolution = de_result,
    GeneticAlgorithm = ga_result,
    SimulatedAnnealing = sa_result,
    ParticleSwarmOptimization = pso_result,
    NelderMead = nm_result
  ))
}

# Define single-objective test functions
sphere <- function(x) sum(as.numeric(x)^2)
rastrigin <- function(x) 10 * length(x) + sum(as.numeric(x)^2 - 10 * cos(2 * pi * as.numeric(x)))
rosenbrock <- function(x) sum(100 * (x[-1] - as.numeric(x[-length(x)])^2)^2 + (as.numeric(x[-length(x)]) - 1)^2)
ackley <- function(x) -20 * exp(-0.2 * sqrt(mean(as.numeric(x)^2, na.rm = TRUE))) - exp(mean(cos(2 * pi * as.numeric(x)), na.rm = TRUE)) + 20 + exp(1)
griewank <- function(x) 1 + sum(as.numeric(x)^2) / 4000 - prod(cos(as.numeric(x) / sqrt(seq_along(x))))

# Define bounds for each test function
lower_bounds <- list(sphere = rep(-5.12, 2), rastrigin = rep(-5.12, 2), rosenbrock = rep(-5, 2), ackley = rep(-32, 2), griewank = rep(-600, 2))
upper_bounds <- list(sphere = rep(5.12, 2), rastrigin = rep(5.12, 2), rosenbrock = rep(10, 2), ackley = rep(32, 2), griewank = rep(600, 2))

# Run each function with all optimizers
sphere_res <- test_single_objective_all(sphere, lower_bounds$sphere, upper_bounds$sphere)
rastrigin_res <- test_single_objective_all(rastrigin, lower_bounds$rastrigin, upper_bounds$rastrigin)
rosenbrock_res <- test_single_objective_all(rosenbrock, lower_bounds$rosenbrock, upper_bounds$rosenbrock)
ackley_res <- test_single_objective_all(ackley, lower_bounds$ackley, upper_bounds$ackley)
griewank_res <- test_single_objective_all(griewank, lower_bounds$griewank, upper_bounds$griewank)

# Compile and print all results
all_results <- list(Sphere = sphere_res, Rastrigin = rastrigin_res, Rosenbrock = rosenbrock_res, Ackley = ackley_res, Griewank = griewank_res)
print(all_results)
