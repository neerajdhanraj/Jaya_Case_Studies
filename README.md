---

# Jaya Optimization Case Studies and Comparisons

This repository contains the code and resources for case studies demonstrating the application and performance of the Jaya optimization algorithm, along with comparisons to other optimization methods. The repository also includes a multi-objective energy optimization case study using the Jaya algorithm.

---

## Contents

1. **Single-Objective Optimization: Algorithm Comparisons**
   - Implementation of the Jaya algorithm and its comparison with other optimization methods:
     - Differential Evolution (DE)
     - Genetic Algorithm (GA)
     - Simulated Annealing (SA)
     - Particle Swarm Optimization (PSO)
     - Nelder-Mead (NM)
   - Evaluation conducted on classic benchmark problems:
     - Sphere
     - Rastrigin
     - Rosenbrock
     - Ackley
     - Griewank
   - Results include the achieved values for each method across all test functions.

2. **Multi-Objective Energy Optimization**
   - Application of the multi-objective Jaya algorithm to an energy optimization problem.
   - Objectives:
     - Minimize carbon emissions.
     - Minimize system costs.
     - Maximize reliability of renewable energy integration.
   - Outputs include the Pareto front of trade-offs between objectives and visualizations of energy contributions from different sources (e.g., wind, solar, hydro).
   - Discussion on the practical implications of the results for energy policy and planning.

---

## Installation

To use the scripts in this repository, you will need the following R packages installed:

```r
install.packages(c("Jaya", "DEoptim", "GA", "pso"))
```

For visualization and data analysis, additional packages such as `ggplot2` and `plotly` may be required.

---

## Usage

### **1. Single-Objective Optimization**
To run the benchmark comparisons:
1. Open the `single_objective_comparisons.R` script.
2. Modify the parameters if necessary (e.g., bounds, population size, iterations).
3. Run the script to generate results for each optimization method.

The script evaluates all methods on the benchmark problems and outputs the achieved objective values for comparison.

### **2. Multi-Objective Energy Optimization**
To perform the multi-objective energy optimization:
1. Open the `multiobjective_energy_optimization.R` script.
2. Specify the parameters for the energy model (e.g., cost, emissions, reliability metrics).
3. Run the script to optimize energy contributions and generate the Pareto front.

The script outputs:
- Pareto front visualizations.
- Trade-offs between cost, emissions, and reliability.
- Energy contributions from renewable and non-renewable sources.

---

## Repository Structure

```plaintext
.
├── README.md                   # Project overview and instructions
├── single_objective_comparisons.R   # Code for single-objective optimization comparisons
├── multiobjective_energy_optimization.R # Code for multi-objective energy optimization case study
├── benchmark_functions.R       # Definitions of benchmark optimization problems
├── figures/                    # Folder for generated plots and figures
├── data/                       # Data files for energy optimization case study
└── results/                    # Outputs from both case studies
```

---

## Results Overview

### **Case Study 1: Single-Objective Optimization**
- The Jaya algorithm demonstrates competitive performance across all benchmark problems, achieving global optima or near-optimal solutions comparable to other methods.

### **Case Study 2: Multi-Objective Energy Optimization**
- The multi-objective Jaya algorithm effectively balances trade-offs between emissions, costs, and reliability, providing actionable insights for renewable energy integration.

---

## References
- Jaya Algorithm: Rao, R. V. (2016). Jaya: A Simple and New Optimization Algorithm for Solving Constrained and Unconstrained Optimization Problems. *International Journal of Industrial Engineering Computations*.
- Benchmark Functions: Common test functions for optimization (e.g., Sphere, Rastrigin).

---

## License
This repository is licensed under the MIT License. See `LICENSE` for details.

---

## Acknowledgments
- Developed as part of the study comparing optimization techniques and applying Jaya to energy optimization problems.
- Special thanks to contributors and the R development community.

---
