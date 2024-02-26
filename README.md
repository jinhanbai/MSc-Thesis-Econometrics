# Quantifying Financial Resilience: Tail Risk Assessment in High-Dimensional Predictor Spaces Leveraging Extreme Value Theory and Random Forests

## Table of Contents
- [General Scripts](#general-scripts)
  - [packages.R](#packagesr)
  - [runfile.R](#runfiler)
  - [cv_functions.R](#cv_functionsr)
  - [emf.R](#emfr)
  - [erf.R](#erfr)
  - [ecf.R](#ecfr)
  - [general_functions.R](#general_functionsr)
- [Simulation Studies](#simulation-studies)
  - [simulations_main.R](#simulations_mainr)
  - [data_gen.R](#datagenr)
  - [test_significance.R](#test_significancer)
  - [sim.R](#simr)
- [Application](#application)
  - [backtest.R](#backtestr)
  - [application_main.R](#application_mainr)
  - [applications_functions.R](#applications_functionsr)
  - [variable_importance.R](#variable_importancer)
  - [raw_daily_data_prep.R](#raw_daily_data_prepr)
  - [raw_monthly_data_prep.R](#raw_monthly_data_prepr)
  - [raw_quarterly_data_prep.R](#raw_quarterly_data_prepr)
  - [data_aggregate.R](#data_aggregater)
  - [data_visualization.R](#data_visualizationr)

## General Scripts
This section contains scripts related to general functionalities.

### packages.R
List of packages used.

### runfile.R
Defines global functions/variables.

### cv_functions.R
General functions to run cross-validation procedure.

### emf.R
Contains functions related to the EMF methodology.

### erf.R
Contains functions related to the ERF methodology.

### ecf.R
Contains functions related to the ECF methodology.

### general_functions.R
Some general functions.

## Simulation Studies
Scripts related to conducting simulation studies.

### simulations_main.R
Main script to run simulation studies.

### data_gen.R
Script to generate data for simulation studies.

### test_significance.R
Script to perform statistical tests for performance differences.

### sim.R
General functions to run the simulation studies.

## Application
Scripts related to application studies.

### backtest.R
Run Christoffersen backtesting procedure.

### application_main.R
Main script to run application studies.

### applications_functions.R
General functions related to the application studies.

### variable_importance.R
Generates correlation matrices and variable importance plots.

### raw_daily_data_prep.R
Processes the daily variables.

### raw_monthly_data_prep.R
Processes the monthly variables.

### raw_quarterly_data_prep.R
Processes the quarterly variables.

### data_aggregate.R
Aggregates all variables to one dataset.

### data_visualization.R
Calculates descriptive statistics and plots the time series.
