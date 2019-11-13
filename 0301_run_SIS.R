#' ---
#' title: "Using the step_simple_birth_death function from another file"
#' author: "Edie Bishop"
#' date: "8th of November 2019"
#' output: html_document
#' ---


source("0204-plot_helper.R")
source("0301-step_SIS.R")


num.cattle <- 100
initial.infected <- 2
initial.susceptible <- num.cattle - initial.infected 

herd.df <- data.frame(SUSCEPTIBLES = initial.susceptibles,
                      INFECTED = initial.infected)

ecoli.transmission <- 4
ecoli.recovery <- 2

for(new.time in timesteps) {
  ## First we extract the current population size 
  current.population <- tail(herd.df, 1)
  ## Then calculate the changes to the population using or steo_deterministic_growth() function
  updated.population <-
    step_deterministic_SIS(latest=tail(herd.df, 1),
                                   transmission.rate = ecoli.transmission,
                                   recoversy.rate = ecoli.recovery)
  ## Then define the population.df dataframe, which is the population.df dataframe, with the updated.population
  ## dataframe appended to the end of it. 
  herd.df <- rbind(herd.df, updated.population)
}