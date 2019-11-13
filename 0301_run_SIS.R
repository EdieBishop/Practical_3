#' ---
#' title: "Using the step_simple_birth_death function from another file"
#' author: "Edie Bishop"
#' date: "8th of November 2019"
#' output: html_document
#' ---

#' # Introduction 
#' In this script we call on a simple population dynamics model (step_deterministic_SIS), which takes a value for 
#' disease transmission rate, and a value for recovery rate, and returns a dataframe containing counts of 
#' susceptible and infection individuals. The model can be represented by the following mathematical equations: 
#' \[S(t + 1) = S(t) -  \beta * S(t) * I(t) / N + \sigma * I(t)\]
#' \[I(t + 1) = I(t) +  \beta * S(t) * I(t) / N - \sigma * I(t)\]
#' *Where S = susceptible animals, I = infected animals, t = time, \(\beta\) = transmission rate & \(\sigma\)*
#' *= recovery rate*
#' 
#' 
#' ### Source our functions 
source("0301_plot_helper.R")
source("0301_step_SIS.R")

#' ### Load packages
library(kableExtra) # Allows us to build tables in the html report 

#' ### Define parameters 

#' To start with we define the population size and the transmission and recovery rates, in this case for 
#' E. coli infection in a herd of cattle.   
num.cattle <- 100
initial.infected <- 2
initial.susceptible <- num.cattle - initial.infected 

#' Next we define our dataframe, herd.df.x, which is a dataframe contianing two columns: number of susceptible 
#' cattle and number of infected cattle. Each dataframe will correspond to a differnt rate of E. coli transmission. 
herd.df.a <- data.frame(SUSCEPTIBLES = initial.susceptible,
                      INFECTED = initial.infected)
herd.df.b <- data.frame(SUSCEPTIBLES = initial.susceptible,
                      INFECTED = initial.infected)
herd.df.c <- data.frame(SUSCEPTIBLES = initial.susceptible,
                      INFECTED = initial.infected)
herd.df.d <- data.frame(SUSCEPTIBLES = initial.susceptible,
                      INFECTED = initial.infected)

#' Then we define the transmission rate and the recovery rate for E. coli infection. We will model four different 
#' transmission rates, and assume the same recovery rate in each simulation. 
ecoli.transmission.a <- 1/2
ecoli.transmission.b <- 2/3
ecoli.transmission.c <- 1
ecoli.transmission.d <- 4/3
ecoli.recovery <- 1/3

#' Then we define the start time and the end time for our study, and the timesteps that we wish our model 
#' to run over. 
start.time <- 0
end.time <- 50
timesteps <- seq(from = start.time + 1, to = end.time)

#' ### Run the function 
#' Now we loop through the time itself (starting at the second time step). The for() command creates the loop, 
#' and then we call our function, step_deterministic_SIS(), which creates a new value for susceptiple and 
#' infected cattle at each time step. We call the funtion four times, in order to model each of the different
#' transmission rates. 

for(new.time in timesteps) {
  ## Calculate the changes to the herd using or steo_deterministic_SIS() function
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.a, 1),
                                   transmission.rate = ecoli.transmission.a,
                                   recovery.rate = ecoli.recovery)
  ## Then define the herd.df dataframe, which is the herd.df dataframe with the updated.population
  ## dataframe appended to the end of it. 
  herd.df.a <- rbind(herd.df.a, updated.population)
}

#' We need to add another column (time) to our population.df dataframe, so that we can make a plot of 
#' population againsts time. We do this by using c() to add start.time and timesteps to the dataframe 
#' in a newly defined column below: 
herd.df.a$time <- c(start.time, timesteps)

#'
#'---------------------------------------------------------------------------------------------------
#'

for(new.time in timesteps) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.b, 1),
                                               transmission.rate = ecoli.transmission.b,
                                               recovery.rate = ecoli.recovery)
  herd.df.b <- rbind(herd.df.b, updated.population)
}
herd.df.b$time <- c(start.time, timesteps)

#'
#'----------------------------------------------------------------------------------------------------
#'

for(new.time in timesteps) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.c, 1),
                                               transmission.rate = ecoli.transmission.c,
                                               recovery.rate = ecoli.recovery)
  herd.df.c <- rbind(herd.df.c, updated.population)
}
herd.df.c$time <- c(start.time, timesteps)

#'
#'-----------------------------------------------------------------------------------------------------
#'

for(new.time in timesteps) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.d, 1),
                                               transmission.rate = ecoli.transmission.d,
                                               recovery.rate = ecoli.recovery)
  herd.df.d <- rbind(herd.df.d, updated.population)
}
herd.df.d$time <- c(start.time, timesteps)

#' ### Plotting the results 

#' Then we can use our plot_population function to plot the herdn.df dataframe. 
plot_populations(herd.df.a, col = c("green", "red"))
#' **Figure 1** 
plot_populations(herd.df.b, col = c("green", "red"))
plot_populations(herd.df.c, col = c("green", "red"))
plot_populations(herd.df.d, col = c("green", "red"))




