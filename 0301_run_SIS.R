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

#' ### Define parameters 

#' To start with we define the population size and the transmission and recovery rates, in this case for 
#' E. coli infection in a herd of cattle.   
num.cattle <- 100
initial.infected <- 2
initial.susceptible <- num.cattle - initial.infected 

#' Next we define our dataframe, herd.df, which is a dataframe contianing two columns: number of susceptible 
#' cattle and number of infected cattle.
herd.df <- data.frame(SUSCEPTIBLES = initial.susceptible,
                      INFECTED = initial.infected)

#' Then we define the transmission rate and the recovery rate for E. coli infection 
ecoli.transmission <- 1
ecoli.recovery <- 1/2

#' Then we define the start time and the end time for our study, and the timesteps that we wish our model 
#' to run over. 
start.time <- 0
end.time <- 100
timesteps <- seq(from = start.time + 1, to = end.time)

#' ### Run the function 
#' Now we loop through the time itself (starting at the second time step). The for() command creates the loop, 
#' and then we call our function, step_deterministic_SIS(), which creates a new value for susceptiple and 
#' infected cattle at each time step. 

for(new.time in timesteps) {
  ## Calculate the changes to the herd using or steo_deterministic_SIS() function
  updated.population <- step_deterministic_SIS(latest=tail(herd.df, 1),
                                   transmission.rate = ecoli.transmission,
                                   recovery.rate = ecoli.recovery)
  ## Then define the herd.df dataframe, which is the herd.df dataframe with the updated.population
  ## dataframe appended to the end of it. 
  herd.df <- rbind(herd.df, updated.population)
}

#' We need to add another column (time) to our population.df dataframe, so that we can make a plot of 
#' population againsts time. We do this by using c() to add start.time and timesteps to the dataframe 
#' in a newly defined column below: 
herd.df$time <- c(start.time, timesteps)

#' ### Plotting the results 

#' Then we can use our plot_population function to plot the herdn.df dataframe. 
plot_populations(herd.df, col = c("green", "red"))
