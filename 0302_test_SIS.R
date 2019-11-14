#' ---
#' title: "Simulating disease transmission using different timesteps"
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

#' Next we define our dataframe, herd.df.x, which is a dataframe contianing two columns: number of susceptible 
#' cattle and number of infected cattle. Each dataframe will correspond to a differnt rate of E. coli transmission. 
herd.df <- data.frame(SUSCEPTIBLES = initial.susceptible,
                      INFECTED = initial.infected)
herd.df.a <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected)
herd.df.b <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected)
herd.df.c <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected)

#' In this exercise we will run the model over different timesteps. The default timestep, which the 
#' transmission rate for E. coli in cattle is generally given in, is one week. Therefore, we will first 
#' run the simulation using one week timesteps, followed by one day, then three weeks then five weeks. 
timestep.a <- 1/7
timestep.b <- 3
timestep.c <- 5

#' Then we define the transmission rate and the recovery rate for E. coli infection. We will model four different 
#' transmission rates, and assume the same recovery rate in each simulation. 
ecoli.transmission <- 2/3
ecoli.recovery <- 1/3 

#' Then we define the start time and the end time for our study, and the timesteps that we wish our model 
#' to run over. 
start.time <- 0
end.time <- 30


#' ### Run the function 
#' Now we loop through the time itself (starting at the second time step). The for() command creates the loop, 
#' and then we call our function, step_deterministic_SIS(), which creates a new value for susceptiple and 
#' infected cattle at each time step. We call the funtion four times, in order to model each of the different
#' transmission rates. 
#' We will start by running the simulation without specifying "timestep", therefore each timestep will 
#' be the default of one week. 
timesteps <- seq(from = start.time + 1, to = end.time)

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


#' We then use timestep.a to define a new variable, timesteps.a, which will tell the function that we
#' want to simulate the population at each day. 
timesteps.a <- seq(from = start.time + timestep.a, to = end.time, by = timestep.a)

for(new.time in timesteps.a) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.a, 1),
                                               transmission.rate = ecoli.transmission * timestep.a,
                                               recovery.rate = ecoli.recovery * timestep.a)
  herd.df.a <- rbind(herd.df.a, updated.population)
}
herd.df.a$time <- c(start.time, timesteps.a)



#'
#'---------------------------------------------------------------------------------------------------
#'
#'We then do the same again, this time using timestep.b = 3 to run the simulation with three-weekly timesteps
timesteps.b <- seq(from = start.time + timestep.b, to = end.time, by = timestep.b)
for(new.time in timesteps.b) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.b, 1),
                                               transmission.rate = ecoli.transmission * timestep.b,
                                               recovery.rate = ecoli.recovery * timestep.b)
  herd.df.b <- rbind(herd.df.b, updated.population)
}
herd.df.b$time <- c(start.time, timesteps.b)

#'
#'----------------------------------------------------------------------------------------------------
#'
#' We do the same again, using timestep.c = 5, which will mean we model the tranmission in five-weekly timesteps
timesteps.c <- seq(from = start.time + timestep.c, to = end.time, by = timestep.c)
for(new.time in timesteps.c) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.c, 1),
                                               transmission.rate = ecoli.transmission * timestep.c,
                                               recovery.rate = ecoli.recovery * timestep.c)
  herd.df.c <- rbind(herd.df.c, updated.population)
}
herd.df.c$time <- c(start.time, timesteps.c)


plot_populations(herd.df, col = c("seagreen1", "indianred1"))
plot_populations(herd.df.a, new.graph = FALSE, col = c("palegreen", "salmon"))
plot_populations(herd.df.b, new.graph = FALSE, col = c("seagreen3", "orangered"))
plot_populations(herd.df.c, new.graph = FALSE, col = c("seagreen", "red3"))

#' **Fig 1.** Change is number of suscetible cattle and cattle infected with *Escherichia coli* over 
#' 30 weeks, modelled with timesteps of one day, one week, three weeks and five weeks from lightest 
#' to darkest lines respectively. 




