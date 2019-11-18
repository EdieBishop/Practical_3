#' ---
#' title: "Useing our new function (timestep_deterministic_SIS)"
#' author: "Edie Bishop"
#' date: "13th of November 2019"
#' output: html_document
#' ---

#' # Introduction 
#' In this script we call on a new function (timestep_deterministic_SIS), which takes a value for 
#' disease transmission rate, a value for recovery rate, and a value for timestep, and returns 
#' a dataframe with three columns (usceptibles, infecteds, and time). The model that this function 
#' produces can be represented mathamatically by the following equation:  
#' \[S(t + 1) = S(t) -  \beta * S(t) * I(t) / N + \sigma * I(t)\]
#' \[I(t + 1) = I(t) +  \beta * S(t) * I(t) / N - \sigma * I(t)\]
#' *Where S = susceptible animals, I = infected animals, t = time, \(\beta\) = transmission rate & \(\sigma\)*
#' *= recovery rate*
#' 
#' We will test our new function using two defined timesteps. Since transmission rate of *Escherichia coli* 
#' infection in cattle is usually represented as rate per week, we will specify timesteps so that we can 
#' model the population with daily timesteps and with eight-weekly timesteps. 
#' 
#' We will then do the same thing, but using our previously created simpler function (step_deterministic_SIS), 
#' in which we cannot define a timestep, and need to specify the timesteps outside of the function. 
#' 
#' ### Source our functions 
#' We need to source both our new function (timestep_deterministic_SIS), and our previous function 
#' (step_deterministic_SIS) as well as our plot_populations function. 
source("0301_plot_helper.R")
source("0303_timestep_SIS.R")
source("0301_step_SIS.R")


#' ### Define parameters 

#' To start with we define the population size and the transmission and recovery rates, in this case for 
#' E. coli infection in a herd of cattle.   
num.cattle <- 100
initial.infected <- 2
initial.susceptible <- num.cattle - initial.infected 

ecoli.transmission <- 2/3
ecoli.recovery <- 1/3 

#' Then we define the start time and the end time for our study, and the timesteps that we wish our model 
#' to run over. 
start.time <- 0
end.time <- 100
timestep.a <- 1/7
timestep.b <- 8

#' ## Using step_deterministic_SIS
#' 
#' We will first use our simpler function, step_deterministic_SIS.
#' We need to define our dataframe, herd.df.x, which is a dataframe contianing two columns: number of susceptible 
#' cattle and number of infected cattle. Each dataframe will correspond to a differnt rate of E. coli transmission. 

herd.df.a <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected)
herd.df.b <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected)


#' ### Run the function 
#' Now we loop through the time itself (starting at the second time step). The for() command creates the loop, 
#' and then we call our function, step_deterministic_SIS(), which creates a new value for susceptiple and 
#' infected cattle at each time step. 

#' Define timesteps, which in this case will use the value timestep.a to specify what timesteps our model 
#' will run over 
timesteps.a <- seq(from = start.time + timestep.a, to = end.time, by = timestep.a)

for(new.time in timesteps.a) {
  ## Calculate the changes to the herd using or steo_deterministic_SIS() function
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.a, 1),
                                               transmission.rate = ecoli.transmission * timestep.a,
                                               recovery.rate = ecoli.recovery * timestep.a)
  ## Then define the herd.df dataframe, which is the herd.df dataframe with the updated.population
  ## dataframe appended to the end of it. 
  herd.df.a <- rbind(herd.df.a, updated.population)
}

#' We need to add another column (time) to our population.df dataframe, so that we can make a plot of 
#' population againsts time. We do this by using c() to add start.time and timesteps to the dataframe 
#' in a newly defined column below: 
herd.df.a$time <- c(start.time, timesteps.a)

#'
#'-----------------------------------------------------------------------------------------------------
#'
#' We now do the same thing again, but using timestep.b to define the timesteps we want the model to 
#' run over. 
timesteps.b <- seq(from = start.time + timestep.b, to = end.time, by = timestep.b)

for(new.time in timesteps.b) {
  updated.population <- step_deterministic_SIS(latest=tail(herd.df.b, 1),
                                               transmission.rate = ecoli.transmission * timestep.b,
                                               recovery.rate = ecoli.recovery * timestep.b)
  herd.df.b <- rbind(herd.df.b, updated.population)
}
herd.df.b$time <- c(start.time, timesteps.b)


#' ## Plot the results 
plot_populations(herd.df.a, col = c("seagreen1", "indianred1"))
plot_populations(herd.df.b, new.graph = FALSE, col = c("seagreen", "red3"))

#' **Fig 1.** Change in number of suscetible cattle and cattle infected with *Escherichia coli*, 
#' created using the step_deterministic_SIS() function, over 
#' 30 weeks, modelled with timesteps of one day and eight weeks represented by lighter and darker
#' lines respectively.
#'


#' ## Using timestep_deterministic_SIS
#'
#' We will now use our more complex function, timestep_deterministic_SIS, which accepts four 
#' arguments: latest, timestep, transmission.rate and recovery.rate. 
#' We need to define our new dataframes, this time contianing three columns: number of susceptible 
#' cattle, number of infected cattle and time. We will use the same previously defined transmission and 
#' recovery rates for E. coli. 

herd.df.1 <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected,
                        time = start.time)
herd.df.2 <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected,
                        time = start.time)

#' ### Run the function 
#' Now we can run our new timestep_deterministic_SIS. This function can update time, along with the number 
#' of infected and susceptible individuals. Now we don't need to make the vector of timesteps in advance, 
#' we can use a while() loop to decide when we're finished. 

# we define a new dataframe which will be passed through the function 
updated.population <- herd.df.1
# Rather than specifying timesteps, we can now use a while() loop to specify that we wish to continue running 
# the function until we reach the end time. 
while(updated.population$time < end.time)
{updated.population <-
  # our new function accepts four arguments, which we define below 
  timestep_deterministic_SIS(latest = tail(herd.df.1, 1),
                             timestep = timestep.a,
                             transmission.rate = ecoli.transmission, 
                             recovery.rate = ecoli.recovery)
# we then update the herd.1 dataframe by adding the updated population dataframe, which has been passed 
# through the function, to it. 
herd.df.1 <- rbind(herd.df.1, updated.population)
}

#'
#'-----------------------------------------------------------------------------------------------------
#'

# We will now do the same again, this time using timestep.b
updated.population <- herd.df.2
while(updated.population$time < end.time)
{updated.population <-
  timestep_deterministic_SIS(latest = tail(herd.df.2, 1),
                             timestep = timestep.b,
                             transmission.rate = ecoli.transmission, 
                             recovery.rate = ecoli.recovery)
herd.df.2 <- rbind(herd.df.2, updated.population)
}

#' ### Plot the results
#' 
#' We can now plot the results produced by the timestep_deterministic_SIS function, and see that they are 
#' the same as those produced above. 
plot_populations(herd.df.1, col = c("seagreen1", "indianred1"))
plot_populations(herd.df.2, new.graph = FALSE, col = c("seagreen", "red3"))

#' **Fig 2.** Change is number of suscetible cattle and cattle infected with *Escherichia coli*, 
#' created using the timestep_deterministic_SIS() function, over 
#' 30 weeks, modelled with timesteps of one day and eight weeks represented by lighter and darker
#' lines respectively.
#' 
#' ## Conclusion 
#' 
#' The advantage of our new model is that we no longer need to to define a variable called "timesteps" prior 
#' to running the model. We can now just decide what timestep we want to simulate the model with, define
#' that variable, and pass it to the function. This will decrease the risk of making errors involved with 
#' altering our transmission and recovery rates outside the function, as we know that the function can do this 
#' itself and do it correctly. 
#' 
#' 
#' 
#' 
#' 
#' 