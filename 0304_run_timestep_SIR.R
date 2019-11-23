#' ---
#' title: "Useing our new function (timestep_deterministic_SIR)"
#' author: "Edie Bishop"
#' date: "13th of November 2019"
#' output: html_document
#' ---

#' # Introduction 
#' In this script we call on a new function (timestep_deterministic_SIR), which takes a value for 
#' disease transmission rate, a value for recovery rate, and a value for timestep, and returns 
#' a dataframe with four columns (susceptibles, infecteds, recovered and time). In this exercise we will be 
#' modelling a foot and mouth disease (FMD) outbreak. Since this disease moves so quickly through herds, 
#' we will model it in terms of farms affected, rather than individual cattle. The model can be represented 
#' mathamatically by the following equation:  
#' \[S(t + 1) = S(t) -  \beta * S(t) * I(t) / N \]
#' \[I(t + 1) = I(t) +  \beta * S(t) * I(t) / N - \sigma * I(t)\]
#' \[R(t + 1) = R(t) + \sigma * I(t) \]
#' *Where S = susceptible farms, I = infected farms, R = recovered farms, t = time,*
#' *\(\beta\) = transmission rate & \(\sigma\) = recovery rate*
#' 
#' We will test our new function using two different tansmission rates. The transmission and recovery rates 
#' for FMD are generally very high, hence why we are modelling the disease outbreak in farms rather than 
#' in individual cattle. For this reason, timesteps need to be small (smaller than 1) in order to properly 
#' capture the outbreak.  
#' 
#' 
#' ### Source our functions 
#' We need to source both our new function (timestep_deterministic_SIR), and the plot_populations function. 
source("0301_plot_helper.R")
source("0304_step_SIR.R")


#' ### Define parameters 

#' To start with we define the population size and the transmission and recovery rates, in this case for 
#' FMD infection across farms.   
num.farms <- 100
initial.infected <- 2
initial.susceptible <- num.farms - initial.infected 
initial.recovered <- num.farms - initial.infected - initial.susceptible 

#' Then we define the transmission rate and the recovery rate for FMD. 
FMD.transmission.a <- 2
FMD.transmission.b <- 1
FMD.recovery <- 1/2

#' Then we define the start time and the end time for our study, and the timesteps that we wish our model 
#' to run over. 
start.time <- 0
end.time <- 30
timestep <- 1/7

#' Next we define our dataframes. We will have a separate dataframe to model the two different recovery rates. 
#' Since we now have essentially three populations (susceptible, infected and recovered), we will need a 
#' dataframe with four columns. 
farm.df.1 <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected,
                        RECOVERED = initial.recovered,
                        time = start.time)
farm.df.2 <- data.frame(SUSCEPTIBLES = initial.susceptible,
                        INFECTED = initial.infected,
                        RECOVERED = initial.recovered,
                        time = start.time)

#' ### Run the function 
#' Now we can run our new timestep_deterministic_SIR. This function will update time and the number 
#' of infected, susceptible and recovered farms. 

# we define a new dataframe which will be passed through the function 
updated.population <- farm.df.1
# Rather than specifying timesteps, we can now use a while() loop to specify that we wish to continue running 
# the function until we reach the end time. 
while(updated.population$time < end.time)
{updated.population <-
  # our new function accepts four arguments, which we define below 
  timestep_deterministic_SIR(latest = tail(farm.df.1, 1),
                             timestep = timestep,
                             transmission.rate = FMD.transmission.a, 
                             recovery.rate = FMD.recovery)
# we then update the herd.1 dataframe by adding the updated population dataframe, which has been passed 
# through the function, to it. 
farm.df.1 <- rbind(farm.df.1, updated.population)
}

#'
#'-----------------------------------------------------------------------------------------------------
#'

# We will now do the same again, this time using timestep.b
updated.population <- farm.df.2
while(updated.population$time < end.time)
{updated.population <-
  timestep_deterministic_SIR(latest = tail(farm.df.2, 1),
                             timestep = timestep,
                             transmission.rate = FMD.transmission.b, 
                             recovery.rate = FMD.recovery)
farm.df.2 <- rbind(farm.df.2, updated.population)
}

#' ### Plot the results
#' We can now plot the results produced by the timestep_deterministic_SIR function. We would expect that 
#' the outbreak with the lower transmission rate will have a slightly more prolonged timeframe, and a lower 
#' peak in infected population.  
plot_populations(farm.df.1)
#' **Fig 1.** Change in the number of susceptible, infected and recovered farms during a bovine FMD 
#' outbreak, assuming an Ro of 4 (transmission rate = 2, recovery rate = 1/2). 
#' 
plot_populations(farm.df.2)
#' **Fig 1.** Change in the number of susceptible, infected and recovered farms during a bovine FMD 
#' outbreak, assuming an Ro of 2 (transmission rate = 1, recovery rate = 1/2). 
#' 
#' ## Conclusion
#' 
#' As expected, in the lower transmission rate situation we see that the outbreak results in a lower peak 
#' value for the total number of infected farms, and a longer timeframe over which the outbreak occurs. 
#' We can see that, in comparison to the SIS model, in which we ended up with a stable number of infected 
#' and susceptible animals at equilibrium, in this case, since recovery from infection offers immunity 
#' to the herd, then we see that we end up with a very low number of infected animals once the infection 
#' peak has passed. 

