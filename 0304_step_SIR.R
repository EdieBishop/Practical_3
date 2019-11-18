#' ---
#' title: "Function to model disease transmission using specified timesteps"
#' author: "Edie Bishop"
#' date: "13th of November 2019"
#' output: html_document
#' ---

#' File: 0303_timestep_SIS.r
#' =========================
#' 
#' 
#' ### Function: timestep_deterministic_SIS()
#' 
#' 
#' Arguments: 
#' 
#' - latest -- a dataframe that contains a single row containing the latest population size for each of 
#'             the component populations - in this case SUSCEPTIBLEs and INFECTEDs  
#' - transmission.rate -- transmission rate of the disease
#' - recovery.rate -- recovery rate from the disease  
#' - timestep -- the unit of time which we would like to model the population to 
#' 
#' Returns: 
#' 
#' - the updated population of susceptible, infected and recovered individuals and time as a dataframe 
#' 

timestep_deterministic_SIR <- function(latest, timestep, transmission.rate, recovery.rate)
{
  # First we need to define two new variables, which are the effective transmission and recovery rates 
  # (ie. the transmission and recovery rates multiplied by the timesteps)
  effective.transmission <- transmission.rate * timestep
  effective.recovery <- recovery.rate * timestep
  
  # define the total population 
  total.pop <- latest$SUSCEPTIBLE + latest$INFECTED 
  
  # define transition.rate.step, as this equation is used to calculate the number of infected and 
  # the number of susceptible we can define it once here rather than having to define it twice 
  transmission.rate.step <- (effective.transmission * latest$SUSCEPTIBLE * latest$INFECTED)/total.pop
  
  # define recovery.rate.step, as this equation is also used in calculating both the change is 
  # susceptibles and infecteds
  recovery.rate.step <- effective.recovery * latest$INFECTED
  
  # calculate the next value in the SUSCEPTIBLE varibale based on the previous value
  next.num.susceptible <- latest$SUSCEPTIBLE - transmission.rate.step 
  
  # calculate the next value in the INFECTED varaible based on the previous value 
  next.num.infected <- latest$INFECTED + transmission.rate.step - recovery.rate.step
  
  # We are here adding in an extra line, in which we calculate the number of animals which have 
  # recovered at each time step 
  next.num.recovered <- latest$RECOVERED + recovery.rate.step
  
  # calculate the next value in the TIME variable 
  next.timestep <- latest$time + timestep
  
  # create a dataframe which is only used within the function, which contains the latest value 
  # for SUSCEPTIBLE, INFECTED, RECOVERED and TIME variables 
  data.frame(SUSCEPTIBLES = next.num.susceptible, 
             INFECTED = next.num.infected, 
             RECOVERED = next.num.recovered,
             time = next.timestep)
}

#' Now we will check that our function name is unique, and also tell R that we do not want to generate 
#' a report unless our function name passes this test. 

library(codetools)
globals <- findGlobals(timestep_deterministic_SIR, merge = FALSE)$variables
if(length(globals) != 0)
{
  stop("Function timestep_deterministic_SIR() may not use global varibale(s): ",
       globals)
}
