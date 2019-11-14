#' ---
#' title: "Function to model disease transmission"
#' author: "Edie Bishop"
#' date: "13th of November 2019"
#' output: html_document
#' ---

#' File: 0301_step_SIS.r
#' =========================
#' 
#' 
#' ### Function: step_deterministic_birth_death()
#' 
#' 
#' Arguments: 
#' 
#' - latest -- a dataframe that contains a single row containing the latest population size for each of 
#'             the component populations - in this case SUSCEPTIBLEs and INFECTEDs  
#' - transmission.rate -- transmission rate of the disease
#' - recovery.rate -- recovery rate from the disease  
#' 
#' Returns: 
#' 
#' - the updated population of susceptible and infected individuals as a dataframe 
#' 

step_deterministic_SIS <- function(latest, transmission.rate, recovery.rate)
{
  # define the total population 
  total.pop <- latest$SUSCEPTIBLE + latest$INFECTED
  # define transition.rate.step, as this equation is used to calculate the number of infected and 
  # the number of susceptible we can define it once here rather than having to define it twice 
  transmission.rate.step <- (transmission.rate * latest$SUSCEPTIBLE * latest$INFECTED)/total.pop
  # define recovery.rate.step, as this equation is also used in calculating both the change is 
  # susceptibles and infecteds
  recovery.rate.step <- recovery.rate * latest$INFECTED
  # calculate the next value in the SUSCEPTIBLE varibale based on the previous value
  next.num.susceptible <- latest$SUSCEPTIBLE - transmission.rate.step + recovery.rate.step
  # calculate the next value in the INFECTED varaible based on the previous value 
  next.num.infected <- latest$INFECTED + transmission.rate.step - recovery.rate.step
  # create a dataframe which is only used within the function, which contains the latest value 
  # for SUSCEPTIBLE and INFECTED variables 
  data.frame(SUSCEPTIBLES = next.num.susceptible, INFECTED = next.num.infected)
}

#' Now we will check that our function name is unique, and also tell R that we do not want to generate 
#' a report unless our function name passes this test. 

library(codetools)
globals <- findGlobals(step_deterministic_SIS, merge = FALSE)$variables
if(length(globals) != 0)
{
  stop("Function step_deterministic_SIS() may not use global varibale(s): ",
       globals)
}
