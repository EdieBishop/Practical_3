#' ---
#' title: "Function to model population growth, taking imto account both birth and death rates"
#' author: "Edie Bishop"
#' date: "8th of November 2019"
#' output: html_document
#' ---

#' File: 0301_step_SIS.r
#' =========================
#' 
#' We are now going to update our function so that rather than just taking one parameter, growth.rate, 
#' which will determine population change, we can now give the model two parameters, birth.rate and 
#' death.rate.   

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
  total.pop <- latest$SUSCEPTIBLE + latest$INFECTED
  
  transmission.rate.step <- (transmission.rate * latest$SUSCEPTIBLE * latest$INFECTED)/total.pop
  
  recovery.rate.step <- recovery.rate * latest$INFECTED
  
  next.num.susceptible <- latest$SUSCEPTIBLE - transmission.rate.step + recovery.rate.step
  
  next.num.infected <- latest$INFECTED + transmission.rate.step - recovery.rate.step
  
  data.frame(SUSCEPTIBLES = next.num.susceptible, INFECTED = next.num.infected)
}



library(codetools)
globals <- findGlobals(step_deterministic_SIS, merge = FALSE)$variables
if(length(globals) != 0)
{
  stop("Function step_deterministic_SIS() may not use global varibale(s): ",
       globals)
}
