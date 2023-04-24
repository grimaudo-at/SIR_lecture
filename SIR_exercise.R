#install.packages("deSolve")
#install.packages("ggplot2")
#install.packages("tidyr")
#Install these packages if you haven't already. 
#'deSolve' is a package for numerically solving differential equations. We will use it to "solve" our SIR models. 
#'ggplot2' is a package for visualizing data. We will use it to plot the results of our SIR models. 
#'tidyr' is a package for data cleaning and organizing. We will use its 'pivot_longer' function to re-shape our simulated data for plotting. 
library(deSolve)
library(ggplot2)
library(tidyr)

# Define SIR model function
SIR <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- #Need an equation!
    dI <- #Need an equation!
    dR <- #Need an equation!
    return(list(c(dS, dI, dR)))
  })
}

#Define how long we want to simulate. Let's choose 100 with time steps of 0.01. This could be 100 days, weeks, hours, whatever. 
#REMEMBER that you cannot simply change between units of time without also changing your parameters (beta, gamma) because those 
#are time unit-specific parameters. 
times <- seq(0, 400, by = 0.01)

#Define initial state sizes. These are in units of the proportion of the population.
initial.I <- 0.0001
initial.R <- 0
initial.S <- 1-initial.I-initial.R

# Set initial values and parameters
initial.values <- c(S = initial.S, I = initial.I, R = initial.R) 
parameters <- c(beta = 0.4, gamma = 0.1)

#The function ode() comes from the deSolve package that you loaded at the beginning of the script. It 'solves' your equation, but you need to give
#it your model in form of a function (your SIR() function), the starting values, the parameters, and the time series you want to simulate on. 
#In the below code, we solve the equation, save it as a dataframe called 'output', and change it from wide to long format. 
output<- as.data.frame(ode(func = SIR, y = initial.values, parms = parameters, times = times)) %>%
  pivot_longer(!time, names_to="state",values_to="proportion") 
#View(output)

#Now we can plot the model results: 
SIR.plot <- ggplot(aes(x=time, y=proportion, color=state), data=output)+
  geom_line()+
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1))+
  scale_color_manual(values=c("red2","blue3","green4"));SIR.plot





### Adding vaccination class with waning immunity:
