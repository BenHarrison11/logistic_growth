install.packages("ggplot2")
library(ggplot2)

growth_data <- read.csv("experiment.csv")

#Below are the parameters that will be substituted into both functions which are being plotted

N0 <- 982 #This is the initial population size
r <- 0.01 #This is the growth rate
t <- seq(0, 5000, by = 0.1) #This will give me the sequence of t values that are being inputted into the function

#Plotting the logistic growth curve
logistic_growth_plot <- ggplot() +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point() +
  
  xlim(0, 5000) +
  
  xlab("Time (minutes)") +
  
  ylab("Population Size") +
  
  ggtitle("Logistic Growth Model") +
  
  theme(plot.title = element_text(hjust = 0.5))


#Plotting the exponential growth curve involves first generating a function for exponential growth

exponential_growth <- function(t) {
  
  Nt <- N0*exp(r*t)
  
  return(Nt)
  
}

#Plotting exponential growth

exponential_growth_plot <- ggplot() +
  
  geom_function(fun=exponential_growth, colour = "black") +
  
  xlim(0, 5000) +
  
  xlab("Time (minutes)") +
  
  ylab("Population Size") +
  
  ggtitle("Exponential Growth Model") +
  
  theme(plot.title = element_text(hjust = 0.5)) 




#It would be more useful to have the functions plotted on top of each other for easier comparisons


Combined_plot <- ggplot() +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_function(fun=exponential_growth, colour ='black') +
  
  xlim(0, 5000) +
  
  scale_y_continuous(trans='log10') + 
  
  xlab("Time (minutes)") +
  
  ylab("Population Size") +
  
  ggtitle("Figure 8: A Comparative Plot") +
  
  theme(plot.title = element_text(hjust = 0.5))


#Finally I will show all of these graphs next to each other 
#Using the gridExtra package
install.packages("gridExtra")
install.packages("grid")
library(gridExtra)
library(grid)
grid.arrange(
  logistic_growth_plot, exponential_growth_plot,                
  Combined_plot,                    
  layout_matrix = rbind(c(1,2),   
                        c(3)), 
  top = textGrob("Figure 7: Comparing Logistic and Exponential Growth Models", gp = gpar(fontsize = 14, fontface = "bold"))
)