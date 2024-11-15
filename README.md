# Logistic Growth

R scripts for a reproducible analysis of logistic growth

## Plotting the logistic growth data

```{r}
install.packages("ggplot2")
library(ggplot2)

growth_data <- read.csv("experiment.csv")

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  theme_bw()

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  scale_y_continuous(trans='log10') +
  
  theme_bw()
```

## Fitting a Linear Model 

```{r}
library(dplyr)

growth_data <- read.csv("experiment.csv")

#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1500) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)

#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>2500)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)
```

In this section, we have used a linear approximation for the following equation.

N(t) = N0e\^(rt)

This linear approximation has been achieved by performing a log transformation, to get the equation in the form y = c + mx.

ln(N(t)) = ln(N0e\^(rt))

ln(N) = ln(N0) + rtln(e)

ln(N) = ln(N0) + rt

There are two cases to consider. The first is where we restrict ourselves to the region of exponential growth (K \>\> N0 and t is small).

In this case ln(N) = ln(N0) + rt.

Using our linear model in Case 1 above, we can see that the estimate for the intercept (ln(N0)) is equal to approximately 6.89. Therefore our approximation for N0 is equal to e\^6.89 which is approximately 982. The estimate for the gradient (r) is equal to 0.01. Therefore our approximation for r is 0.01.

In the second case where t is large and the population size remains constant, we can use the following approximation to calculate K (seen in our linear model in Case 2 above)

N(t) = K + 0\*t

N(t) = K

Therefore our estimate for K is 6.00e+10.

## Plotting Data and Model

Using the estimated values of N0, r and k from our linear model approximation above, we can then plot (in red) the results of our model onto the original data set, to see how well our approximation fits the actual data.

```{r}
logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

N0 <- 982.401417 #
  
r <- 0.0100086 #
  
K <- 60000000000 #

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point()

  #scale_y_continuous(trans='log10')
```
