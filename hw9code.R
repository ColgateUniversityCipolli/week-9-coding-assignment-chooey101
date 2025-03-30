library(tidyverse)

mor.data <- read_csv("agacis.csv") 
#Clean the data

dat.precip.long <- mor.data |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation))

#A: Compute the MLEs for these data using a Gamma distribution.

llgamma <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, rate=beta)))
  
  return(ifelse(neg, -loglik, loglik))
}

(mles <- optim(par = c(1,1),
               fn = llgamma,
               data=dat.precip.long$Precipitation,
               neg=T))
alpha.hat.mle <- mles$par[1]
beta.hat.mle <- mles$par[2]

#B: Compute the Mles for the data using a Log-Normal Distribution

lllnorm <- function(par, data, neg = F) {
  x <- data
  mu <- par[1]
  sigma <- par[2]
loglik <- sum(log(dlnorm(x = x, meanlog = mu, sdlog = sigma)))
ifelse(neg,-loglik, loglik)
}

(mle.lnorm <- optim(fn = lllnorm, par = c(1,1), data = dat.precip.long$Precipitation, neg = T))