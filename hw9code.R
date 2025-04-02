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
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, rate=beta)), na.rm=T)
  
  return(ifelse(neg, -loglik, loglik))
}

(mles <- optim(par = c(1,1),
               fn = llgamma,
               data=dat.precip.long$Precipitation,
               neg=T))
gamma.ll <- -mles$value[1]
alpha.hat.mle <- mles$par[1]
beta.hat.mle <- mles$par[2]

#B: Compute the Mles for the data using a Log-Normal Distribution

lllnorm <- function(par, data, neg = F) {
  x <- data
  mu <- par[1]
  sigma <- par[2]
loglik <- sum(log(dlnorm(x = x, meanlog = mu, sdlog = sigma)), na.rm=T)
ifelse(neg,-loglik, loglik)
}

(mle.lnorm <- optim(fn = lllnorm, par = c(1,1), data = dat.precip.long$Precipitation, neg = T))
norm.ll <- -mle.lnorm$value[1]

#C:  Compute the likelihood ratio to compare the Weibull and the Gamma distribution
weibull.ll <- -2166.496
wg.ratio <- exp(weibull.ll-gamma.ll) 
#Because the liklihood ratio is extremely close to zero, we can say that the gamma distribution offers stronger support in representing the data than the weibull distribution does.

#D: Compute the liklihood ratio to compare the weibull and lognormal distribution
wl.ratio <- exp(weibull.ll-norm.ll) 
#In this case, the ratio is signifigantly above one, indicating that the weibull distribution offers much stronger support for the data than the normal distribution does.

#E:Compute the likelihood ratio to compare the Gamma and the Log-Normal distribution
gl.ratio <- exp(gamma.ll-norm.ll)
#This ratio is once again far above one, indicating that the Gamma distribution offers stronger support for the data then the log normal distribution does.
