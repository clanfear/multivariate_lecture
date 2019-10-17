## ---- include=FALSE------------------------------------------------------
knitr::purl("./multivariate_models.Rmd")


## ---- warning=FALSE, message=FALSE---------------------------------------
library(dplyr); library(rethinking);  data(foxes)
foxes_std <- mutate_at(foxes, vars(-group), ~ scale(.))
m_1 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area + b2*groupsize + b3*avgfood, #<<
    a ~ dnorm( 0, 100),
    c(b1, b2, b3) ~ dnorm(0, 2), #<<
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
precis( m_1)


## ---- fig.width=8, fig.height=4, fig.align="center"----------------------
plot(precis(m_1))


## ---- fig.width=4, fig.height=3, fig.align="center"----------------------
m_sp_1 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area, 
    a ~ dnorm( 0, 100),
    b1 ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
precis(m_sp_1)


## ---- fig.width=4, fig.height=3, fig.align="center"----------------------
m_sp_2 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b2*groupsize, 
    a ~ dnorm( 0, 100),
    b2 ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
precis(m_sp_2)


## ---- fig.width=6, fig.height=3, fig.align="center"----------------------
m_sp_3 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area + b2*groupsize, 
    a ~ dnorm( 0, 100),
    c(b1, b2) ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
plot(precis(m_sp_3))


## ---- fig.width=6, fig.height=3, fig.align="center"----------------------
m_mc_1 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*avgfood + b2*area, 
    a ~ dnorm( 0, 100),
    c(b1, b2) ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
plot(precis(m_mc_1))
cor(foxes_std)["avgfood", "area"]


## ---- fig.width=4, fig.height=3------------------------------------------
m_mc_2 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area, 
    a ~ dnorm( 0, 100),
    b1 ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
plot(precis(m_mc_2))


## ---- fig.width=4, fig.height=3------------------------------------------
m_mc_3 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b2*avgfood, 
    a ~ dnorm( 0, 100),
    b2 ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std
)
plot(precis(m_mc_3))


## ------------------------------------------------------------------------


