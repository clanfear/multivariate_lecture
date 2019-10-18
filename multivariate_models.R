## ---- , fig.width=8, fig.height=4, warning=FALSE, message=FALSE, fig.align="center"----
library(dplyr); library(rethinking); set.seed(7); data(foxes)
foxes_std <- mutate_at(foxes, vars(-group), ~ scale(.))
m_1 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area + b2*groupsize + b3*avgfood, #<<
    a             ~ dnorm( 0, 100),
    c(b1, b2, b3) ~ dnorm(0, 2), #<<
    sigma         ~ dunif(0, 10)
  ), data=foxes_std
)
precis( m_1)


## ---- fig.width=8, fig.height=4, message=FALSE, warning=FALSE, fig.align="center"----
plot(precis(m_1))


## ---- fig.width=6, fig.height=4, message=FALSE, warning=FALSE, fig.align="center"----
foxes_std <- foxes_std %>% mutate(., spur_y = rnorm(nrow(.), weight, 1),
                                     spur_x = rnorm(nrow(.), weight, 1))
m_sp_1 <- map(
  alist(
    spur_y ~ dnorm(mu, sigma) ,
    mu <- a + b1*spur_x, 
    a     ~ dnorm( 0, 100),
    b1    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_sp_1))


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_sp_2 <- map(
  alist(
    spur_y ~ dnorm(mu, sigma) ,
    mu <- a + b1*weight + b2*spur_x, 
    a         ~ dnorm( 0, 100),
    c(b1, b2) ~ dnorm(0, 2), 
    sigma     ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_sp_2))


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_sp_3 <- map(
  alist(
    spur_y ~ dnorm(mu, sigma) ,
    mu <- a + b1*weight, 
    a     ~ dnorm( 0, 100),
    b1    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_sp_3))


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_sp_1 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area, 
    a     ~ dnorm( 0, 100),
    b1    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
precis(m_sp_1)


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_sp_2 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b2*groupsize, 
    a     ~ dnorm( 0, 100),
    b2    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
precis(m_sp_2)


## ---- fig.width=6, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_sp_3 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area + b2*groupsize, 
    a         ~ dnorm( 0, 100),
    c(b1, b2) ~ dnorm(0, 2), 
    sigma     ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_sp_3))


## ---- fig.width=6, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_mc_1 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*avgfood + b2*area, 
    a         ~ dnorm( 0, 100),
    c(b1, b2) ~ dnorm(0, 2), 
    sigma     ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_mc_1))
cor(foxes_std)["avgfood", "area"]


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_mc_2 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b1*area, 
    a     ~ dnorm( 0, 100),
    b1    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_mc_2))


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_mc_3 <- map(
  alist(
    weight ~ dnorm(mu, sigma) ,
    mu <- a + b2*avgfood, 
    a     ~ dnorm( 0, 100),
    b2    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_mc_3))


## ---- fig.width=6, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
foxes_std <- foxes_std %>% 
  mutate(., pt_z = rnorm(nrow(.), weight, 1),
            pt_y = rnorm(nrow(.), 0.4*pt_z + 0.1*weight, 0.25))
m_pt_1 <- map(
  alist(
    pt_y ~ dnorm(mu, sigma) ,
    mu <- a + b1*pt_z + b2*weight, 
    a         ~ dnorm( 0, 100),
    c(b1, b2) ~ dnorm(0, 2), 
    sigma     ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_pt_1))



## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_pt_2 <- map(
  alist(
    pt_y ~ dnorm(mu, sigma) ,
    mu <- a + b2*weight, 
    a     ~ dnorm( 0, 100),
    b2    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_pt_2))


## ---- fig.width=4, fig.height=3, message=FALSE, warning=FALSE, fig.align="center"----
m_pt_3 <- map(
  alist(
    pt_y ~ dnorm(mu, sigma) ,
    mu <- a + b2*pt_z, 
    a     ~ dnorm( 0, 100),
    b2    ~ dnorm(0, 2), 
    sigma ~ dunif(0, 10)
  ), data=foxes_std)
plot(precis(m_pt_3))


## ---- fig.width=5, fig.height=4, message=FALSE, warning=FALSE, fig.align="center", fig.keep='last', results="hide"----
wt_seq <- seq(-2.5,2.5,0.01)
mu <- link(m_pt_2, data=list(weight = wt_seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.95)
weight.sim <- sim(m_pt_2, data=list(weight = wt_seq))
weight.HPDI <- apply(weight.sim, 2, HPDI, prob=0.95)
plot(pt_y ~ weight, data=foxes_std)
lines( wt_seq, mu.mean)
shade( mu.HPDI, wt_seq)
shade( weight.HPDI, wt_seq)


## ---- fig.width=5, fig.height=4, message=FALSE, warning=FALSE, fig.align="center", fig.keep='last', results="hide"----
pt_z_seq <- seq(-2.5,2.5,0.01)
mu <- link(m_pt_3, data=list(pt_z = pt_z_seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.95)
pt_z.sim <- sim(m_pt_3, data=list(pt_z = pt_z_seq))
pt_z.HPDI <- apply(pt_z.sim, 2, HPDI, prob=0.95)
plot(pt_y ~ pt_z, data=foxes_std)
lines( pt_z_seq, mu.mean)
shade( mu.HPDI, pt_z_seq)
shade( pt_z.HPDI, pt_z_seq)

