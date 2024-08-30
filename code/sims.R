pacman::p_load(tidyverse, brms)

# Factorial design -----------------------------------------------------

N_p <- 180 # number of participants 
N_c <- 6 # number of conditions

# participants are randomly assigned to 1 of 6 conditions 

participants <-  expand_grid(
  
  # decision criteria: Expected value (EV), Frequent winner (FW)
  criterion = c(rep("EV", N_p/N_c), rep("FW", N_p/N_c)) , 
  
  # complexities: safe-risky (SR), risky-risky 2 outcomes (RR2), risky-risky 3 outcomes (RR3),
  complexity = c("SR", "RR2", "RR3")
  
  ) %>%
  
  mutate(id = row_number()) %>% 
  select(id, everything())

#factorial design

participants %>% 
  group_by(criterion, complexity) %>% 
  summarise(N = n())


# Hypotheses (Generative simulation) --------------------------------------------------------------

# define logistic function to constrain switch and accuracy to 0 to 1
logistic <- function(x, phi=1) {
  1 / (1 + exp(-phi*x))
}

# Each participants solves 40 problems 

dat <- participants %>% uncount(40)

nrow(dat) # 7200 decisions 

# 1200 decisions per condition
dat %>% 
  group_by(criterion, complexity) %>% 
  summarise(N = n())


# Hypothesis Set 1: Effects on switching

set.seed(123134)
dat <- dat %>% mutate(
  
  
  switch = logistic(0 + 
    
    # 1.1: Higher switching frequency for FW vs. EV
    if_else(criterion == "FW", .5, -.5) * 
    
    # 1.2: Effect of FW vs. EV increases with complexity
    if_else(complexity == "SR", 1.2, if_else(complexity == "RR2", 1.5, 1.8)) + 
    
    # unobserved influences
    rnorm(N_p, 0, .5)
    
    )
)

# Hypothesis Set 2: Effects on accuracy


set.seed(343891)
dat <- dat %>% mutate(
  
  accuracy = logistic( 0 + 
  
    # 2.1 Accuracy decreases with increasing complexity
    if_else(complexity == "SR", 1.5, if_else(complexity == "RR2", 1, .5)) - 
    
    # 2.2 Higher switching frequency leads to higher (lower) accuracy given the decision criterion is FW (EV)
    if_else(criterion == "FW", (1-switch) , switch) * 
    
    # 2.3 Higher (low) switching frequency punishes EV (FW) more in complex problems
    if_else(complexity == "SR", 1, if_else(complexity == "RR2", 2, 3)) +
  
    # unobserved influences
    rnorm(N_p, 0, .5)
    
    )
  )

set.seed(8897)
dat <- dat %>% mutate(
  
  choice = rbinom(nrow(dat),1,accuracy)
  
)

dat <- dat %>% mutate(id=as.factor(id), 
                      criterion = as.factor(criterion), 
                      complexity = as.factor(complexity))

# Simulation checks ----------------------------------------------------------------

# 1.1: Higher switching frequency for FW vs. EV
dat %>% 
  ggplot(aes(x=criterion, y=switch)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) + 
  theme_minimal()

# 1.2: Effect of FW vs. EV increases with complexity
dat %>% 
  ggplot(aes(x=criterion, y=switch, fill = factor(complexity, levels = c("SR", "RR2", "RR3")))) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(fill = "Complexity") +
  theme_minimal()


# 2.1 Accuracy increases with decreasing complexity
dat %>% 
  ggplot(aes(x=factor(complexity, levels = c("SR", "RR2", "RR3")), y=accuracy)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Complexity") + 
  theme_minimal()


# 2.2 Higher switching frequency leads to higher (lower) accuracy given the decision criterion is FW (EV)
dat %>% 
  ggplot(aes(x=switch, y=accuracy, color = criterion)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(limits = c(0,1)) + 
  theme_minimal()

# 2.3 Higher (low) switching frequency punishes EV (FW) more in complex problems

dat %>% 
  ggplot(aes(x=switch, y=accuracy, color = criterion, linetype = factor(complexity, levels = c("SR", "RR2", "RR3")))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  facet_wrap(~criterion) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(linetype = "Complexity") + 
  theme_minimal()

# Statistical Models ------------------------------------------------------------------

# 1.1: Higher switching frequency for FW vs. EV

m1.1 <- brm(switch ~ criterion + (1|id) ,
            data = dat , 
            family = Beta(link = "logit") , 
            chains = 4 , 
            iter = 2000 ,
            cores = 4
            )
summary(m1.1)

# 1.2: Effect of FW vs. EV increases with complexity

m1.2 <- brm(switch ~ criterion:complexity + (1|id) ,
            data = dat ,
            family = Beta(link = "logit") ,
            iter = 2000 , 
            chains = 4 , 
            cores = 4
            )
summary(m1.2.1)



# 2.1 Accuracy increases with decreasing complexity

m2.1 <- brm(choice ~ complexity + (1|id) ,
            data = dat , 
            family = bernoulli(link = "logit") , 
            iter = 2000 , 
            chains = 4 , 
            cores = 4
)
summary(m2.1)

# 2.2 Higher switching frequency leads to higher (lower) accuracy given the decision criterion is FW (EV)


m2.2 <- brm(choice ~ switch:criterion + (1|id) ,
              data = dat , 
              family = bernoulli(link = "logit") , 
              iter = 2000 , 
              chains = 4 , 
              cores = 4
)
summary(m2.2.1)



# 2.3 Higher (low) switching frequency punishes EV (FW) more in complex problems

dat_fw <- dat %>% filter(criterion == "FW")
dat_ev <- dat %>% filter(criterion == "EV")


m2.3.1 <- brm(choice ~ switch:complexity + (1|id) ,
            data = dat_fw , 
            family = bernoulli(link = "logit") , 
            iter = 4000 , 
            chains = 4 , 
            cores = 4
)
summary(m2.3.1)


m2.3.2 <- brm(choice ~ switch:complexity + (1|id) ,
              data = dat_ev , 
              family = bernoulli(link = "logit") , 
              iter = 4000 , 
              chains = 4 , 
              cores = 4
)
summary(m2.3.2)
