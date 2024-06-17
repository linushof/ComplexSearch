pacman::p_load(tidyverse)

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

dat <- dat %>% mutate(
  
  
  switch = logistic(0 + 
    
    # 1.1: Higher switching frequency for FW vs. EV
    if_else(criterion == "FW", .5, -.5) * 
    
    # 1.2: Effect of FW vs. EV increases with complexity
    if_else(complexity == "SR", 1.2, if_else(complexity == "RR2", 1.5, 1.8)) + 
    
    # unobserved influences
    rnorm(N_p, 0, 1)
  )
)

# Hypothesis Set 2: Effects on accuracy



dat <- dat %>% mutate(
  
  accuracy = logistic( 0 + 
  
    # 2.1 Accuracy increases with decreasing complexity
    if_else(complexity == "SR", 1.5, if_else(complexity == "RR2", 1, .5)) + 
    
    # 2.2 Higher switching frequency leads to higher (lower) accuracy given the decision criterion is FW (EV)
    ifelse(criterion == "FW", switch,(1-switch)) - 
    
    # 2.3 Higher (low) switching frequency is punishes EV (FW) more in complex problems
    1 * ifelse(criterion == "FW", (1-switch), switch) * if_else(complexity == "SR", 1.1, if_else(complexity == "RR2", 1.3, 1.5)) + 
  
    # unobserved influences
    rnorm(N_p, 0, 1) , 
    
    phi=.5)
  )

dat <- dat %>% mutate(
  
  choice = rbinom(nrow(dat),1,accuracy)
  
)

View(dat)
# Simulation checks ----------------------------------------------------------------

# 1.1: Higher switching frequency for FW vs. EV
dat %>% 
  ggplot(aes(x=criterion, y=switch)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) 


dat %>% 
  ggplot(aes(x=switch, fill=criterion)) + 
  geom_density(alpha = .5)

# 1.2: Effect of FW vs. EV increases with complexity
dat %>% 
  ggplot(aes(x=criterion, y=switch, fill = factor(complexity, levels = c("SR", "RR2", "RR3")))) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(fill = "Complexity")


# 2.1 Accuracy increases with decreasing complexity
dat %>% 
  ggplot(aes(x=factor(complexity, levels = c("SR", "RR2", "RR3")), y=accuracy)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) 


# 2.2 Higher switching frequency leads to higher (lower) accuracy given the decision criterion is FW (EV)
dat %>% 
  ggplot(aes(x=switch, y=accuracy, color = criterion)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(limits = c(0,1))

# 2.3 Higher (low) switching frequency punishes EV (FW) more in complex problems

dat %>% 
  ggplot(aes(x=switch, y=accuracy, color = criterion, linetype = factor(complexity, levels = c("SR", "RR2", "RR3")))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~criterion) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(linetype = "Complexity")

# Statistical Models ------------------------------------------------------------------


