library(tidyverse)
library(knitr)


# Factorial design -----------------------------------------------------

N_p <- 180 # number of participants 
N_c <- 6 # number of conditions

participants <-  expand_grid(
  
  # decision criteria: Expected value (EV), Frequent winner (FW)
  criterion = c(rep("EV", N_p/N_c), rep("FW", N_p/N_c)) , 
  
  # complexities: safe-risky (SR), risky-risky 2 outcomes (RR2), risky-risky 3 outcomes (RR3),
  complexity = c("SR", "RR2", "RR3")
  
  ) %>%
  
  mutate(id = row_number()) %>% 
  select(id, everything())


conditions <- participants %>% group_by(criterion, complexity) %>% summarise(N = n())


# Hypotheses (Generative simulation) --------------------------------------------------------------

# Hypothesis Set 1: Effects on switching

dat <- participants %>% mutate(
  
  
  switch = .4 + 
    
    # 1.1: Higher switching frequency for FW vs. EV
    if_else(criterion == "FW", .15, -.15) * 
    
    # 1.2: Effect of FW vs. EV increases with complexity
    if_else(complexity == "SR", 1.1, if_else(complexity == "RR2", 1.3, 1.5)) + 
    
    # unobserved influences
    rnorm(N, 0, .3)
  
)

# Hypothesis Set 2: Effects on accuracy

dat <- dat %>% mutate(
  
  accuracy = .5 + 
  
    # 2.1 Accuracy increases with decreasing complexity
    if_else(complexity == "SR", .3, if_else(complexity == "RR2", .2, .1)) + 
    
    # 2.2 Higher switching frequency leads to higher (lower) accuracy given the decision criterion is FW (EV)
    .2 * ifelse(criterion == "FW", switch,(1-switch)) + 
    
    # 2.3 Higher (low) switching frequency is punishes EV (FW) more in complex problems
    -1 * .2 * ifelse(criterion == "FW", (1-switch), switch) * if_else(complexity == "SR", 1.1, if_else(complexity == "RR2", 1.3, 1.5)) + 
  
    # unobserved influences
    rnorm(N, 0, .1)
  
)


# Simulation checks ----------------------------------------------------------------

# 1.1: Higher switching frequency for FW vs. EV
dat %>% 
  ggplot(aes(x=criterion, y=switch)) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,1)) 

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


    
