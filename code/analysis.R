# Preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse ,
               brms , 
               tidybayes , 
               #ggdist ,
               ggpubr , 
               scico ,
               #bayesplot ,
               posterior
               )
               

# load data
dat <- read_rds("data/Study 2/choices.rds") %>% 
  mutate(partID = as.factor(partID) ,
         probID = as.factor(ID)
  )


# Models ------------------------------------------------------------------

## switch effects ----------------------------------------------------------

m1 <- brm(rightchoice ~ 1 + switchprob*aim + (1|partID) + (1|probID) ,
            data = dat ,
            family = bernoulli(link = "logit") , 
            iter = 2000 ,
            warmup = 1000 ,
            chains = 6 , 
            cores = 6
          #save_pars = save_pars(all = TRUE)
          )

# summary and convergence
summary(m1)
# get_variables(m1)
mcmc_trace(m1, 
           pars = c("b_Intercept", "b_switchprob" , "b_aimFW" , "b_switchprob:aimFW" , 
                        "sd_partID__Intercept" , "sd_probID__Intercept")
           )

# posterior

m1_posts <-  m1 %>% 
  spread_draws(b_Intercept, b_switchprob, b_aimFW, `b_switchprob:aimFW`) %>% # cols collect iter of parameters 
  rename(m_EV = b_Intercept , 
         off_m_FW = b_aimFW , 
         b_EV = b_switchprob ,
         off_b_FW =  `b_switchprob:aimFW`) %>% 
  mutate(m_FW = m_EV + off_m_FW , 
         b_FW = b_EV + off_b_FW , 
         odds_EV = exp(b_EV),
         odds_FW = exp(b_FW))

# posterior summary

m1_effects <-  m1_posts %>% 
  summarise_draws(default_summary_measures())


## switch behavior ---------------------------------------------------------

m2 <- brm(switchprob ~ 1 + aim + (1|probID) ,
          data = dat , 
          family = gaussian(link = "identity") , 
          iter = 2000 ,
          warmup = 1000 ,
          chains = 6 , 
          cores = 6 ,
          save_pars = save_pars(all = TRUE)
          )

# summary and convergence
summary(m2)
get_variables(m2)
mcmc_trace(m2, 
           pars = c("b_Intercept", "b_aimFW" , "sd_probID__Intercept")
)

# posterior summaries 

get_variables(m2)
m2_posts <- m2 %>%
  spread_draws(b_Intercept,  b_aimFW) %>% 
  rename(m_EV = b_Intercept ,
         off_FW = b_aimFW) %>% 
  mutate(m_FW = m_EV + off_FW, 
         diff = m_FW - m_EV)

# mSwitch_post %>% 
#   gather_draws(m_EV, m_FW, diff) %>% 
#   mean_qi()


## complexity -----------------------------------------------------

### safe vs. risky ----------------------------------------------------------

## Subset data
dat_fw <- dat %>% filter(aim == "FW")        # Subsetting the choice data
dat_ev <- dat %>% filter(aim == "EV")        # Subsetting the choice data


## switch effects short term
m3_fw <- brm(rightchoice ~ switchprob*env + (1|partID) + (1|probID) ,
                        data = dat_fw ,
                        family = bernoulli(link = "logit") , 
                        iter = 2000 ,
                        warmup = 1000 ,
                        chains = 6 , 
                        cores = 6)

### summary and convergence
# get_variables(m3)
summary(m3_fw)

### posterior
#get_variables(m3_ev)
m3_fw_posts <- m3_fw %>% 
  spread_draws(b_switchprob,  `b_switchprob:envsafe`) %>% 
  rename(b_FW_risky = b_switchprob , 
         off_safe =  `b_switchprob:envsafe`) %>% 
  mutate(b_FW_safe = b_FW_risky + off_safe , 
         odds_FW_risky = exp(b_FW_risky),
         odds_FW_safe = exp(b_FW_safe))
# 
# posts %>% 
#   gather_draws(odds_FW_risky, odds_FW_safe) %>% 
#   mean_qi()


## switch effects long term
m3_ev <- brm(rightchoice ~ switchprob*env + (1|partID) + (1|probID) ,
                        data = dat_ev ,
                        family = bernoulli(link = "logit") , 
                        iter = 2000 ,
                        warmup = 1000 ,
                        chains = 6 , 
                        cores = 6)

### summary and convergence
summary(m3_ev)

### posterior
#get_variables(m3_ev)
m3_ev_posts <- m3_ev %>% 
  spread_draws(b_switchprob,  `b_switchprob:envsafe`) %>% 
  rename(b_EV_risky = b_switchprob , 
         off_safe =  `b_switchprob:envsafe`) %>% 
  mutate(b_EV_safe = b_EV_risky + off_safe , 
         odds_EV_risky = exp(b_EV_risky),
         odds_EV_safe = exp(b_EV_safe))



### all ---------------------------------------------------------------------

# add model

# Results -------------------------------------------------------------------

## switch effects ----------------------------------------------------------

# data + effects

m1_reg_m <- m1_effects %>% 
  select(variable, mean) %>% 
  filter(variable %in% c("m_EV", "b_EV", "m_FW", "b_FW")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(prob_EV = round(plogis(m_EV + b_EV * rate), 3) , 
         prob_FW = round(plogis(m_FW + b_FW * rate), 3)) %>% 
  pivot_longer(cols=c(prob_EV, prob_FW), names_to = "aim", values_to = "pred") %>% 
  mutate(aim = case_when(aim == "prob_EV" ~ "Long term" ,
                         aim == "prob_FW" ~ "Short term"))

m1_reg_95 <- m1_effects %>% 
  select(variable, q95) %>% 
  filter(variable %in% c("m_EV", "b_EV", "m_FW", "b_FW")) %>%
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(prob_EV = round(plogis(m_EV + b_EV * rate), 3) , 
         prob_FW = round(plogis(m_FW + b_FW * rate), 3)) %>% 
  pivot_longer(cols=c(prob_EV, prob_FW), names_to = "aim", values_to = "pred") %>% 
  mutate(aim = case_when(aim == "prob_EV" ~ "Long term" ,
                         aim == "prob_FW" ~ "Short term"))

m1_reg_5 <- m1_effects %>%
  select(variable, q5) %>% 
  filter(variable %in% c("m_EV", "b_EV", "m_FW", "b_FW")) %>%
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(prob_EV = round(plogis(m_EV + b_EV * rate), 3) , 
         prob_FW = round(plogis(m_FW + b_FW * rate), 3)) %>% 
  pivot_longer(cols=c(prob_EV, prob_FW), names_to = "aim", values_to = "pred") %>% 
  mutate(aim = case_when(aim == "prob_EV" ~ "Long term" ,
                         aim == "prob_FW" ~ "Short term") , 
         pred95 = m1_reg_95$pred)

statSub <- dat %>%
  select(partID, participant, age, gender, aim, CP, effort, switchprob, rightchoice, ID) %>% 
  group_by(partID, participant, age, gender, aim, CP) %>% 
  summarize(
    nChoice = n() , 
    meanEffort = round(mean(effort), 2) , 
    meanSwitching = round(mean(switchprob), 2) ,
    meanAccuracy = round(mean(rightchoice), 2) ,
  ) %>% 
  ungroup() %>% 
  mutate(aim = case_when(aim == "prob_EV" ~ "Long term" ,
                         aim == "prob_FW" ~ "Short term"))


p_m1_effects <- statSub %>% 
  ggplot(aes(color=aim, fill=aim)) +
  geom_ribbon(data = m1_reg_5, aes(x = rate, ymin = pred, ymax = pred95), alpha = 0.5) +
  geom_point(aes(x=meanSwitching, meanAccuracy), size = 3, alpha = .5) + 
  geom_line(data = m1_reg_m, aes(x=rate, y=pred), linewidth = 1.5) +
  geom_line(data = m1_reg_5, aes(x=rate, y=pred), linewidth = .3) +
  geom_line(data = m1_reg_95, aes(x=rate, y=pred), linewidth = .3) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  labs(x = "Switch Rate",
       y = "Choice Accuracy",
       color = "Decision Goal",
       fill = "Decision Goal") +
  scale_fill_scico_d(palette = "managua" , begin = .1, end = .9) + 
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) +
  theme_pubr(base_size = 20) + 
  theme(legend.position = "bottom") 
p_m1_effects

# posteriors

p_m1_posts <- m1_posts %>% 
  gather_draws(b_EV, b_FW) %>% 
  ggplot(aes(x=.value, y =.variable, fill = .variable)) +
  stat_halfeye(.width = c(.95, .8, .5), interval_size_range = c(1,3), alpha = .8) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) + 
  labs(y = "Posterior Density" ,
       x = expression(beta),
       fill = "", 
       color = "") +
  scale_y_discrete(labels=c("Long", "Short"), limits = rev) +
  scale_x_continuous(limits = c(-1.1,1.1)) +
  scale_fill_scico_d(palette = "managua" , begin = .1, end = .9) + 
  theme_pubr(base_size = 20) + 
  theme(legend.position = "none",
        axis.text.y = element_blank() , 
        axis.ticks.y = element_blank())
p_m1_posts

## switch behavior -------------------------------------------------------------------------

# Data + Effects

switch_means <- statSub %>% group_by(aim) %>% 
  summarise(m = mean(meanSwitching) ,
            sd = sd(meanSwitching) , 
            n = n(), 
            se = sd(meanSwitching)/sqrt(n())) %>% 
  mutate(aim = case_when(aim == "EV" ~ "Long term" ,
                         aim == "FW" ~ "Short term"))

p_m2_effects <- dat %>% 
  mutate(aim = case_when(aim == "EV" ~ "Long term" ,
                         aim == "FW" ~ "Short term")) %>% 
  ggplot(aes(x=aim, y=switchprob, color = aim)) + 
  geom_jitter(size=1, alpha = .2, width = .1) +
  geom_boxplot(width = .2, color = "black", outliers = F, alpha = .1, notch = F) +
  geom_point(data=switch_means, aes(y=m),  color = "black", size=5) + 
  labs(x = "Decision Goal", 
       y = "Switch Rate",
       color = "Decision Goal") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_x_discrete(labels=c("Long term", "Short term")) +
  scale_fill_scico_d(palette = "managua" , begin = .1, end = .9) + 
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) +
  theme_pubr(base_size = 16) +   
  theme(legend.position = "none") 

#test_marginal <- ggMarginal(test, type="density", groupFill = TRUE, alpha = .8, margins = "y")

# posterior

p_m2_posts <- m2_post %>%
  ggplot(aes(x=diff)) +
  stat_halfeye(.width = c(.95, .8, .5), interval_size_range = c(1,3), alpha = .8, fill = "#6CB0DD") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) + 
  labs(y = "Posterior Density" ,
       x = expression(beta[Short] - beta[Long])) + 
  scale_y_continuous(limits = c(-.5,1.1)) + 
  scale_x_continuous(limits = c(-.1,.1)) + 
  theme_pubr(base_size = 16)  +
  theme(legend.position = "none", 
         axis.text.y = element_blank() , 
        axis.ticks.y = element_blank()
        )

## complexity --------------------------------------------------------------

### safe vs. risky ----------------------------------------------------------

p_m3_ev_posts <- m3_ev_posts %>% 
  gather_draws(odds_EV_risky, odds_EV_safe) %>% 
  ggplot(aes(x=.value, y =.variable, group = .variable, fill = .variable)) +
  stat_halfeye(alpha = .5) + 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  scale_y_discrete(labels=c("Risky", "Safe")) +
  scale_fill_manual(values=c("#c80896", "#ffccff"  )) +
  labs(y = "Posterior Density" ,
       x = expression(e^b["Switch, Environment"]),
       fill = ""
  ) + 
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")

p_m3_fw_posts <- m3_fw_posts %>% 
  gather_draws(odds_FW_risky, odds_FW_safe) %>% 
  ggplot(aes(x=.value, y =.variable, group = .variable, fill = .variable)) +
  stat_halfeye(alpha = .5) + 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  scale_y_discrete(labels=c("Risky", "Safe")) +
  scale_fill_manual(values=c("#fb9b06", "#ffff66"  )) +
  labs(y = "Posterior Density" ,
       x = expression(e^b["Switch, Environment"]),
       fill = ""
  ) + 
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")



### all ---------------------------------------------------------------------


# add model






# Supplements ------------------------------------------------------------


## accuracy ----------------------------------------------------------------

### model -------------------------------------------------------------------

mAccuracy <- brm(rightchoice ~ aim + (1|partID) + (1|probID) ,
                 data = dat ,
                 family = bernoulli(link="logit") ,
                 iter = 2000 ,
                 warmup = 1000 , 
                 chains = 6 , 
                 cores = 6)

# summary and convergence
summary(mAccuracy)
# get_variables(mAccuracy)

# posterior
mAccuracy_post <- mAccuracy %>% 
  spread_draws(b_Intercept,  b_aimFW) %>% 
  rename(m_EV = b_Intercept ,
         off_FW = b_aimFW) %>% 
  mutate(m_FW = m_EV + off_FW, 
         diff = exp(m_EV - m_FW))

mAccuracy_post %>% 
  gather_draws(m_EV, m_FW, diff) %>% 
  mean_qi()


### results -----------------------------------------------------------------

# effects

# dAccuracy <-  statSub %>% 
#   group_by(aim) %>% 
#   summarise(m = mean(meanAccuracy) ,
#             sd = sd(meanAccuracy) , 
#             n = n(), 
#             se = sd(meanAccuracy)/sqrt(n()))
# dAccuracy

statSub %>% 
  ggplot(aes(x=aim, y=meanAccuracy, color = aim, fill = aim)) + 
  geom_violin(width = .25, alpha = .3) +
  geom_beeswarm(size=3, alpha = .2) +
  geom_boxplot(width = .1, color = "black", alpha = .3) +
  geom_point(data=dAccuracy, aes(y=m),  color = "black", size=4, shape = 18) + 
  geom_hline(yintercept = .5, linetype = "dashed", linewidth = 1) +
  labs(x = "Decision Goal", 
       y = "Mean Accuracy") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_x_discrete(labels=c("Long", "Short")) +
  # scale_color_manual(values = values) +
  # scale_fill_manual(values = values) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")

mAccuracy_post %>% 
  ggplot(aes(x=diff)) +
  stat_halfeye(point_interval = "mean_qi", fill = "#c80896", alpha = .5) + 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  labs(x = expression("Odds Ratio:  " * frac("Long", "Short")) , 
       y = "Posterior Density") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 16)
ggsave("talks/figures/accuracy_model.png", height = 5, width = 7)





## add analyses ------------------------------------------------------------

