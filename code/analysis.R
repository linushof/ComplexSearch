
# Preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse ,
               ggsci , 
               ggdist , 
               tidybayes , 
               gridExtra , 
               ggpubr , 
               ggbeeswarm,
               viridis,
               viridisLite,
               brms , 
               bayesplot , 
               bayestestR , 
               lm.beta , 
               car , 
               data.table , 
               scico ,
               modelr
               )
               

# load data
dat <- read_rds("data/choices.rds") %>% 
  mutate(partID = as.factor(partID) ,
         probID = as.factor(ID)
  )



logistic <- function(x, phi=1) {
  1 / (1 + exp(-phi*x))
}

values <- c("#c80896", "#fb9b06")



# Descriptive Statistics --------------------------------------------------

## Treatments --------------------------------------------------------------

statTreat <- dat %>%
  group_by(aim) %>% # group by decision criterion
  group_by(CP, .add=T) %>% # and/or complexity
  summarize(
    nChoice = n() , 
    medianEffort = median(effort) ,
    meanEffort = round(mean(effort), 2) ,
    sdEffort = round(sd(effort), 2) ,
    meanSwitchRate = round(mean(switchprob), 2) ,
    sdSwitchRate = round(sd(switchprob), 2) ,
    meanAccuracy = round(mean(rightchoice), 2) ,
    sdAccuracy = round(sd(rightchoice), 2)
    )

## Participants ------------------------------------------------------------

statSub <- dat %>%
  select(partID, participant, age, gender, aim, CP, effort, switchprob, rightchoice, ID) %>% 
  #distinct() %>%  
  group_by(partID, participant, age, gender, aim, CP) %>% 
  summarize(
    nChoice = n() , 
    meanEffort = round(mean(effort), 2) , 
    meanSwitching = round(mean(switchprob), 2) ,
    meanAccuracy = round(mean(rightchoice), 2) ,
            ) %>% 
  ungroup()

## age and gender summaries for reporting

# age
statSub %>% 
  summarize(meanAge = round(mean(age,na.rm = T), 3) ,
            sdAge = round(sd(age,na.rm =T ),2))

# gender
statSub %>%
  group_by(gender) %>%
  summarize(count=n()) %>% 
  mutate(round(count/sum(count), 2)) 


## Problems ----------------------------------------------------------------

problem.check <- dat %>% 
  group_by(ID, aim) %>%
  summarize(
    nChoice = n() , 
    meanEffort = round(mean(effort), 2) ,
    sdEffort = round(sd(effort), 2) ,
    meanAccuracy = round(mean(rightchoice), 2) ,
    sdAccuracy = round(sd(rightchoice), 2) ,
    meanSwitchRate = round(mean(switchprob), 2)
    )

# assign complexity
problem.check$CP <- c(rep("LC",80),rep("MC",80),rep("HC",80))

# effort per problem
problem_plot_A <- problem.check %>% 
  mutate(CP = factor(CP,levels=c("LC","MC", "HC"))) %>%
  ggplot(aes(ID,meanEffort,color=CP))+
  geom_jitter(aes(color=CP),size=2,alpha=0.7)+
  labs(title="(a) Sampling Effort per Problem",
       subtitle="Average Sample Size in all Conditions",
       y="Sample Size",
       x="Problem ID",
       color="Complexity",
       caption="")+
  scale_y_continuous(limits=c(0,50),n.breaks=5)+
  scale_x_continuous(limits=c(0,120),n.breaks=3)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="none")+
  scale_color_jama()+ facet_grid(~aim)
problem_plot_A

# switching per problem
problem_plot_B <- problem.check %>% 
  mutate(CP = factor(CP,levels=c("LC","MC", "HC"))) %>%
  ggplot(aes(ID, meanSwitchRate, color=CP)) +
  geom_jitter(aes(color=CP),size=2,alpha=0.7) +
  labs(title="(b) Switching Rate per Problem",
       subtitle="Average Switching Rate in all Conditions",
       y="Switching Rate",
       x="Problem ID",
       color="Complexity",
       caption="")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(0,120),n.breaks=3)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="none")+
  scale_color_jama()+ facet_grid(~aim)
problem_plot_B

# accuracy per problem
problem_plot_C <- problem.check %>% 
  mutate(CP = factor(CP,levels=c("LC","MC", "HC"))) %>%
  ggplot(aes(ID, meanAccuracy, color=CP))+
  geom_jitter(aes(color=CP),size=2,alpha=0.7)+
  labs(title="(c) Choice Accuracy per Problem",
       subtitle="Average Accuracy in all Conditions",
       y="Accuracy",
       x="Problem ID",
       color="Complexity",
       caption="Horizontal line marks accuracy of 0.5 which equals the result of guessing.")+
  scale_y_continuous(limits=c(0,1),n.breaks=5)+
  scale_x_continuous(limits=c(0,120),n.breaks=3)+
  theme_bw()+
  geom_hline(yintercept=0.5,linewidth=0.5,linetype="dashed")+
  theme(text=element_text(size=14, family="serif"),
        legend.position="bottom")+
  scale_color_jama()+ facet_grid(~aim)
problem_plot_C

grid.arrange(problem_plot_A, problem_plot_B, problem_plot_C, ncol=1)

# ggsave("manuscript/figures/lotteries.jpg", height = 10, width = 8)


# Criterion Effects -------------------------------------------------------

dat <- dat %>% 
  mutate(
    partID = as.factor(partID) ,
    probID = as.factor(ID)
    )


## Accuracy ----------------------------------------------------------------


# Descriptive analysis

dAccuracy <-  statSub %>% 
  group_by(aim) %>% 
  summarise(m = mean(meanAccuracy) ,
            sd = sd(meanAccuracy) , 
            n = n(), 
            se = sd(meanAccuracy)/sqrt(n()))
dAccuracy

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
  scale_color_manual(values = values) + 
  scale_fill_manual(values = values) +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none")
ggsave("talks/figures/accuracy.png", height = 5, width = 7)


# Statistical Inference

mAccuracy <- brm(rightchoice ~ aim + (1|partID) + (1|probID) ,
                 data = dat ,
                 family = bernoulli(link="logit") ,
                 iter = 2000 ,
                 warmup = 1000 , 
                 chains = 6 , 
                 cores = 6)

summary(mAccuracy)

# get_variables(mAccuracy)
mAccuracy_post <- mAccuracy %>% 
  spread_draws(b_Intercept,  b_aimFW) %>% 
  rename(m_EV = b_Intercept ,
         off_FW = b_aimFW) %>% 
  mutate(m_FW = m_EV + off_FW, 
         diff = exp(m_EV - m_FW))

mAccuracy_post %>% 
  gather_draws(m_EV, m_FW, diff) %>% 
  mean_qi()

mAccuracy_post %>% 
  ggplot(aes(x=diff)) +
  stat_halfeye(point_interval = "mean_qi", fill = "#c80896", alpha = .5) + 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  labs(x = expression("Odds Ratio:  " * frac("Long", "Short")) , 
       y = "Posterior Density") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 16)
ggsave("talks/figures/accuracy_model.png", height = 5, width = 7)


## Switch Effects -------------------------------------------------------


m2.3 <- brm(rightchoice ~ switchprob*aim + (1|partID) + (1|probID) ,
            data = dat ,
            family = bernoulli(link = "logit") , 
            iter = 2000 ,
            warmup = 1000 ,
            chains = 6 , 
            cores = 6)
summary(m2.3)

# plot posterior distributions

get_variables(m2.3)
posts <-  m2.3 %>% 
  spread_draws(b_Intercept, b_switchprob, b_aimFW, `b_switchprob:aimFW`) %>% 
  rename(m_EV = b_Intercept , 
         off_m_FW = b_aimFW , 
         b_EV = b_switchprob ,
         off_b_FW =  `b_switchprob:aimFW`) %>% 
  mutate(m_FW = m_EV + off_m_FW , 
         b_FW = b_EV + off_b_FW , 
         odds_EV = exp(b_EV),
         odds_FW = exp(b_FW))

posts %>% 
  gather_draws(odds_EV, odds_FW) %>% 
  mean_qi()

posts %>% 
  gather_draws(odds_EV, odds_FW) %>% 
  ggplot(aes(x=.value, y =.variable, fill = .variable)) +
  stat_halfeye(alpha = .5)+ 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  labs(y = "Posterior Density" ,
       x = expression(e^b["Switch"]),
       fill = "") +
  scale_fill_manual(values= values) + 
  scale_y_discrete(labels=c("Long", "Short")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")
ggsave("talks/figures/switch_effect.png", height = 5, width = 7)


# check <- dat %>%
#   add_epred_draws(m2.3, ndraws = 100) %>% 
#   group_by(aim, partID, .draw) %>% 
#   summarise(
#     mSwitch = mean(switchprob) , 
#     mAcc = mean(.epred)) %>% 
#   ungroup() %>% 
#   mutate(draw = as.factor(.draw))
# 
# dat %>%
#   add_epred_draws(m2.3, ndraws = 100) %>% 
#   View()


posts[sample(1:nrow(posts), 50), ]

preds <- expand_grid(SwitchRate = seq(0,1,.01) , 
                     posts[sample(1:nrow(posts), 50), ])

preds <- preds %>% 
  mutate(EV =  logistic(m_EV + b_EV*SwitchRate) , 
         FW =  logistic(m_FW + b_FW*SwitchRate)
         )

preds <- preds %>% 
  pivot_longer(cols = c(EV, FW), names_to = "aim", values_to = "acc")
names(preds)

preds %>% 
  ggplot(aes(x = SwitchRate, y = acc, color = aim)) +
  geom_line(aes(group = interaction(.draw, aim)), linewidth = .2, alpha = .3) +
  geom_point(data=statSub, aes(x=meanSwitching, meanAccuracy), size = 3, alpha = .5) + 
  labs(y = "Mean Choice Accuracy" , 
       x = "Mean Switch Rate",
       color = "Decision Goal") +
  scale_color_manual(values= values, labels = c("Long", "Short")) + 
  #scale_y_discrete(labels=c("Long", "Short")) +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "top")
ggsave("talks/figures/switch_effect.png", height = 5, width = 7)

## Switch Behavior ------------------------------------------------------


# Descriptive statistics

means <- statSub %>% group_by(aim) %>% 
  summarise(m = mean(meanSwitching) ,
            sd = sd(meanSwitching) , 
            n = n(), 
            se = sd(meanSwitching)/sqrt(n()))

statSub %>% 
  ggplot(aes(x=aim, y=meanSwitching, color = aim)) + 
  geom_violin(width = .3, alpha = .3) +
  geom_beeswarm(size=3, alpha = .2) +
  geom_boxplot(width = .05, color = "black", alpha = .3, outliers = F) +
  geom_point(data=means, aes(y=m),  color = "black", size=4, shape = 18) + 
  labs(x = "Decision Goal", 
       y = "Mean Switch Rate") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_x_discrete(labels=c("Long", "Short")) +
  scale_color_manual(values = values) + 
  scale_fill_manual(values = values) +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none")
ggsave("talks/figures/switch_behavior.png", height = 5, width = 7)




# model with random and subject specific intercepts throws error
mSwitchBehave <- brm(switchprob ~ aim, 
                     data = dat , 
                     family = gaussian(link = "identity") , 
                     iter = 2000 ,
                     warmup = 1000 ,
                     chains = 6 , 
                     cores = 6
                     )

summary(mSwitchBehave)


mSwitchBehave2 <- brm(switchprob ~ aim + (1|probID), 
                     data = dat , 
                     family = gaussian(link = "identity") , 
                     iter = 2000 ,
                     warmup = 1000 ,
                     chains = 6 , 
                     cores = 6
)

summary(mSwitchBehave2)

get_variables(mSwitchBehave2)
mSwitchBehave2_post <- mSwitchBehave2 %>%
  spread_draws(b_Intercept,  b_aimFW) %>% 
  rename(m_EV = b_Intercept ,
         off_FW = b_aimFW) %>% 
  mutate(m_FW = m_EV + off_FW, 
         diff = m_FW - m_EV)

mSwitchBehave2_post %>% 
  gather_draws(m_EV, m_FW, diff) %>% 
  mean_qi()

mSwitchBehave2_post %>% 
  ggplot(aes(x=diff)) +
  stat_halfeye(point_interval = "mean_qi", fill = "#fb9b06", alpha = .5) + 
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) + 
  labs(y = "Posterior Density" ,
       x = expression(b[Short] - b[Long])) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 16)
ggsave("talks/figures/switch_behavior_model.png", height = 5, width = 7)



# ## Setup Model 1.1
# load("models/m1.1n.Rdata") # load from directory
# 
# zoib_model <- bf(
#   switchprob ~ 0 + intercept + aim + (1|partID),
# phi ~ 1 + (1|partID),
# zoi ~ 1 + (1|partID),
# coi ~ 1 + (1|partID))
# 
# m1.1n <- brm(formula = zoib_model ,        
#              family = zero_one_inflated_beta(), 
#              data = dat2 ,
#              chains = 4 ,
#              iter = 12000 ,
#              warmup = 3000 ,
#              cores = 4)


# Ecological Analysis -----------------------------------------------------


## Safe and Risky ----------------------------------------------------------

dat <- dat %>% 
  mutate(env = as.factor(if_else(CP=="LC", "safe", "risk")))

statSub <- statSub %>% 
  mutate(env = as.factor(if_else(CP=="LC", "safe", "risk")))
  

### Accuracy -------------------------------------------------

means_group <- statSub %>% group_by(aim, env) %>% 
  summarise(m = mean(meanAccuracy) ,
            sd = sd(meanAccuracy) , 
            n = n(), 
            se = sd(meanAccuracy)/sqrt(n()))

statSub %>% 
  ggplot(aes(x=aim, y=meanAccuracy, group = interaction(env, aim), color = aim, shape = env)) + 
  geom_beeswarm(size=3, alpha = .5, dodge.width = .5) +
  geom_violin(width = .4, alpha = .1, position=position_dodge(0.5)) +
  geom_point(data=means_group, aes(y=m, group = env), color = "black",  size=4, shape = 18, position=position_dodge(0.5)) + 
  geom_hline(yintercept = .5, linewidth = 1, linetype = "dashed", color = "black") +
  labs(y = "Mean Accuracy" , 
       x = "Decision Goal",
       color = "Decision Goal",
       shape = "Environment") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_color_manual(values = values, labels = c("Long", "Short")) + 
  scale_shape_manual(values = c(16, 1)) + 
  theme_minimal(base_size = 16) +
  guides(fill="none", color = "none") + 
  theme(legend.position = "top")
ggsave("talks/figures/accuracy_environment.png", height = 5, width = 7)

mAccuracy_SR <- brm(rightchoice ~ aim*env + (1|partID) + (1|probID) ,
                    data = dat , 
                    family = bernoulli(link="logit") ,
                    iter = 2000 ,
                    warmup = 1000 , 
                    chains = 6 , 
                    cores = 6)

summary(mAccuracy_SR)

get_variables(mAccuracy_SR)
mAccuracy_post <- mAccuracy_SR %>% 
  spread_draws(b_Intercept, b_aimFW, b_envsafe, `b_aimFW:envsafe`) %>% 
  rename(m_EV_risk = b_Intercept ,
         off_FW = b_aimFW ,
         off_safe = b_envsafe , 
         off_FW_safe = `b_aimFW:envsafe`
         ) %>% 
  mutate(diff_EV = exp(off_safe) ,
         m_FW_risk = m_EV_risk + off_FW , 
         m_FW_safe = m_EV_risk + off_FW + off_safe + off_FW_safe ,
         diff_FW = exp(m_FW_safe - m_FW_risk))

mAccuracy_post %>% 
  gather_draws(diff_EV, diff_FW) %>% 
  mean_qi()

mAccuracy_post %>% 
  gather_draws(diff_FW, diff_EV) %>% 
  ggplot(aes(x=.value, y = .variable, fill = .variable)) +
  stat_halfeye(point_interval = "mean_qi",alpha = .5) + 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  labs(x = expression("Odds Ratio:  " * frac("Safe", "Risky")), 
       y = "Posterior Density") + 
  scale_y_discrete(labels = c("Long", "Short")) + 
  scale_fill_manual(values= values) + 
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none")
ggsave("talks/figures/accuracy_environment_model.png", height = 5, width = 7)



## Setup Model 2.1
load("models/m2.1.Rdata") # load from directory

# Model definition
#   m2.1 <- brm(rightchoice ~ 0 + intercept + CP + (1|partID) ,
#            data = dat2 , 
#            family = bernoulli(link = "logit") , 
#            iter = 16000 , 
#            warmup = 4000 ,
#            chains = 4 , 
#            cores = 4)

# Checking priors
get_prior(m2.1)

# Estimates  
summary(m2.1)

### Switch Effects ----------------------------------------

# Subset data
dat_fw <- dat %>% filter(aim == "FW")        # Subsetting the choice data
dat_ev <- dat %>% filter(aim == "EV")        # Subsetting the choice data


## EV 
mSwitchEff_SR_ev <- brm(rightchoice ~ switchprob*env + (1|partID) + (1|probID) ,
                        data = dat_ev ,
                        family = bernoulli(link = "logit") , 
                        iter = 2000 ,
                        warmup = 1000 ,
                        chains = 6 , 
                        cores = 6)

summary(mSwitchEff_SR_ev)



get_variables(mSwitchEff_SR_ev)
posts <-mSwitchEff_SR_ev %>% 
  spread_draws(b_switchprob,  `b_switchprob:envsafe`) %>% 
  rename(b_EV_risky = b_switchprob , 
         off_safe =  `b_switchprob:envsafe`) %>% 
  mutate(b_EV_safe = b_EV_risky + off_safe , 
         odds_EV_risky = exp(b_EV_risky),
         odds_EV_safe = exp(b_EV_safe))

posts %>% 
  gather_draws(odds_EV_risky, odds_EV_safe) %>% 
  mean_qi()

posts %>% 
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
ggsave("talks/figures/switch_effect_environment_EV.png", height = 5, width = 7)


# check <- dat %>%
#   add_epred_draws(m2.3, ndraws = 100) %>% 
#   group_by(aim, partID, .draw) %>% 
#   summarise(
#     mSwitch = mean(switchprob) , 
#     mAcc = mean(.epred)) %>% 
#   ungroup() %>% 
#   mutate(draw = as.factor(.draw))
# 
# check %>% 
#   ggplot(aes(x = mSwitch, y = mAcc, color = aim)) +
#   geom_smooth(aes(group = interaction(draw, aim)), method = "lm", se = FALSE, linewidth = .5) +
#   geom_point(data=statSub, aes(x=meanSwitching, meanAccuracy), size = 3, alpha = .8) + 
#   scale_color_scico_d(palette="managua") +
#   labs(y = "Mean Choice Accuracy" , 
#        x = "Mean Switch Rate",
#        color = "Decision\nGoal"
#   ) + 
#   theme_minimal()
# ggsave("talks/figures/accuracy_switching_2.png", height = 4, width = 6)



## FW 
mSwitchEff_SR_fw <- brm(rightchoice ~ switchprob*env + (1|partID) + (1|probID) ,
                        data = dat_fw ,
                        family = bernoulli(link = "logit") , 
                        iter = 2000 ,
                        warmup = 1000 ,
                        chains = 6 , 
                        cores = 6)

summary(mSwitchEff_SR_fw)

get_variables(mSwitchEff_SR_fw)
posts <-mSwitchEff_SR_fw %>% 
  spread_draws(b_switchprob,  `b_switchprob:envsafe`) %>% 
  rename(b_FW_risky = b_switchprob , 
         off_safe =  `b_switchprob:envsafe`) %>% 
  mutate(b_FW_safe = b_FW_risky + off_safe , 
         odds_FW_risky = exp(b_FW_risky),
         odds_FW_safe = exp(b_FW_safe))

posts %>% 
  gather_draws(odds_FW_risky, odds_FW_safe) %>% 
  mean_qi()

posts %>% 
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
ggsave("talks/figures/switch_effect_environment_FW.png", height = 5, width = 7)




### Switch Behavior  ---------------------------------------


# Descriptive 

means_group <- statSub %>% group_by(aim, env) %>% 
  summarise(m = mean(meanSwitching) ,
            sd = sd(meanSwitching) , 
            n = n(), 
            se = sd(meanSwitching)/sqrt(n()))


statSub %>% 
  ggplot(aes(x=aim, y=meanSwitching, group = interaction(env, aim), color = aim, shape = env)) + 
  geom_beeswarm(size=3, alpha = .5, dodge.width = .5) +
  geom_violin(width = .4, alpha = .1, position=position_dodge(0.5)) +
  geom_point(data=means_group, aes(y=m, group = env), color = "black",  size=4, shape = 18, position=position_dodge(0.5)) + 
  labs(y = "Mean Switch Rate" , 
       x = "Decision Goal",
       color = "Decision Goal",
       shape = "Environment") + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_x_discrete(labels = c("Long", "Short")) + 
  scale_color_manual(values = values, labels = c("Long", "Short")) + 
  scale_shape_manual(values = c(16, 1)) + 
  theme_minimal(base_size = 16) + 
  guides(fill="none", color = "none") + 
  theme(legend.position = "top")
ggsave("talks/figures/switch_behavior_environment.png", height = 5, width = 7)





# Inferential

mSwitchBehave_SR <- brm(switchprob ~ aim*env + (1|probID),
                        data = dat , 
                        family = gaussian(link = "identity") , 
                        iter = 2000 ,
                        warmup = 1000 ,
                        chains = 6 , 
                        cores = 6
)


summary(mSwitchBehave_SR)


get_variables(mSwitchBehave_SR)
mSwitchBehave_SR_post <- mSwitchBehave_SR %>% 
  spread_draws(b_Intercept, b_aimFW, b_envsafe, `b_aimFW:envsafe`) %>% 
  rename(m_EV_risk = b_Intercept ,
         off_FW = b_aimFW ,
         off_safe = b_envsafe , 
         off_FW_safe = `b_aimFW:envsafe`
  ) %>% 
  mutate(diff_EV = exp(off_safe) ,
         m_FW_risk = m_EV_risk + off_FW , 
         m_FW_safe = m_EV_risk + off_FW + off_safe + off_FW_safe ,
         diff_FW = exp(m_FW_safe - m_FW_risk))

mSwitchBehave_SR_post %>% 
  gather_draws(diff_EV, diff_FW) %>% 
  mean_qi()


mSwitchBehave_SR_post %>% 
  gather_draws(diff_FW, diff_EV) %>% 
  ggplot(aes(x=.value, y = .variable, fill = .variable)) +
  stat_halfeye(point_interval = "mean_qi",alpha = .5) + 
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) + 
  labs(x = expression("Odds Ratio:  " * frac("Safe", "Risky")), 
       y = "Posterior Density") + 
  scale_y_discrete(labels = c("Long", "Short")) + 
  scale_fill_manual(values= values) + 
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none")
ggsave("talks/figures/switch_behavior_environment_model.png", height = 5, width = 7)




# Safe, Risky (2), Risky (3) ----------------------------------------------



# Supplementary Analysis ------------------------------------------------------------



## Convergence & Fit -------------------------------------------------------

# # pp check
# ppcheck_2.3 <- pp_check(m2.3, ndraws=100)+
#   labs(title="Posterior Predictive Distribution of Model 2.3",
#        subtitle="",
#        caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
#        color="y Distribution")+
#   theme_bw()+
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.position="bottom",
#         text=element_text(size=14, family="serif"))
# 
# # MCMC trace
# trace_plot2.3 <- mcmc_plot(m2.3,type="trace")+
#   labs(title="Parameter Convergence over MCMC Iteration of Model 2.2",
#        subtitle="")+
#   theme_bw()+
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         legend.position="bottom",
#         text=element_text(size=14, family="serif"))





## Time Effects ------------------------------------------------------------


# Sampling per trial round (time effect on sampling behavior)
plot.sampling.time <- dat2 %>% ggplot(aes(Trialround,effort, color=CP))+ 
  geom_point(alpha=1/10)+ 
  geom_smooth(method = "lm",se=F)+ 
  labs(title="(a) Sampling Effort per Trial Round",
       subtitle="Observations and linear Trend in all Conditions",
       y="Sample Size",
       x="Trial Round",
       color="Complexity",
       caption="
          
          
          ")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="none")+
  scale_color_jama()+facet_grid(~aim)
plot.sampling.time

# Switching per trial round (time effect on sampling behavior)
plot.sw.time <- dat2 %>% ggplot(aes(Trialround,switchprob, color=CP))+ 
  geom_point(alpha=1/10)+ 
  geom_smooth(method = "lm",se=F)+ 
  labs(title="(b) Switching Rate per Trial Round",
       subtitle="Observations and linear Trend in all Conditions",
       y="Switching Rate",
       x="Trial Round",
       color="Complexity",
       caption="
          
          ")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="none")+
  scale_color_jama()+facet_grid(~aim)
plot.sw.time

# Accuracy per trial round (time effect on sampling behavior)
plot.acc.time <- dat2 %>% mutate(CP = factor(CP,levels=c("LC","MC", "HC"))) %>%
  ggplot(aes(Trialround,rightchoice, color=CP))+ 
  geom_point(alpha=1/10)+ 
  geom_smooth(method = "lm",se=F)+ 
  labs(title="(c) Choice Accuracy per Trial Round",
       subtitle="Observations and linear Trend in all Conditions",
       y="Accuracy",
       x="Trial Round",
       color="Complexity",
       caption="The variable Trial Round captures the sequence in which a participant solves the choice problems.")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="bottom")+
  scale_color_jama()+facet_grid(~aim)
plot.acc.time

# save graph  
ggsave("./graphs/pre/time.jpg",grid.arrange(plot.sampling.time, plot.sw.time, plot.acc.time, ncol=1),height = 10, width = 8)


