# load packages 
pacman::p_load(tidyverse ,
               ggsci , 
               gridExtra , 
               ggpubr , 
               brms , 
               bayesplot , 
               bayestestR , 
               lm.beta , 
               car , 
               data.table
               )
               

# load data
dat <- read_rds("data/choices.rds")

## color coding

aimcol <- c("#00C698","#AA4499")
names(aimcol) <- c("EV","FW")


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

## Accuracy ----------------------------------------------------------------


## Switching Effects -------------------------------------------------------

## Setup Model 2.2
load("models/m2.2.Rdata") # load from directory

# Model definition
#   m2.2 <- brm(rightchoice ~ 0 + intercept + switchprob*aim + (1|partID) ,
#              data = dat2 , 
#              family = bernoulli(link = "logit") , 
#              iter = 10000 , 
#              warmup = 2500 ,
#              chains = 4 , 
#              cores = 4)

# Checking priors
get_prior(m2.2)

# Estimates  
summary(m2.2)

# Introduction of prediction column
dat2$accuracy2.2 <- predict(m2.2,newdata=dat2)

# Posterior coefficients (odds ratios)
posterior2.2    <- as.matrix(m2.2)
OR.FW    <- exp(posterior2.2[,"b_switchprob"]+posterior2.2[,"b_switchprob:aimFW"])
OR.EV    <- exp(posterior2.2[,"b_switchprob"])

m2.2.estimates  <- cbind(OR.EV,OR.FW)

# OR of switching in FW
round(exp(fixef(m2.2)[2]+fixef(m2.2)[4]),2) # estimate
ci((OR.FW),0.95)                            # 95% CI

# OR of switching in EV
round(exp(fixef(m2.2)[2]),2)  # estimate
ci((OR.EV),0.95)              # 95% CI

# pp check
pp_plot2.2 <- pp_check(m2.2, ndraws=50)+
  labs(title="Posterior Predictive Distribution of Model 2.2",
       subtitle="",
       caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
       color="y Distribution")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic()

# MCMC trace
trace_plot2.2 <- mcmc_plot(m2.2,type="trace")+
  labs(title="Parameter Convergence over MCMC Iteration of Model 2.2",
       subtitle="")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic(alpha=0.5)

# save plot
ggsave("./graphs/pre/2_2.jpg",grid.arrange(trace_plot2.2, pp_plot2.2, ncol=2),height = 5, width = 16)

# Trend
pred_plot2.2 <-  dat2 %>% 
  ggplot(aes(x=switchprob, y=accuracy2.2[,"Estimate"], color = aim))+ 
  geom_point(alpha=1/10,size=1)+ 
  geom_smooth(method = "lm",linewidth=0.7,se=F)+ 
  scale_y_continuous(limits = c(0.25,1),n.breaks=4)+
  labs(title="Predicted Choice Accuracy per Switching Rate and Criterion",
       subtitle="Model Predictions and linear Trend",
       y="Choice Accuracy",
       x="Switching Rate",
       color="Criterion")+
  theme_bw()+
  theme(legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_manual(values=c(aimcol))
pred_plot2.2

# save plot
ggsave("./graphs/Hypos/2_2_pred_plot.png",plot=pred_plot2.2,height = 5, width = 8)

# Posterior coefficients
color_scheme_set("red")
coeff_plot2.2.OR <- bayesplot::mcmc_areas((m2.2.estimates), 
                                          pars = c("OR.FW",
                                                   "OR.EV"),
                                          prob = 0.95,
                                          point_est="mean")+
  labs(title="Effects of Switching on Choice Accuracy per Criterion",
       subtitle="Posterior Distribution of Effects with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Odds Ratio)",
       caption="Dashed line at x = 1 marks the point of no effect of switching on accuracy.
                       Values below 1 indicate a negative effect, values above 1 a positive effect.")+
  scale_x_continuous(limits = c(0,3),n.breaks=12)+
  geom_vline(xintercept=1,size=1,linetype="dashed")+
  theme_bw()+
  theme(legend.position="bottom",
        text=element_text(size=14, family="serif"))
coeff_plot2.2.OR

# save plot
ggsave("./graphs/Hypos/2_2_OR_plot.png",plot=coeff_plot2.2.OR,height = 3, width = 8)



## Switching Behavior ------------------------------------------------------


## Setup Model 1.1
load("models/m1.1n.Rdata") # load from directory

# zoib_model <- bf(
#  switchprob ~ 0 + intercept + aim + (1|partID),
#  phi ~ 1 + (1|partID),
#  zoi ~ 1 + (1|partID),
#  coi ~ 1 + (1|partID))

#  m1.1n <- brm(formula = zoib_model ,
#           family = zero_one_inflated_beta() ,
#           data = dat2 ,
#           chains = 4 ,
#           iter = 12000 ,
#           warmup = 3000 ,
#           cores = 4)

# Checking priors
get_prior(m1.1n)

# Estimates  
summary(m1.1n)

# Introduction of prediction column
pred1.1 <- predict(m1.1n,newdata=dat2)
dat2$pred1.1 <- (pred1.1[,"Estimate"])

# Posterior coefficients (probabilities)
posterior1.1      <-    as.matrix(m1.1n)
switchrate.EV     <-    logistic(posterior1.1[,"b_intercept"])
switchrate.FW     <-    logistic(posterior1.1[,"b_intercept"]+posterior1.1[,"b_aimFW"])

# Posterior coefficients (odds ratio)
switch.OR.FW    <-    (switchrate.FW/(1-switchrate.FW))/(switchrate.EV/(1-switchrate.EV))
m1.1.estimates  <-    cbind(switchrate.EV,switchrate.FW,switch.OR.FW)

# Nominal differences (not reported)
switchdiff  <-  (switchrate.FW)-(switchrate.EV)
m1.1.diff   <-  cbind(switchdiff)

# p for switching in EV
round(logistic(fixef(m1.1n)[4]),2)  # estimate
ci(switchrate.EV,0.95)              # 95% CI

# p for switching in FW
round(logistic(fixef(m1.1n)[4]+fixef(m1.1n)[5]),2)  # estimate
ci(switchrate.FW,0.95)                              # 95% CI

# OR for switching in FW vs EV
round(mean(switch.OR.FW),2)   # estimate
ci(switch.OR.FW,0.95)         # 95% CI

# pp check
pp_plot1.1 <- pp_check(m1.1n, ndraws=50)+
  labs(title="Posterior Predictive Distribution of Model 1.1",
       subtitle="",
       caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
       color="y Distribution")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic()

# MCMC trace
trace_plot1.1 <- mcmc_plot(m1.1n,type="trace")+
  labs(title="Parameter Convergence over MCMC Iteration of Model 1.1",
       subtitle="")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic(alpha=0.5)

# save plot
ggsave("./graphs/pre/1_1.jpg",grid.arrange(trace_plot1.1, pp_plot1.1, ncol=2),height = 5, width = 16)

# Trend
pred_plot1.1 <- ggline(dat2, x = "aim", y = "pred1.1", color = "aim",add = "mean_sd",size=0.7)+
  geom_jitter(aes(color=aim),alpha=1/10,size=1)+
  labs(title="Predicted Switching Rate per Criterion",
       subtitle="Density of Model Predictions with Mean and Standard Deviation",
       y="Switching Rate",
       x="Criterion",
       color="Criterion")+
  scale_y_continuous(limits=c(-0.1,1))+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="bottom")+
  scale_color_manual(values=c(aimcol))
pred_plot1.1

# save plot
ggsave("./graphs/Hypos/1_1_pred_plot.png",plot=pred_plot1.1,height = 5, width = 8)

# Posterior effects
color_scheme_set("red")
coeff_plot1.1 <- bayesplot::mcmc_areas((m1.1.estimates), 
                                       pars = c("switchrate.EV",
                                                "switchrate.FW"),
                                       prob = 0.95,
                                       point_est="mean")+
  labs(title="Switching Rate per Criterion",
       subtitle="Posterior Distribution of Model Parameters with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Probability)")+
  scale_x_continuous(limits = c(0,0.3),n.breaks=6)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_plot1.1

# save plot
ggsave("./graphs/Hypos/1_1_coeff_plot.png",plot=coeff_plot1.1,height = 3, width = 8)

color_scheme_set("red")
coeff_plot1.1.OR <- bayesplot::mcmc_areas(m1.1.estimates, 
                                          pars = c("switch.OR.FW"),
                                          prob = 0.95,
                                          point_est="mean")+
  labs(title="Effect of Search Criterion (FW) on Switching Rate",
       subtitle="Posterior Distribution of Effects with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Odds Ratio)",
       caption="Vertical line at x = 1 marks the point of no effect of criterion on switching.
                       Values below 1 indicate a negative effect, values above 1 a positive effect.")+
  scale_x_continuous(limits = c(0.2,2.2),n.breaks=12)+
  geom_vline(xintercept=1,size=1,linetype="dashed")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_plot1.1.OR

# save plot
ggsave("./graphs/Hypos/1_1_OR_plot.png",plot=coeff_plot1.1.OR,height = 2.5, width = 8)





# Ecological Analysis -----------------------------------------------------

## Complexity and Accuracy -------------------------------------------------

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

# Introduction of prediction column
dat2$accuracy2.1  <- predict(m2.1, newdata=dat2)
dat2$acc2.1       <- dat2$accuracy2.1[,"Estimate"]

# Posterior coefficients (probabilities)
posterior2.1  <-  as.matrix(m2.1)
Acc.LC        <-  logistic(posterior2.1[,"b_intercept"])
Acc.MC        <-  logistic(posterior2.1[,"b_intercept"]+posterior2.1[,"b_CPMC"])
Acc.HC        <-  logistic(posterior2.1[,"b_intercept"]+posterior2.1[,"b_CPHC"])

# Posterior coefficients (odds ratios)
Acc.OR.MC     <-  exp(posterior2.1[,"b_intercept"]+posterior2.1[,"b_CPMC"])/
  exp(posterior2.1[,"b_intercept"]) 
Acc.OR.HC     <-  exp(posterior2.1[,"b_intercept"]+posterior2.1[,"b_CPHC"])/
  exp(posterior2.1[,"b_intercept"])

m2.1.estimates  <-  cbind(Acc.LC, Acc.MC, Acc.HC, Acc.OR.MC, Acc.OR.HC)

# p rightchoice in LC
round(logistic(fixef(m2.1)[1]),2) # estimate
ci((Acc.LC),0.95)                 # 95% CI

# p rightchoice in MC
round(logistic(fixef(m2.1)[1]+fixef(m2.1)[2]),2)  # estimate
ci((Acc.MC),0.95)                                 # 95% CI

# p rightchoice in HC
round(logistic(fixef(m2.1)[1]+fixef(m2.1)[3]),2)  # estimate
ci((Acc.HC),0.95)                                 # 95% CI

# OR MC vs LC
round((exp(fixef(m2.1)[1]+fixef(m2.1)[2]))/(exp(fixef(m2.1)[1])),2) # estimate
ci((Acc.OR.MC),0.95)                                                # 95% CI

# OR HC vs LC
round((exp(fixef(m2.1)[1]+fixef(m2.1)[3]))/(exp(fixef(m2.1)[1])),2) # estimate
ci((Acc.OR.HC),0.95)                                                # 95% CI

# pp check
pp_plot2.1 <- pp_check(m2.1, ndraws=50)+
  labs(title="Posterior Predictive Distribution of Model 2.1",
       subtitle="",
       caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
       color="y Distribution")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic()

# MCMC trace
trace_plot2.1 <- mcmc_plot(m2.1,type="trace")+
  labs(title="Parameter Convergence over MCMC Iteration of Model 2.1",
       subtitle="")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic(alpha=0.5)

# save plot
ggsave("./graphs/pre/2_1.jpg",grid.arrange(trace_plot2.1, pp_plot2.1, ncol=2),height = 5, width = 16)

# Trend
pred_plot2.1 <- ggline(dat2, x = "CP", y = "acc2.1", color = "aim",add = "mean_sd",size=0.7)+
  geom_jitter(aes(color=aim),alpha=1/10,size=1)+
  labs(title="Predicted Choice Accuracy per Complexity and Criterion",
       subtitle="Density of Model Predictions with Mean and Standard Deviation",
       y="Choice Accuracy",
       x="Complexity",
       color="Criterion")+
  scale_y_continuous(limits = c(0.25,1),n.breaks=4)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="bottom")+
  scale_color_manual(values=c(aimcol))
pred_plot2.1

# save plot
ggsave("./graphs/Hypos/2_1_pred_plot.png",plot=pred_plot2.1,height = 5, width = 8)

# Posterior effects
color_scheme_set("red")
coeff_plot2.1 <- bayesplot::mcmc_areas((m2.1.estimates), 
                                       pars = c("Acc.LC",
                                                "Acc.MC",
                                                "Acc.HC"),
                                       prob = 0.95,
                                       point_est="mean")+
  labs(title="Choice Accuracy per Complexity",
       subtitle="Posterior Distribution of Parameters with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Probability)")+
  scale_x_continuous(limits = c(0.5,0.9),n.breaks=9)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_plot2.1

# save plot
ggsave("./graphs/Hypos/2_1_coeff_plot.png",plot=coeff_plot2.1,height = 3.5, width = 8)

coeff_plot2.1.OR <- bayesplot::mcmc_areas((m2.1.estimates), 
                                          pars = c("Acc.OR.MC",
                                                   "Acc.OR.HC"),
                                          prob = 0.95,
                                          point_est="mean")+
  labs(title="Effects of Complexity on Choice Accuracy (vs. Low Complexity)",
       subtitle="Posterior Distribution of Effects with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Odds Ratio)",
       caption="Vertical line at x = 1 marks the point of no effect of complexity on accuracy.
                       Values below 1 indicate a negative effect, values above 1 a positive effect.")+
  scale_x_continuous(limits = c(0.2,1.8),n.breaks=16)+
  geom_vline(xintercept=1,size=1,linetype="dashed")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_plot2.1.OR

# save plot
ggsave("./graphs/Hypos/2_1_OR_plot.png",plot=coeff_plot2.1.OR,height = 3, width = 8)





## Complexity and Switching Effects ----------------------------------------

## Setup Models 2.3a & 2.3b

# Subset data
dat_fw <- dat2 %>% filter(aim == "FW")        # Subsetting the choice data
dat_ev <- dat2 %>% filter(aim == "EV")        # Subsetting the choice data

# Load models from directory
load("models/m2.3a.Rdata")
load("models/m2.3b.Rdata")

# Define models
#   m2.3a <- brm(rightchoice ~ 0 + intercept + switchprob*CP + (1|partID) ,
#            data = dat_fw , 
#            family = bernoulli(link = "logit") , 
#            iter = 12000 ,
#            warmup = 3000 ,
#            chains = 4 , 
#            cores = 4)

#   m2.3b <- brm(rightchoice ~ 0 + intercept + switchprob*CP + (1|partID) ,
#              data = dat_ev , 
#              family = bernoulli(link = "logit") , 
#              iter = 12000 , 
#              warmup = 3000 ,
#              chains = 4 , 
#              cores = 4)

# Checking priors
get_prior(m2.3a)
get_prior(m2.3b)

# Estimates
summary(m2.3a)
summary(m2.3b)

# Introduction of prediction column
dat_fw$accuracy2.3 <- predict(m2.3a,newdata=dat_fw)
dat_ev$accuracy2.3 <- predict(m2.3b,newdata=dat_ev)

# Reverse the subsetting
dat2 <- rbind(dat_fw,dat_ev)

# Posterior coefficients (odds ratios)
posterior2.3a <- as.matrix(m2.3a)
OR.FW_LC  <- exp(posterior2.3a[,"b_switchprob"])
OR.FW_MC  <- exp(posterior2.3a[,"b_switchprob"]+posterior2.3a[,"b_switchprob:CPMC"])
OR.FW_HC  <- exp(posterior2.3a[,"b_switchprob"]+posterior2.3a[,"b_switchprob:CPHC"])

posterior2.3b <- as.matrix(m2.3b)
OR.EV_LC  <- exp(posterior2.3b[,"b_switchprob"])
OR.EV_MC  <- exp(posterior2.3b[,"b_switchprob"]+posterior2.3b[,"b_switchprob:CPMC"])
OR.EV_HC  <- exp(posterior2.3b[,"b_switchprob"]+posterior2.3b[,"b_switchprob:CPHC"])

m2.3.estimates  <- cbind(OR.FW_LC,OR.FW_MC,OR.FW_HC,
                         OR.EV_LC,OR.EV_MC,OR.EV_HC)

# Effects of switching on accuracy in FW 
round(exp(fixef(m2.3a)[2]),2)                   # Odds ratio LC
ci(OR.FW_LC)                                    # 95% CI
round(exp(fixef(m2.3a)[2]+fixef(m2.3a)[5]),2)   # Odds ratio MC
ci(OR.FW_MC)                                    # 95% CI
round(exp(fixef(m2.3a)[2]+fixef(m2.3a)[6]),2)   # Odds ratio HC
ci(OR.FW_HC)                                    # 95% CI

# Effects of switching on accuracy in EV 
round(exp(fixef(m2.3b)[2]),2)                   # Odds ratio LC
ci(OR.EV_LC)                                    # 95% CI
round(exp(fixef(m2.3b)[2]+fixef(m2.3b)[5]),2)   # Odds ratio MC
ci(OR.EV_MC)                                    # 95% CI
round(exp(fixef(m2.3b)[2]+fixef(m2.3b)[6]),2)   # Odds ratio HC
ci(OR.EV_HC)                                    # 95% CI

# pp check a
pp_plot2.3a <- pp_check(m2.3a, ndraws=50)+
  labs(title="Posterior Predictive Distribution of Model 2.3a",
       subtitle="",
       caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
       color="y Distribution")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic()

# MCMC trace a
trace_plot2.3a <- mcmc_plot(m2.3a,type="trace")+
  labs(title="Parameter Convergence over MCMC Iteration of Model 2.3a",
       subtitle="")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic(alpha=0.5)

# pp check b
pp_plot2.3b <- pp_check(m2.3b, ndraws=50)+
  labs(title="Posterior Predictive Distribution of Model 2.3b",
       subtitle="",
       caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
       color="y Distribution")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic()

# MCMC trace b
trace_plot2.3b <- mcmc_plot(m2.3b,type="trace")+
  labs(title="Parameter Convergence over MCMC Iteration of Model 2.3b",
       subtitle="")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic(alpha=0.5)

# save plot
ggsave("./graphs/pre/2_3.jpg",grid.arrange(trace_plot2.3a, pp_plot2.3a, trace_plot2.3b, pp_plot2.3b, ncol=2),height = 10, width = 16)

# Trend
pred_plot2.3 <- dat2 %>% 
  ggplot(aes(x=switchprob, y=accuracy2.3[,"Estimate"], color=aim, linetype=factor(CP, levels = c("LC", "MC", "HC"))))+ 
  geom_point(alpha=1/10,size=1)+ 
  geom_smooth(method = "lm", se = FALSE, color = "black",size=0.7)+ 
  facet_wrap(~aim)+ 
  scale_y_continuous(limits = c(0.25,1),n.breaks=4) +
  labs(title="Predicted Choice Accuracy per Switching Rate and Complexity",
       subtitle="Model Predictions and linear Trend",
       y="Choice Accuracy",
       x="Switching Rate",
       linetype="Complexity",
       color="Criterion")+
  theme_bw()+
  scale_color_manual(values=c(aimcol))+
  theme(legend.position="bottom",
        text=element_text(size=14, family="serif"))
pred_plot2.3

# save plot
ggsave("./graphs/Hypos/2_3_pred_plot.png",plot=pred_plot2.3,height = 5, width = 8)

# effects
color_scheme_set("red")
coeff_FW_plot2.3 <- bayesplot::mcmc_areas((m2.3.estimates), 
                                          pars = c("OR.FW_LC", 
                                                   "OR.FW_MC",
                                                   "OR.FW_HC"),
                                          prob = 0.95,point_est = "mean")+
  labs(title="(a) Effects of Switching on Choice Accuracy in the FW Criterion",
       subtitle="Posterior Distribution of Effects with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Odds Ratio)",
       caption="
                       ")+
  scale_x_continuous(limits = c(0,5),n.breaks=10)+
  geom_vline(xintercept=1,size=1,linetype="dashed")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_FW_plot2.3

color_scheme_set("red")
coeff_EV_plot2.3 <- bayesplot::mcmc_areas((m2.3.estimates), 
                                          pars = c("OR.EV_LC",
                                                   "OR.EV_MC",
                                                   "OR.EV_HC"),
                                          prob = 0.95,point_est = "mean")+
  labs(title="(b) Effects of Switching on Choice Accuracy in the EV Criterion",
       subtitle="Posterior Distribution of Effects with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Odds Ratio)",
       caption="Vertical line at x = 1 marks the point of no effect of switching on accuracy.
                       Values below 1 indicate a negative effect, values above 1 a positive effect.")+
  scale_x_continuous(limits = c(0,5),n.breaks=10)+
  geom_vline(xintercept=1,size=1,linetype="dashed")+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_EV_plot2.3

# save plot 
ggsave("./graphs/Hypos/2_3_OR_plot.jpg",grid.arrange(coeff_FW_plot2.3, coeff_EV_plot2.3, ncol=1),height = 8, width = 8)




## Complexity and Switching Behavior  ---------------------------------------

## Setup Model 1.2
load("models/m1.2n.Rdata") # load from directory

#  zoib_model_2 <- bf(
#    switchprob ~ 0 + intercept + aim * CP + (1|partID),
#    phi ~ 1 + (1|partID),
#    zoi ~ 1 + (1|partID),
#    coi ~ 1 + (1|partID))

#  m1.2n <- brm(formula = zoib_model_2 ,
#           family = zero_one_inflated_beta() ,
#           data = dat2 ,
#           chains = 4 ,
#           iter = 10000 ,
#           warmup = 2500 ,
#           cores = 4)

# Checking priors
get_prior(m1.2n)

# Estimates
summary(m1.2n)

# Introduction of prediction column
pred1.2  <- predict(m1.2n, newdata=dat2)
dat2$pred1.2  <- pred1.2[,"Estimate"]

# Posterior coefficients (probabilities)
posterior1.2    <-  as.matrix(m1.2n)
Switchp.EV.LC   <-  logistic(posterior1.2[,"b_intercept"])
Switchp.EV.MC   <-  logistic(posterior1.2[,"b_intercept"]+posterior1.2[,"b_CPMC"])
Switchp.EV.HC   <-  logistic(posterior1.2[,"b_intercept"]+posterior1.2[,"b_CPHC"])
Switchp.FW.LC   <-  logistic(posterior1.2[,"b_intercept"]+posterior1.2[,"b_aimFW"])
Switchp.FW.MC   <-  logistic(posterior1.2[,"b_intercept"]+posterior1.2[,"b_aimFW"]+posterior1.2[,"b_CPMC"]+posterior1.2[,"b_aimFW:CPMC"])
Switchp.FW.HC   <-  logistic(posterior1.2[,"b_intercept"]+posterior1.2[,"b_aimFW"]+posterior1.2[,"b_CPHC"]+posterior1.2[,"b_aimFW:CPHC"])

# Posterior coefficients (odds ratios)
OR.LC  <- (Switchp.FW.LC/(1-Switchp.FW.LC))/(Switchp.EV.LC/(1-Switchp.EV.LC))
OR.MC  <- (Switchp.FW.MC/(1-Switchp.FW.MC))/(Switchp.EV.MC/(1-Switchp.EV.MC))
OR.HC  <- (Switchp.FW.HC/(1-Switchp.FW.HC))/(Switchp.EV.HC/(1-Switchp.EV.HC))

m1.2.estimates  <-  cbind(Switchp.EV.LC, Switchp.EV.MC, Switchp.EV.HC,
                          Switchp.FW.LC, Switchp.FW.MC, Switchp.FW.HC,
                          OR.LC, OR.MC, OR.HC)

# Probability differences (not reported)
switchdiff.LC  <- (Switchp.FW.LC)-(Switchp.EV.LC)
switchdiff.MC  <- (Switchp.FW.MC)-(Switchp.EV.MC)
switchdiff.HC  <- (Switchp.FW.HC)-(Switchp.EV.HC)

m1.2.diff      <- cbind(switchdiff.LC, switchdiff.MC, switchdiff.HC)

# OR 
round(mean(OR.LC),2)
ci(OR.LC,0.95)

round(mean(OR.MC),2)
ci(OR.MC,0.95)

round(mean(OR.HC),2)
ci(OR.HC,0.95)

# pp check
pp_plot1.2 <- pp_check(m1.2n, ndraws=50)+
  labs(title="Posterior Predictive Distribution of Model 1.2",
       subtitle="",
       caption="y based on model coefficients | yrep based on 50 random draws from the posterior distribution",
       color="y Distribution")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic()

# MCMC trace
trace_plot1.2 <- mcmc_plot(m1.2n,type="trace")+
  labs(title="Parameter Convergence over MCMC Iteration of Model 1.2",
       subtitle="")+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom",
        text=element_text(size=14, family="serif"))+
  scale_color_cosmic(alpha=0.5)

# save graph
ggsave("./graphs/pre/1_2.jpg",grid.arrange(trace_plot1.2, pp_plot1.2, ncol=2),height = 5, width = 16)

# Trend 
pred_plot1.2 <- ggline(dat2, x = "CP", y = "pred1.2", color = "aim",add = "mean_sd",size=0.7)+
  geom_jitter(aes(color=aim),alpha=1/10,size=1)+
  labs(title="Predicted Switching Rate per Criterion and Complexity",
       subtitle="Density of Model Predictions with Mean and Standard Deviation",
       y="Switching Rate",
       x="Complexity",
       color="Criterion")+
  scale_y_continuous(limits=c(-0.1,1))+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"),
        legend.position="bottom")+
  scale_color_manual(values=c(aimcol))
pred_plot1.2

# save plot
ggsave("./graphs/Hypos/1_2_pred_plot.png",plot=pred_plot1.2,height = 5, width = 8)

# Posterior probabilities (not reported)
color_scheme_set("red")
coeff_plot1.2 <- bayesplot::mcmc_areas((m1.2.estimates), 
                                       pars = c("Switchp.FW.LC",
                                                "Switchp.EV.LC",
                                                "Switchp.FW.MC",
                                                "Switchp.EV.MC",
                                                "Switchp.FW.HC",
                                                "Switchp.EV.HC"),
                                       prob = 0.95,
                                       point_est="mean")+
  labs(title="Switching Rate per Criterion and Complexity",
       subtitle="Posterior Distribution of Model Parameters with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Probability)")+
  scale_x_continuous(limits = c(0,1),n.breaks=10)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_plot1.2

# save plot
ggsave("./graphs/Hypos/1_2_coeff_plot.png",plot=coeff_plot1.2,height = 6, width = 8)

# Posterior effects (OR)
color_scheme_set("red")
coeff_plot1.2.OR <- bayesplot::mcmc_areas((m1.2.estimates), 
                                          pars = c("OR.LC",
                                                   "OR.MC",
                                                   "OR.HC"),
                                          prob = 0.95,
                                          point_est="mean")+
  labs(title="Effects of Search Criterion (FW) on Switching Rate per Complexity",
       subtitle="Posterior Distribution of Effects with Mean and 95% Credible Interval",
       x="Possible Parameter Values (Odds Ratio)",
       caption="Vertical line at x = 1 marks the point of no effect of criterion on switching.
                       Values below 1 indicate a negative effect, values above 1 a positive effect.")+
  geom_vline(xintercept=1,linewidth=1,linetype="dashed")+
  scale_x_continuous(limits = c(0,5),n.breaks=12)+
  theme_bw()+
  theme(text=element_text(size=14, family="serif"))
coeff_plot1.2.OR

# save graph
ggsave("./graphs/Hypos/1_2_OR_plot.png",plot=coeff_plot1.2.OR,height = 3.5, width = 8)





# Supplementary Analysis ------------------------------------------------------------


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


