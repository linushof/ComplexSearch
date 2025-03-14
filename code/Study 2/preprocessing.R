# load packages 

library(ggplot2)
library(tidyverse)
library(lm.beta)
library(car)
library(plyr)
library(data.table)
library(brms)
library(bayesplot)
library(ggpubr)
library(bayestestR)
library(ggsci)
library(gridExtra)


#aggregate all PsychoPy exports from "data" folder into one data frame

dat.input <- tibble(list.files(path = 'data/raw', 
                               pattern = '*.csv', 
                               full.names = T) %>% 
                      map(read_csv) %>% 
                      rbindlist(fill = T))

# format gender column

dat.input$gender <- as.factor(revalue(dat.input$gender, c("FALSE" = "f")))

dat.input$gender <- ifelse(dat.input$gender=="female","f",
                           ifelse(dat.input=="f\n", NA ,
                                  ifelse(dat.input=="Female", "f",
                                         ifelse(dat.input=="FEMALE","f",
                                                ifelse(dat.input$gender=="fermale","f",
                                                       ifelse(dat.input$gender=="M","m",
                                                              ifelse(dat.input$gender=="m\n",NA,
                                                                     ifelse(dat.input$gender=="male","m",
                                                                            ifelse(dat.input$gender=="Male","m",
                                                                                   ifelse(dat.input$gender=="f","f",
                                                                                          ifelse(dat.input$gender=="m","m", NA)
                                                                                          )
                                                                                   )
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )


## select relevant columns to form data frames for usage

# dat.sampling for record of sampling process, this contains the full trial data
dat.sampling <- dat.input %>%                                                                    
  select(participant ,
         aim , # decision goal
         CP , # complexity
         ID , # problem identifier
         Trialround , # choice trial (sequence in which ID/problems appeared to the participant)
         O1 , # sampled outcome option 1 / left option
         O2 , # sampled outcome option 2 / right option
         selected , # chosen option 
         attractive , # option with better EV
         freqwinner # option with better FW
         ) %>% 
  filter(!is.na(CP))
dat.sampling <- as_tibble(dat.sampling)
## archiving data for later assessment
write_rds(dat.sampling,"data/samples.rds")


# dat2 for hypothesis testing, this contains all choices (one choice is one line item)

dat2 <- dat.input %>%                                                                         
  select(participant , 
         age , 
         gender ,
         aim , 
         CP , 
         ID ,
         Trialround ,
         effort , # number of sampled outcomes in trial
         switching , # number of switches in trial
         selected ,
         attractive ,
         freqwinner
         ) %>% 
  filter(!is.na(selected)) %>% 
  distinct()

## define factor levels / data types

# complexity as ordinal factor
dat2$CP <- factor(dat2$CP, levels=c("LC","MC","HC"))    

# search aim (decision criterion) as unordered factor
dat2$aim <- factor(dat2$aim, levels=c("EV","FW"))  

# participant gender as unordered factor
dat2$gender <- factor(dat2$gender, levels=c("m","f","d"))

# age as double
dat2$age <- as.double(dat2$age) 

## filter out participants who took notes
dat2 <- dat2 %>% dplyr::filter(participant != "618_4_1", participant!="618_6_20") # filtered set


## prepare additional columns for analysis

# switching probability
dat2$switchprob <- (dat2$switching/(dat2$effort-1))                                           

# choice matching criterion (binomial)
dat2$rightchoice <- ifelse(dat2$aim=='EV',    
                           ifelse(dat2$selected==dat2$attractive,1,0),
                           ifelse(dat2$aim=='FW',
                                  ifelse(dat2$selected==dat2$freqwinner,1,0),
                                  NA))

# clean participant numeration from 1 onwards
id <- c(1:(length(dat2$participant)/40))
partID <- rep(id,each=40)
dat2$partID <- partID
dat2 <- as_tibble(dat2)

## archiving data for later assessment

# write.csv2(dat2,"data/choices.csv",col.names=T,row.names=F)
write_rds(dat2,"data/choices.rds")
