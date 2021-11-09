#----------------------------------------------------------------------------------------------
# PROJECT: SIBECOL GENDER ANALYSIS / BLOCK 1
# DESCRIPTION: Code to analyze the gender biases in the SIBECOL participants. 
# AUTHORS: Anna Lupon*, Pablo Rodríguez-Lozano, Mireia Bartrons, Alba Anadon-Rosell, 
# Meritxell Batalla, Susana Bernal, Andrea G. Bravo, Pol Capdevila, Miguel Cañedo-Argüelles, 
# Núria Catalán, Ana Genua-Olmedo, Cayetano Gutiérrez-Cánovas, Maria João Feio, 
# Federica Lucati, Gabriela Onandia, Sílvia Poblador, Roser Rotchés-Ribalta, Anna 
# Sala-Bubaré, María Mar Sánchez-Montoya, Marta Sebastián, Aitziber Zufiaurre, Ada Pastor 
# *Corresponding author: alupon@ceab.csic.es
#----------------------------------------------------------------------------------------------

#Clean memory

rm(list = ls())

# Load packages 

library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)
library(MuMIn)
library(ggpubr)
library(multcomp)
library(plm)
library(reshape)
library(car)
library(cowplot)
library(hier.part)

# Set working directories
path = gsub("/Code", "", dirname(rstudioapi::getActiveDocumentContext()$path))

DataPath = paste0(path,"/Data")
ResultPath = paste0(path, "/Results") 

#-------------------------------------------------------------------------------
#  SECTION 1: CONTRIBUTIONS AND PRESENTERS #################################### 
#-------------------------------------------------------------------------------

# Q: NUMBER OF CONTRIBUTIONS BY GENDER AND STAGE CAREER ####
# stats: Descriptors + chi-squared test of % women btw career stages + t-test for each career stage btw %W and %M

# Data Preparation

setwd(DataPath)
data_all = read.csv("Data_Block1.csv")

data = data_all %>% dplyr::select(talk.id, talk.type, presenter.stage, presenter.sex)
data$presenter.stage = ordered(data$presenter.stage, c("pre-doctoral", "post-doctoral", "senior non-permanent","senior permanent", "other"))

## All contributions ----------------------------------------------

data %>% group_by(presenter.sex) %>%  
          summarise(n = n()) %>% mutate(freq = n / sum(n)) # 285 (50%) by women

data %>% group_by(presenter.stage, 
                  presenter.sex) %>%
          summarise(n = n()) %>% mutate(freq = n / sum(n)) # % by career stage

data %>% group_by(talk.type, 
                  presenter.sex) %>%
  summarise(n = n()) %>% mutate(freq = n / sum(n)) # % by talk type


#  SECTION 2: COAUTHORS ###################################################### 
## Data file = Data_Block1.csv
## Figures = figure 2a.
## Stats: Descriptors + t-test (we treated sessions as replicates + only for invited, oral and poster contribution)

# Data Preparation ####

setwd(DataPath)
data_all = read.csv("Data_Block1.csv")

data = data_all %>% dplyr::select (talk.id, talk.type, session, presenter.sex, last.author.sex)

# Q: WHAT WAS THE GENDER DISTRIBUTION OF PRESENTERS (FIRST AUTHOR)?  ####

prop_first = data %>% 
  filter(talk.type%in%c("poster","oral","invited")) %>%
  mutate (gender = ifelse (presenter.sex == "W", 1, 0))

# Mean proportion of women as first authors

mean(prop_first$gender)

# Q: WHAT WAS THE GENDER DISTRIBUTION OF PI (LAST AUTHOR)?  ####

prop_last = data %>% filter(talk.type%in%c("poster","oral","invited")) %>%
  mutate (gender = ifelse (last.author.sex == "W", 1, 0))

# Mean proportion of women as first authors

mean(prop_last$gender)

# Q: COMBINATION OF FIRST AND LAST AUTHORS (First/Last) ####
# Data preparation 

data = data %>% mutate (sex.comb = ifelse (presenter.sex == "W" & last.author.sex == "W", "WW", 
                                     ifelse (presenter.sex == "W" & last.author.sex == "M", "WM",
                                     ifelse (presenter.sex == "M" & last.author.sex == "W", "MW", "MM"))))

# Descriptors

data %>% group_by (sex.comb) %>% 
          summarise(n = n()) %>% mutate(freq = n / sum(n)) # MM = 40%, MW = 11%, WM = 32%, WW = 17%

# Stats: chi.-squared btw different combinations

proptot = data %>% mutate(sex.comb = as.factor (sex.comb)) %>%
  count(sex.comb) %>% 
  mutate(prop = prop.table(n)*100)

# Chi-test 

prop.test(proptot$n, n=rep(sum(proptot$n),4), p=c(0.25, 0.25, 0.25, 0.25))

#  SECTION 3: Gender biases among awards ########################################### 
## Data file = Data_Block1.csv
## Stats: Descriptors 

# Q: GENDER DISTRIBUTION AMONG AWARD CANDIDATES  ####

# Data preparation

setwd(DataPath)
data_all = read.csv("Data_Block1.csv")

data = data_all %>% dplyr::select (talk.id, talk.type, session, prize, presenter.sex, Winner, presenter.stage) %>%
                      filter (presenter.stage == "pre-doctoral"|presenter.stage == "post-doctoral") ## note that not all post-doctoral reseachers could apply (only the ones that defended < 2 years ago)

# Number of candidates

data %>% count(prize) # 238 candidates from 353 possibles = 67%

# Number of women among candidates

data %>% filter (prize=="TRUE") %>%
          count(presenter.sex) %>% mutate(prop = prop.table(n)*100) # candidates: women = 130 (55%)

# Number of women by career stage

data %>% filter( prize=="TRUE") %>%
          group_by(presenter.sex) %>% 
          count(presenter.stage) %>% mutate(prop = prop.table(n)*100) # Women = 94 (72%) predoc and 36 (27%) postdoc

# Q: GENDER DISTRIBUTION AMONG WINNERS  ####

# Number of candidates

data %>% filter(prize=="TRUE") %>% 
  count(Winner) # 6 winners

# Number of women among winners

data %>% filter (Winner=="YES") %>%
  count(presenter.sex) %>% mutate(prop = prop.table(n)*100) # 9 Women (82%)

#  SECTION 4: Gender biases among chairs ######################################
## Data file = Data_Block1.csv
## Figures = figure 2b.
## Stats: Descriptors + linear regression models to know relation between % women chairs and % women presenters 

# Data Preparation 

setwd(DataPath)
data_all= read.csv("Data_Block1.csv") 

data = data_all %>% dplyr::select (talk.type, session.type, session, presenter.sex, presenter.stage, prop.coauthor.women, prop.chair.women, prop.chair.permanent) %>%
                      filter (session != "PT")

# Q: PROPORTION OF CHAIRWOMEN ####
## Balance = 40-60% chairs women.
## Stats: Descriptors

data = data %>% mutate ( bias = ifelse (prop.chair.women > 60, 
                                        "Bias against women", 
                                        ifelse (prop.chair.women >= 40 & prop.chair.women <= 60, 
                                                 "Balanced", "Bias against men"))) 
data = data %>% mutate(bias = as.factor(data$bias))

# All sessions together 

data %>% distinct(session, .keep_all = T) %>% 
  group_by(bias) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) #15%W, 37% balanced, 48%M

# General sessions 

data %>% filter(session.type == "GS") %>% 
          distinct(session, .keep_all = T) %>% 
  group_by(bias) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) #40%W, 60%M

# Thematic sessions 

data %>% filter(session.type == "TS") %>% 
          distinct(session, .keep_all = T) %>% 
  group_by(bias) %>% summarise(n = n()) %>% mutate(freq = (n / sum(n))*100) #18%W, 36% balanced, 46%M

# Q: RELATION BETWEEN % CHAIRWOMEN AND % WOMEN PRESENTERS ####
## Stats: linear regression model

data_subset_1 = data %>% dplyr::select (presenter.sex, session) %>% 
  group_by(session, presenter.sex) %>%  
  summarise (presenter.women = n()) %>%  
  mutate(prop.presenter.women = presenter.women / sum(presenter.women)) %>% 
  filter (presenter.sex == "W") %>%
  dplyr::select (session, prop.presenter.women)
data_subset_2 = data %>% dplyr::select (session, prop.chair.women) %>%
                           group_by(session) %>%  summarise_all (mean)
data_subset = full_join (data_subset_1, data_subset_2)

mod = lm(data_subset$prop.presenter.women ~data_subset$prop.chair.women)
(mod.res1 = summary(mod)) # model parameters -- slope positive
(mod.res2 = Anova(mod, type=3)) # p = 0.331, no relation


# Q: RELATION BETWEEN % CHAIRMEN AND % MEN PRESENTERS ####
## Stats: linear regression model

data_subset_1 = data %>% dplyr::select (presenter.sex, session) %>% 
  group_by(session, presenter.sex) %>%  
  summarise (presenter.men = n()) %>%  
  mutate(prop.presenter.men = presenter.men / sum(presenter.men)) %>% 
  filter (presenter.sex == "M") %>%
  dplyr::select (session, prop.presenter.men)
data_subset_2 = data %>%  mutate(prop.chair.men = 1- prop.chair.women) %>% 
  dplyr::select (session, prop.chair.men) %>% 
  group_by(session) %>%  summarise_all (mean)
data_subset = full_join (data_subset_1, data_subset_2)

mod = lm(data_subset$prop.presenter.men ~data_subset$prop.chair.men)
(mod.res1 = summary(mod)) # model parameters -- slope positive
(mod.res2 = Anova(mod, type=3)) # p = 0.314, no relation


# Q: RELATION BETWEEEN % CHAIRWOMEN AND % CHAIRS WITH PERMANENT POSITIONS ####
# Stats: linear regression model

data_subset = data %>% distinct(session, .keep_all = T) 

mod = lm(data_subset$prop.chair.permanent~data_subset$prop.chair.women) 
(mod.res1 = summary(mod)) # model parameters -- slope = negative
(mod.res2 = Anova(mod, type=3)) # p = 0.009, negative relation btw % women and % permanent

