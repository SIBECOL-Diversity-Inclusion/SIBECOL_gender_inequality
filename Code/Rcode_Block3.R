#----------------------------------------------------------------------------------------------
# PROJECT: SIBECOL GENDER ANALYSIS / BLOCK 3
# DESCRIPTION: Code to represent the results from the survey
#              part of the SIBECOL gender bias study
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

library(reshape)
library(plyr)
library(tidyverse)
library(sqldf)
library(lme4)
library(lmerTest)
library(pbkrtest)
library(nlme) # to plot results
library(MuMIn)
library(multcomp) # robust post-hoc
library(sandwich)
library(variancePartition)
library(usdm)
library(psych)
library(GPArotation)
library(cowplot)
library(car)
library(dplyr)

# Set working directories

path = gsub("/Code", "", dirname(rstudioapi::getActiveDocumentContext()$path))

DataPath = paste0(path,"/Data")
ResultPath = paste0(path, "/Results") 

# Loading data

setwd(DataPath)
dat=read.table("Data_Block3.txt",h=T,sep="\t")

# Transform gender into a factor and set the career stage 
# categories into 4

dat = dat %>% 
  mutate(career.stage = ordered(career.stage, c("Pre-doctoral", "Post-doctoral", "Senior non-permanent", "Senior permanent")),
         gender = as.factor(gender))

#---------------------------------------------------------------------------------
# SECTION 1: DESCRIPTOR ANALYSES #####
#---------------------------------------------------------------------------------
# Data = "Data_Block3.txt"
#Stat test: descriptors

# Amount of respondents

count(dat) # 232
100*count(dat)/722 # 32% (722 conference attendees)

# Gender distribution (Q7.3)  

dat %>% 
  group_by(gender) %>%  
  summarise(n = dplyr::n()) %>%  
  mutate(freq = (n / sum(n))*100) # 60% W, 38% M, 1% NA

# Career stage distribution (Q7.7)  

dat %>% 
  group_by(career.stage) %>%  
  summarise(n = n()) %>%  
  mutate(freq =round( (n / sum(n))*100,0)) # 31% predoc, 37% postdoc, 11% senior non-perm, 18% senior perm, 3% NA

# Nationality (Q7.5)

dat %>% 
  group_by(q7.5) %>%  
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 83% spain 

# Country where respondents live (Q7.6)

dat %>% 
  group_by(q7.6) %>% 
  summarise(n = n()) %>%   
  mutate(freq = (n / sum(n))*100) # 75.4% live in Spain, 4.31 in 185 and 2.59 in 61

# Institution where respondents work (Q7.8)

dat %>% 
  group_by(inst) %>%  
  summarise(n = n()) %>%  
  mutate(freq = (n / sum(n))*100)

# Responders identifying themselves among any minority group (Q7.4)
# 1. Race, 2. LGBT, 3. Disabilities, 4. Others, 5. None

dat %>% 
  group_by(minority) %>%  
  summarise(n = n()) %>%  
  mutate(freq = (n / sum(n))*100)



#---------------------------------------------------------------------------------
# SECTION 2: PERCEPTION DURING Q&A SESSIONS #####
#---------------------------------------------------------------------------------
# Data: "Data_Block3.txt"
# Stat test: GLM models (Gaussian or Binomial) + chi-squared tst
# Figures: Table 1, Fig 5

# Q1. WHO ASKED MORE QUESTIONS: WOMEN OR MEN? Q2.5 ------------------------------------------
# 1. W > M, 2. W < M, 3. W = M, 4. NS/NC

# Total respondents

dat %>%  
  group_by(q2.5) %>% 
  drop_na(q2.5) %>%  
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) #32% perceived women asked fewer questions, 39% didn't know 

# Perception by gender 

dat %>%  
  drop_na(gender) %>% 
  group_by(q2.5,gender) %>%   
  drop_na(q2.5) %>%   
  summarise(n = n()) %>%  
  group_by(gender) %>%   
  mutate(freq = (n / sum(n))*100) ## 21% of men and 39% of women answered 2 (women asked fewer questions)

# By gender and career stage, only for people that answered "2" (women asked fewer questions) -- FIGURE 5A

dat_subset = dat %>%  
  drop_na(gender, career.stage) %>% 
  group_by(q2.5,gender, career.stage) %>% 
  drop_na(q2.5) %>% 
  summarise(n = n()) %>%
  group_by(gender, career.stage) %>%
  mutate(freq = (n / sum(n))*100) %>% 
  filter(q2.5 == "2")

mod = glm(freq~gender+career.stage, family="gaussian", dat_subset)
Anova(mod, type=3) # Gender p = 0.009, career stage p=0.076 

#Post hoc test
post_hoc = glht(mod, mcp(career.stage="Tukey")) 
summary(post_hoc) 



# Q2. DID YOU ASK QUESTIONS WHENEVER YOU WANT? Q2.4 ------------------------------------------------   
# 1. Yes, 2. No. 3. I did not asked questions bc I did not want to.
     
# Total respondants

dat %>%  
  group_by(q2.4) %>% 
  drop_na(q2.4) %>%  
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) #30% asked when they wanted

# By gender 

dat %>%  
  drop_na(gender) %>% 
  group_by(q2.4,gender) %>%   
  drop_na(q2.4) %>%   
  summarise(n = n()) %>%  
  group_by(gender) %>%   
  mutate(freq = (n / sum(n))*100) # "1" = Yes, 38.2% of men and 25% of women.

# By gender and career stage, only for people that answered "1" (I asked questions whenever I wanted) -- FIGURE 5B

dat_subset = dat %>% 
  drop_na(gender, career.stage) %>% 
  group_by(q2.4,gender, career.stage) %>% 
  drop_na(q2.4) %>% 
  summarise(n = n()) %>%
  group_by(gender, career.stage) %>%
  mutate(freq = (n / sum(n))*100) %>% 
  filter(q2.4 == "1")

mod = glm(freq~gender+career.stage,family = "gaussian", dat_subset)
Anova(mod, type=3) # Gender p<0.01, career stage p<0.01 



# Q3. THE CHAIR WAS GENDER BIASED WHEN CHOOSING QUESTIONERS. Q3.3 ------------------------------------------------------------------------------- 
# 1. strongly disagree and 5. strongly agree
# NS = the question was not formulated

# Total

dat %>% 
  drop_na(q3.3) %>% 
  filter(q3.3 != "NS") %>% 
  group_by(q3.3) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 88% asnwered there was no gender bias (i.e. answered 1 or 2)

# Mean value of the response

dat %>% 
  mutate (q3.3 = as.numeric (as.character(q3.3))) %>%
  drop_na(q3.3) %>% 
  summarise(mean= mean(q3.3, na.rm=T))

# By gender 

dat %>% 
  mutate(q3.3=as.numeric(as.character(q3.3))) %>% 
  drop_na(q3.3) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q3.3),
         sd=sd(q3.3))

#By career stage  

dat %>% 
  mutate(q3.3=as.numeric(as.character(q3.3))) %>% 
  drop_na(q3.3) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q3.3),
            sd=sd(q3.3))
# Test

dat_subset = dat %>% 
  mutate(q3.3=as.numeric(as.character(q3.3))) %>% 
  drop_na(q3.3)

mod = glm(q3.3~gender+career.stage, family = "gaussian", dat_subset)
Anova(mod, type=3) # Gender p = 0.72, career stage p = 0.43



# Q4. QUESTIONS WERE CONTRUCTIVE Q3.4 ------------------------------------------------------------------------------- 
# 1. Strongly disagee - 5. Strongly agree

# Total

dat %>% 
  drop_na(q3.4) %>% 
  filter(q3.4!="NS") %>% 
  group_by(q3.4) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 85% asnwered they were contructive (answered 4 or 5)

dat %>% 
  drop_na(q3.4) %>% 
  filter(q3.4!= "NS") %>% 
  mutate (q3.4 = as.numeric (as.character(q3.4))) %>% 
  summarise(mean=mean(q3.4)) # mean = 4.19

# By gender  

dat %>% 
  drop_na(q3.4) %>% 
  filter(q3.4!="NS") %>% 
  mutate(q3.4=as.numeric(as.character(q3.4))) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q3.4),
            sd = sd(q3.4))

# By career stage

dat %>% 
  drop_na(q3.4) %>% 
  filter(q3.4!="NS") %>% 
  mutate(q3.4=as.numeric(as.character(q3.4))) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q3.4),
            sd = sd(q3.4))
# Test
dat_subset = dat %>% 
  drop_na(q3.4) %>% 
  filter(q3.4!="NS") %>% 
  mutate(q3.4=as.numeric(as.character(q3.4)))
mod = glm(q3.4~gender+career.stage, family = "gaussian", dat_subset)
Anova(mod, type=3) # Gender p = 0.62, career stage p=0.13



# Q5. QUESTIONS WERE POLITE Q3.5 ------------------------------------------------------------------------------- 
# 1. Strongly disagee - 5. Strongly agree

# Total

dat %>% 
  drop_na(q3.5) %>% 
  filter(q3.5!="NS") %>% 
  group_by(q3.5) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 98.5% asnwered they were polite (answered 4 or 5)

dat %>% 
  drop_na(q3.5) %>% 
  filter(q3.5!= "NS") %>% 
  mutate (q3.5 = as.numeric (as.character(q3.5))) %>% 
  summarise(mean=mean(q3.5))# mean = 4.53

# By gender 

dat %>% 
  drop_na(q3.5) %>% 
  filter(q3.5!="NS") %>% 
  mutate(q3.5=as.numeric(as.character(q3.5))) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q3.5),
            sd=sd(q3.5))

# By career stage

dat %>% 
  drop_na(q3.5) %>% 
  filter(q3.5!="NS") %>% 
  mutate(q3.5=as.numeric(as.character(q3.5))) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q3.5),
            sd=sd(q3.5))

# Test

dat_subset = dat %>% 
  drop_na(q3.5) %>% 
  filter(q3.5!="NS") %>% 
  mutate(q3.5=as.numeric(as.character(q3.5)))
mod = glm(q3.5~gender+career.stage, family = "gaussian", dat_subset)
Anova(mod, type=3) # Gender p = 0.30, career stage p=0.58 



# Q6. I FELT SATISFIED WITH MY ANSWERS (Q3.6)------------------------------------------------------------------------------- 
# 1. Strongly disagree - 5. Strongly agree

# Total

dat %>% 
  drop_na(q3.6) %>% 
  filter(q3.6!="NS") %>% 
  group_by(q3.6) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 85.7% asnwered they were polite (answered 4 or 5)

dat %>% 
  drop_na(q3.6) %>% 
  filter(q3.6!= "NS") %>% 
  mutate (q3.6 = as.numeric (as.character(q3.6))) %>% 
  summarise(mean=mean(q3.6)) # mean = 3.99

# By gender 

dat %>% 
  drop_na(q3.6) %>% 
  filter(q3.6!="NS") %>% 
  mutate(q3.6=as.numeric(as.character(q3.6))) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q3.6),
            sd=sd(q3.6))

# By career stage

dat %>% 
  drop_na(q3.6) %>% 
  filter(q3.6!="NS") %>% 
  mutate(q3.6=as.numeric(as.character(q3.6))) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q3.6),
            sd=sd(q3.6))

#Test 

dat_subset = dat %>% 
  drop_na(q3.6) %>% 
  filter(q3.6!="NS") %>% 
  mutate(q3.6=as.numeric(as.character(q3.6)))
mod = glm(q3.6~gender+career.stage, family = "gaussian", dat_subset)
Anova(mod, type=3) # Gender p = 0.49, career stage p=0.30



#---------------------------------------------------------------------------------
# SECTION 3: PERCEPTION CONFERENCE #################################################################
#---------------------------------------------------------------------------------
# Data: "Data_Block3.txt"
# Stat test: GLM models (Gaussian or Binomial) + chi-squared tst


# Q1. HOW OFTEN DID YOU HEAD ANY GENDER STEREOTYPICAL REMARK? (Q5.4) ----------------------------------------------------------
## 1- Never - 5-Very frequently (the higher the more sexist)

# Total

dat %>% 
  group_by(q5.4) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 86% asnwered they didn't head stereotypical remarks (i.e. answered 1 or 2)

# Mean response 

dat %>% 
  mutate (q5.4 = as.numeric (as.character(q5.4))) %>% 
  drop_na(q5.4) %>% 
  summarise(mean=mean(q5.4))

# By gender 

dat %>% 
  mutate(q5.4=as.numeric(as.character(q5.4))) %>% 
  drop_na(q5.4) %>%
  group_by(gender) %>% 
  summarise(mean=mean(q5.4),
            sd=sd(q5.4))

# Career stage 

dat %>% 
  mutate(q5.4=as.numeric(as.character(q5.4))) %>% 
  drop_na(q5.4) %>%
  group_by(career.stage) %>% 
  summarise(mean=mean(q5.4),
            sd=sd(q5.4))

# Test 

dat_subset = dat %>% 
  mutate (q5.4 = as.numeric (as.character(q5.4))) %>% 
  drop_na(q5.4) 
mod=glm(q5.4~gender+career.stage, family="gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.04, career stage p=0.21 



# Q2. I AM SATISFIED WITH MY SOCIAL INTERACTIONS (q4.4) ------------------------------------------------------------
# 1-Strongly disagree, 5-Strongly agree 

# Total

dat %>% 
  group_by(q4.4) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 72% asnwered they were satisfied (i.e. answered 4 or 5)

dat %>% 
  dplyr::select(q4.4) %>% 
  mutate(q4.4 = as.numeric (as.character(q4.4))) %>% 
  summarise(mean=mean(q4.4, na.rm=T)) # mean = 3.76

# By gender 

dat %>% 
  mutate(q4.4=as.numeric(as.character(q4.4))) %>% 
  drop_na(q4.4) %>%
  group_by(gender) %>% 
  summarise(mean=mean(q4.4),
            sd=sd(q4.4))

# By career stage

dat %>% 
  mutate(q4.4=as.numeric(as.character(q4.4))) %>% 
  drop_na(q4.4) %>%
  group_by(career.stage) %>% 
  summarise(mean=mean(q4.4),
            sd=sd(q4.4))

# Test

dat_subset = dat %>% 
  mutate(q4.4=as.numeric(as.character(q4.4))) %>% 
  drop_na(q4.4) 
mod=glm(q4.4~gender+career.stage, family="gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.12, career stage p=0.15 



# Q3. IN GENERAL, I LIKED THE CONFERENCE (q4.6)------------------------------------------------------------
# 1-Strongly disagree, 5-Strongly agree 

# Total

dat %>% 
  group_by(q4.6) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 91% asnwered they were satisfied (i.e. answered 4 or 5)

dat %>% 
  dplyr::select (q4.6) %>% 
  mutate (q4.6 = as.numeric (as.character(q4.6))) %>% 
  summarise(mean=mean(q4.6, na.rm=T)) # mean = 4.31

# By gender 

dat %>% 
  mutate (q4.6 = as.numeric (as.character(q4.6))) %>%
  drop_na(q4.6) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q4.6),
            sd= sd(q4.6))

# By career stage

dat %>% 
  mutate (q4.6 = as.numeric (as.character(q4.6))) %>%
  drop_na(q4.6) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q4.6),
            sd= sd(q4.6))

# Test 

dat_subset = dat %>% 
  mutate (q4.6 = as.numeric (as.character(q4.6))) %>%
  drop_na(q4.6) 
mod=glm(q4.6~gender+career.stage, family = "gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.58, career stage p=0.04 



# Q4. HOW OFTEN DID YOU FEEL EXCUDED FROM SOCIAL ACTIVITIES? (Q5.1) ------------------------------------------------------------
# 1-Never, 5-Very frequently 

# Total

dat %>% group_by(q5.1) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 73% asnwered they were no extuded (i.e. answered 1 or 2)

dat %>% 
  dplyr::select(q5.1) %>% 
  mutate(q5.1 = as.numeric (as.character(q5.1))) %>% 
  summarise(mean=mean(q5.1, na.rm=T)) # mean = 1.99

# By gender

dat %>% 
  mutate(q5.1=as.numeric(as.character(q5.1))) %>% 
  drop_na(q5.1) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q5.1),
            sd=sd(q5.1))

# Career stage

dat %>% 
  mutate(q5.1=as.numeric(as.character(q5.1))) %>% 
  drop_na(q5.1) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q5.1),
            sd=sd(q5.1))

# Test

dat_subset = dat %>% 
  mutate(q5.1=as.numeric(as.character(q5.1))) %>% 
  drop_na(q5.1)
mod=glm(q5.1~gender+career.stage, family= "gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.96, career stage p=0.002



# Q5. HOW OFTEN SOMEONE PUT YOU DOWN? (Q5.3) ------------------------------------------------------------
# 1-Never, 5-Very frequently 

# Total

dat %>% 
  group_by(q5.3) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 91% asnwered they were no put down (i.e. answered 1 or 2)

dat %>% 
  mutate (q5.3 = as.numeric (as.character(q5.3))) %>% 
  summarise(mean=mean(q5.3, na.rm=T)) # mean = 1.28

# By gender 

dat %>% 
  mutate(q5.3=as.numeric(as.character(q5.3))) %>% 
  drop_na(q5.3) %>%
  group_by(gender) %>% 
  summarise(mean=mean(q5.3),
            sd=sd(q5.3))

# By career stage

dat %>% 
  mutate(q5.3=as.numeric(as.character(q5.3))) %>% 
  drop_na(q5.3) %>%
  group_by(career.stage) %>% 
  summarise(mean=mean(q5.3),
            sd=sd(q5.3))
# Test

dat_subset = dat %>% 
  mutate(q5.3=as.numeric(as.character(q5.3))) %>% 
  drop_na(q5.3) 
mod=glm(q5.3~gender+career.stage, family = "gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.34, career stage p=0.49



# Q6. OTHERS CAME TO ME TO DISCUSS INTELLECTUAL IDEAS (q4.1) ---------------------------------------------------------
# 1- Strongly disagree, 5- Strongly agree

# Total

dat %>% 
  group_by(q4.1) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 66% asnwered they discussed ideas (i.e. answered 4 or 5)

dat %>% 
  mutate (q4.1 = as.numeric (as.character(q4.1))) %>% 
  summarise(mean=mean(q4.1, na.rm=T)) # mean = 3.62

# By gender 

dat %>% 
  mutate(q4.1=as.numeric(as.character(q4.1))) %>% 
  drop_na(q4.1) %>%
  group_by(gender) %>% 
  summarise(mean=mean(q4.1),
            sd=sd(q4.1))

# By career stage

dat %>% 
  mutate(q4.1=as.numeric(as.character(q4.1))) %>% 
  drop_na(q4.1) %>%
  group_by(career.stage) %>% 
  summarise(mean=mean(q4.1),
            sd=sd(q4.1))
# Test

dat_subset = dat %>% 
  mutate(q4.1=as.numeric(as.character(q5.3))) %>% 
  drop_na(q4.1) 
mod=glm(q4.1~gender+career.stage, family = "gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.34, career stage p=0.49



# Q7. I AM SATISIFIED WITH THE LEVEL OF INTELLECTUAL STIMULATION (q4.5) ---------------------------------------------------------
# 1- Strongly disagree, 5- Strongly agree

# Total

dat %>% group_by(q4.5) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 87% asnwered they were satisfied (i.e. answered 4 or 5)

dat %>% 
  mutate (q4.5 = as.numeric (as.character(q4.5))) %>% 
  summarise(mean=mean(q4.5, na.rm=T)) # mean = 4.10

# By gender

dat %>% 
  mutate(q4.5=as.numeric(as.character(q4.5))) %>% 
  drop_na(q4.5) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q4.5),
            sd=sd(q4.5))

# By career stage

dat %>% 
  mutate(q4.5=as.numeric(as.character(q4.5))) %>% 
  drop_na(q4.5) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q4.5),
            sd=sd(q4.5))

# Test 

data_subset = dat %>% 
  mutate(q4.5=as.numeric(as.character(q4.5))) %>% 
  drop_na(q4.5) 
mod=glm(q4.5~gender+career.stage, family="gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.64, career stage p=0.021

# Post hoc
post_hoc <- glht(mod, mcp(career.stage="Tukey")) 
summary(post_hoc) # result



# Q8. HOW OFTEN DID SOMEONE PAY LITTLE ATTENTION TO YOUR STATEMENT (q5.2) ---------------------------------------------------------
# 1- Never, 5-Very frequently

# Total

dat %>% 
  group_by(q5.2) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 73% asnwered that did not happen (i.e. answered 1 or 2)

dat %>% 
  mutate (q5.2 = as.numeric (as.character(q5.2))) %>% 
  mutate(mean=mean(q5.2, na.rm=T)) # mean = 2.00

# By gender

dat %>% 
  mutate(q5.2=as.numeric(as.character(q5.2))) %>% 
  drop_na(q5.2) %>% 
  group_by(gender) %>% 
  summarise(mean=mean(q5.2),
            sd=sd(q5.2))

# By career stage

dat %>% 
  mutate(q5.2=as.numeric(as.character(q5.2))) %>% 
  drop_na(q5.2) %>% 
  group_by(career.stage) %>% 
  summarise(mean=mean(q5.2),
            sd=sd(q5.2))

# Test 

data_subset = dat %>% 
  mutate(q5.2=as.numeric(as.character(q5.2))) %>% 
  drop_na(q5.2) 
mod=glm(q5.2~gender+career.stage, family="gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.51, career stage p=0.36



# Q9. I SUFFERED IMPOSTOR SYNDROME (q4.2) -------------------------------------------------------------------------------
# 1- Strongly disagree, 5- Strongly agree

# Total

dat %>% 
  group_by(q4.2) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) # 28% asnwered they suffered impostor syndrome (i.e. answered 4 or 5)

dat %>% 
  mutate (q4.2 = as.numeric (as.character(q4.2))) %>% 
  summarise(mean = mean(q4.2, na.rm=T)) # mean = 2.53

# By gender 

dat %>% 
  mutate(q4.2=as.numeric(as.character(q4.2))) %>% 
  drop_na(q4.2) %>% 
  group_by(gender) %>% 
  summarise(mean= mean(q4.2),
            sd = sd(q4.2))

# By career stage

dat %>% 
  mutate(q4.2=as.numeric(as.character(q4.2))) %>% 
  drop_na(q4.2) %>% 
  group_by(career.stage) %>% 
  summarise(mean= mean(q4.2),
            sd = sd(q4.2))

# Test 

dat_subset = dat %>% 
  mutate(q4.2=as.numeric(as.character(q4.2))) %>% 
  drop_na(q4.2) 
mod=glm(q4.2~gender+career.stage, family = "gaussian", data= dat_subset)
Anova(mod, type=3) # Gender p = 0.008, career stage p=0.030

