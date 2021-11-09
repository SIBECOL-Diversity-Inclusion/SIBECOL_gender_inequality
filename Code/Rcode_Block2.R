#----------------------------------------------------------------------------------------------
# PROJECT: SIBECOL GENDER ANALYSIS / BLOCK 2
# DESCRIPTION: Code to analyze the gender biases in the SIBECOL attendants. 
# AUTHORS: Anna Lupon*, Pablo Rodríguez-Lozano, Mireia Bartrons, Alba Anadon-Rosell, 
# Meritxell Batalla, Susana Bernal, Andrea G. Bravo, Pol Capdevila, Miguel Cañedo-Argüelles, 
# Núria Catalán, Ana Genua-Olmedo, Cayetano Gutiérrez-Cánovas, Maria João Feio, 
# Federica Lucati, Gabriela Onandia, Sílvia Poblador, Roser Rotchés-Ribalta, Anna 
# Sala-Bubaré, María Mar Sánchez-Montoya, Marta Sebastián, Aitziber Zufiaurre, Ada Pastor 
# *Corresponding author: alupon@ceab.csic.es
#----------------------------------------------------------------------------------------------

rm(list = ls())

# Load packages 

library(viridis)  
library(nlme)
library(lme4)
library(MuMIn)
library(lattice)
library(ggpubr)
library(multcomp)
library(plm)
library(reshape)
library(car)
library(cowplot)
library (smatr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Set working directories
path = gsub("/Code", "", dirname(rstudioapi::getActiveDocumentContext()$path))

DataPath = paste0(path,"/Data")
ResultPath = paste0(path, "/Results") 

#------------------------------------------------------------------------------
#  SECTION 1: ATTENDANCE TO TALKS
#------------------------------------------------------------------------------
# Data file: "Data_Block2.csv"

# DATA PREPARATION #### 

setwd(DataPath)
dades_all = read.csv(file="Data_Block2.csv", header=TRUE, sep=";")
dades = dades_all %>% select (talk.type, session, speaker.sex, chair.sex, attendees, attendees.women, attendees.men)


# Q: WHAT WAS THE GENDER DISTRIBUTION OF ATTENDEES? ####
# Stat test: descriptors

dades = dades %>% mutate (attendees.women.prop = 100*attendees.women/attendees) %>%
                  mutate (attendees.men.prop = 100*attendees.men/attendees)
summary (dades) ## Women attendees from 26 to 81%,, mean = 52%, median = 53%, IQR = 45-60% 


# Q: WHAT WAS THE GENDER DISTRIBUTION OF ATTENDEES FOR EACH CONTRIBUTION TYPE ####
# Stat test: t-test btw % women attendants & % women registered (48%)

## Attendance in Oral talks ----------------------------------------------------------------

dadesOral = dades %>% filter (talk.type == "oral")
shapiro.test(dadesOral$attendees.women.prop) # normal distribution

mean(dadesOral$attendees.women.prop, na.rm=T)

t.test(dadesOral$attendees.women.prop, alternative='less', mu = 48) # p = 1, no sig.
t.test(dadesOral$attendees.women.prop, alternative='greater', mu = 52) # p = 0.066, no sig.

## Attendance in Invited talks --------------------------------------------------------------

dadesInv = dades %>% filter(talk.type == "invited")
shapiro.test(dadesInv$attendees.women.prop) # normal distribution (weird visual)

mean(dadesInv$attendees.women.prop, na.rm=T)

t.test(dadesInv$attendees.women.prop, alternative='less', mu = 48) # p = 0.681, no sig.
t.test(dadesInv$attendees.women.prop, alternative='greater', mu = 52) # p = 0.868, no sig.

## Attendance in Plenary talks -------------------------------------------------------------

dadesPlen = dades %>% filter (talk.type == "plenary")
shapiro.test(dadesPlen$attendees.women.prop) # normal distribution

mean(dadesPlen$attendees.women.prop, na.rm=T)

t.test(dadesPlen$attendees.women.prop, alternative='less', mu = 48) # p = 0.130, no sig
t.test(dadesPlen$attendees.women.prop, alternative='greater', mu = 52) # p = 0.972, no sig


# Q: DID THE GENDER OF THE SPEAKER INFLUENCE THE AMOUNT OF ATTENDEES? ####
# Stat test: descriptors + t-test btw attendees.women & attendees.men

## Attendance in Oral talks ----------------------------------------------------------------

dadesOralWomen = dadesOral %>% filter (speaker.sex == "W")
dadesOralMen = dadesOral %>% filter (speaker.sex == "M")

mean (na.omit(dadesOralMen$attendees))/ mean (na.omit(dadesOralWomen$attendees)) # +12% 
t.test(dadesOralWomen$attendees, dadesOralMen$attendee) # p <0.001, sig. 

## Attendance in Invited talks --------------------------------------------------------------

dadesInvWomen = dadesInv %>% filter (speaker.sex == "W")
dadesInvMen = dadesInv %>% filter (speaker.sex == "M")

mean (na.omit(dadesInvMen$attendees))/ mean (na.omit(dadesInvWomen$attendees)) # +7% 
t.test(dadesInvWomen$attendees, dadesInvMen$attendee) # p = 0.015, sig. 

## Attendance in Plenary talks -------------------------------------------------------------

dadesPlenWomen = dadesPlen %>% filter (speaker.sex == "W")
dadesPlenMen = dadesPlen %>% filter (speaker.sex == "M")

mean (na.omit(dadesPlenMen$attendees))/mean (na.omit(dadesPlenWomen$attendees) ) # +18% 
t.test(dadesPlenWomen$attendees, dadesPlenMen$attendee) # p = 0.012, sig.


# Q: DID THE GENDER OF ATTENDEES CHANGE DEPENDING ON THE GENDER OF THE SPEAKER? ####
# Stat test: linear regression including and excluding speaker.sex as fixed effect.
# We analyzed all contribution types together because some of them (i.e. plenary talks) have few data.

mod.with.gender = glmer(cbind(attendees.women, attendees.men) ~ speaker.sex + (1|session), family="binomial", data=dades, na.action=na.exclude)
mod.without.gender = glmer(cbind(attendees.women, attendees.men) ~ 1 + (1|session), family="binomial", data=dades, na.action=na.exclude)
AICc(mod.with.gender,mod.without.gender) # no diff btw models (AICc < 2), no influence of speakers gender on attendance gender distribution.

# Assumptions of both models --> all figures look very good
op <- par(mfrow = c(2, 2))
plot(mod.with.gender)
EP <- resid(mod.with.gender, type = "pearson")
ED <- resid(mod.with.gender, type = "deviance")
Fit <- predict(mod.with.gender, type = "response") #Fitted
plot(EP~Fit, main = "Pearson residuals")
plot(EP~speaker.sex, main = "Pearson residuals", data=dades)
plot(ED~Fit, main = "Deviance residuals")
plot(ED~speaker.sex, main = "Deviance residuals", data=dades)
plot(predict(mod.with.gender),residuals(mod.with.gender))

op <- par(mfrow = c(2, 2))
plot(mod.without.gender)
EP <- resid(mod.without.gender, type = "pearson")
ED <- resid(mod.without.gender, type = "deviance")
Fit <- predict(mod.without.gender, type = "response") #Fitted
plot(EP~Fit, main = "Pearson residuals")
plot(ED~Fit, main = "Deviance residuals")
plot(predict(mod.without.gender),residuals(mod.without.gender))




#------------------------------------------------------------------------------------
# # SECTION 2: ASKING BEHAVIOUR (Alba, Núria, Txell B., Roser, Federica & Sílvia)
#------------------------------------------------------------------------------------
# Data file: "Data_Block2.csv"
# Associated figures: Fig 3 and Fig 4

# DATA PREPARATION #### 

setwd(DataPath)
dades_all = read.csv(file="Data_Block2.csv", header=TRUE, sep=";")
dades = dades_all %>% select (talk.id, talk.type, session, speaker.sex, chair.sex, attendees, attendees.women, attendees.men, total.questions, questions.women, questions.men)

# Q: HOW MANY TALKS DID NOT GET ANY QUESTION? WAS IT RELATED TO THE GENDER OF THE SPEAKER/CHAIR? ####
# Stat test: descriptors + chi-squeared btw questions.YN & speaker.sex or chair.sex

count(dades %>% filter (total.questions == 0)) # 47 talks with no questions 
count(dades %>% filter (total.questions == 0))/count (dades) # 22% of talks with no questions 

dades = dades %>% mutate (questions.YN = ifelse (total.questions > 0, 0, 1))

chisq.test(dades$questions.YN, dades$speaker.sex) # p = 0.98, no dif for speaker.sex
chisq.test(dades$questions.YN, dades$chair.sex) # p = 0.35, no dif for chair.sex


# Q: DID WOMEN AND MEN SPEAKERS RECIVE THE SAME AMOUNT OF QUESTIONS ?####
# Stat test: descriptors + t.test btw total.questions & speaker.sex

## only for talks with questions (n = 177) ----------------------------------------------------------------

dades %>% select (speaker.sex, total.questions) %>% filter (total.questions > 0) %>%
          group_by(speaker.sex) %>% summarize_all(mean) # questions: W = 2.05, M = 1.93

t.test(total.questions ~ speaker.sex, subset(dades, total.questions >0)) # p = 0.491, no dif for speakr.sex


# Q: WAS THE AMOUNT OF QUESTIONS RAISED BY WOMEN PROPORTIONAL TO WOMEN ATTENDEES? ####
# Stat test: t.test  btw % attendees.women & questions.women 

dades = dades %>% mutate (propA.women = attendees.women/attendees)
dades = dades %>% mutate (propQ.women = questions.women/total.questions)

t.test(dades$propA.women,dades$propQ.women) # p < 0.001, differences.

# Stat test: liner regression model = how far is our data from the teoretical 1:1 line? (from Carter et al. 2016)

model = lm(propQ.women~propA.women, dades) 
summary(model) # slope = 1.15, intercept = -0.229, p < 0.001, at propA.female = 1, propQ.female = 0.92
slope.test(dades$propQ.women, dades$propA.women, test.value = 1, method = "OLS", alpha = 0.05, intercept = TRUE) # no dif from 1:1
predicted_df <- data.frame(propQ.pred = predict(model, dades), propA.women=dades$propA.women) # save predictions of the model 


# Q: WAS THE Q&A BEHAVIOUR INFLUENCED BY THE GENDER OF SPEAKERS OR CHAIRS? ####
# Stat test: t-test btw questions.women & questions.men (corrected by attendants).

dades = dades %>% mutate (q.women.corr = questions.women/attendees.women) %>%
                  mutate (q.men.corr = questions.men/attendees.men)

## Gender speaker ----------------------------------------------------------------                           

dades.speaker.W = dades %>% filter (speaker.sex == "W")
dades.speaker.M = dades %>% filter (speaker.sex == "M")

t.test (dades.speaker.W$q.women.corr, dades.speaker.W$q.men.corr) # p =0.079, no diff (speaker woman -> women questions = men questions)
t.test (dades.speaker.M$q.women.corr, dades.speaker.M$q.men.corr) # p < 0.001 -- diff (speaker man -> women questions << men questions)

## Gender chair ----------------------------------------------------------------    

dades.chair.W = dades %>% filter (chair.sex == "W")
dades.chair.M = dades %>% filter (chair.sex == "M")

t.test (dades.chair.W$q.women.corr, dades.chair.W$q.men.corr) # p =0.096, no diff (chair woman -> women questions = men questions)
t.test (dades.chair.M$q.women.corr, dades.chair.M$q.men.corr) # p < 0.001 -- diff (chair man -> women questions << men questions)

