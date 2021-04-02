#------------------------------------------------------------------------------
# PROJECT: SIBECOL GENDER ANALYSIS / BLOCK 2
# DESCRIPTION: Code to to analyze the attendance, gender bias in sessions and 
#              behavior during Q&A times.
# Authors: SIBECOL Diversity and Inclusion Commission 
#------------------------------------------------------------------------------
#Clean memory

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

#  SECTION 1: ATTENDANCE TO TALKS ##############################################
# Data file: "Data_B2.csv"
# Results: No gender bias on attendance.
# Figures: No figures.

# DATA PREPARATION #### 

setwd(DataPath)
dades_all = read.csv(file="data_2B.csv", header=TRUE, sep=";")
dades = dades_all %>% select (talk.type, session, speaker.sex, chair.sex, attendees, attendees.women, attendees.men)


# Q: WHAT WAS THE GENDER DISTRIBUTION OF ATTENDEES? ####

# STATS: descriptors

dades = dades %>% mutate (attendees.women.prop = 100*attendees.women/attendees) %>%
                  mutate (attendees.men.prop = 100*attendees.men/attendees)
summary (dades) ## Women attendees from 26 to 81%,, mean = 52%, median = 53%, IQR = 45-60% 


# Q: WHAT WAS THE GENDER DISTRIBUTION OF ATTENDEES FOR EACH CONTRIBUTION TYPE ####

# STATS: t-test btw % women attendants & % women registered (48%)

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

# STATS: descriptors + t-test btw attendees.women & attendees.men

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

mean (na.omit(dadesPlenMen$attendees))/ mean (na.omit(dadesPlenWomen$attendees)) # +18% 
t.test(dadesPlenWomen$attendees, dadesPlenMen$attendee) # p = 0.012, sig.

# Q: DID THE GENDER OF ATTENDEES CHANGE DEPENDING ON THE GENDER OF THE SPEAKER? ####

# STATS: linear regression including and excluding speaker.sex as fixed effect.
# We analyzed all contribution types together because some of them (i.e. plenary talks) have few data.

mod.with.gender = glmer(cbind(attendees.women, attendees.men) ~ speaker.sex + (1|session), 
                        family="binomial", data=dades, na.action=na.exclude)
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

# SECTION 2: ASKING BEHAVIOUR ##################################################
# Data file: "Data_2B.csv"
# Results: Women asked less questions than expected for their attendance. The 
# number of questions raised by women increased when the speaker or chair were 
# women.
# Figures: Two figures (Fig 4 and Fig 5):

# DATA PREPARATION #### 

setwd(DataPath)
dades_all = read.csv(file="data_2B.csv", header=TRUE, sep=";")
dades = dades_all %>% select (talk.id, talk.type, session, speaker.sex, chair.sex, attendees, attendees.women, attendees.men, total.questions, questions.women, questions.men)

# Q: HOW MANY TALKS DID NOT GET ANY QUESTION? WAS IT RELATED TO THE GENDER OF THE SPEAKER/CHAIR? ####

# STAT: descriptors + chi-squeared btw questions.YN & speaker.sex or chair.sex

count(dades %>% filter (total.questions == 0)) # 47 talks with no questions 
count(dades %>% filter (total.questions == 0))/count (dades) # 22% of talks with no questions 

dades = dades %>% mutate (questions.YN = ifelse (total.questions > 0, 0, 1))

chisq.test(dades$questions.YN, dades$speaker.sex) # p = 0.98, no dif for speaker.sex
chisq.test(dades$questions.YN, dades$chair.sex) # p = 0.35, no dif for chair.sex


# Q: DID WOMEN AND MEN SPEAKERS RECIVE THE SAME AMOUNT OF QUESTIONS ?####

# STAT: descriptors + t.test btw total.questions & speaker.sex

## all data (n = 218) ----------------------------------------------------------------

dades %>% select (speaker.sex, total.questions) %>% group_by(speaker.sex) %>% summarize_all(mean) # questions: W = 1.62, M = 1.51

t.test(dades$total.questions ~ dades$speaker.sex) # p = 0.519, no dif for speakr.sex

## only for talks with questions (n = 177) ----------------------------------------------------------------

dades %>% select (speaker.sex, total.questions) %>% filter (total.questions > 0) %>%
          group_by(speaker.sex) %>% summarize_all(mean) # questions: W = 2.05, M = 1.93

t.test(total.questions ~ speaker.sex, subset(dades, total.questions >0)) # p = 0.491, no dif for speakr.sex


# Q: WAS THE AMOUNT OF QUESTIONS RAISED BY WOMEN PROPORTIONAL TO WOMEN ATTENDEES? ####

# stats: t.test  btw % attendees.women & questions.women 

dades = dades %>% mutate (propA.women = attendees.women/attendees)
dades = dades %>% mutate (propQ.women = questions.women/total.questions)

t.test(dades$propA.women,dades$propQ.women) # p < 0.001, differences.

# stats: liner regression model = how far is our data from the teoretical 1:1 line? (from Carter et al. 2016)

model = lm(propQ.women~propA.women, dades) 
summary(model) # slope = 1.15, intercept = -0.229, p < 0.001, at propA.female = 1, propQ.female = 0.92
slope.test(dades$propQ.women, dades$propA.women, test.value = 1, method = "OLS", alpha = 0.05, intercept = TRUE) # no dif from 1:1
predicted_df <- data.frame(propQ.pred = predict(model, dades), propA.women=dades$propA.women) # save predictions of the model 


# Q: WERE QUESTIONS RAISED BY WOMEN AND MEN EQUALLY LONG? # Crec que vam dir de treura-ho perquè era molt ambigu. Ho deixo per si de cas. ####

# stats: descriptora + t.test btw lenght questions women & men

lenght.women = c( 20.8336,  20.60838, 21.94203, 23.425, 11.75) # manual calculation from data_2B
lenght.men = c( 21.61869,  25.5887, 19.89856, 38, 33.88889, 58.25, 48.66667) # manual calculation from data_2B 

mean (lenght.women) # duration women questions = 20.8336 sec
mean (lenght.men) # # duration women questions = 21.61869 sec

t.test(lenght.women, lenght.men) # p = 0.03, diff.

# Q: WAS THE Q&A BEHAVIOUR INFLUENCED BY THE GENDER OF SPEAKERS OR CHAIRS? ####

# stats: t-test btw questions.women & questions.men (corrected by attendants).

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

# Q: WAS THE Q&A BEHAVIOUR OF CHAIRS INFLUENCED BY THE SPEAKERS GENDER? ####

# stats: Descriptors + chi-squared 
# chair.question (no / yes.first / yes.last / yes.other / yes.unique) -> TRANSFORMED TO YES / NO

dades.chairs = dades_all %>% select (talk.id, chair.sex, speaker.sex, chair.question)
dades.chairs = na.omit(dades.chairs)
count(dades.chairs %>% filter (chair.question != "no")) # 62 talks with chair questions
count(dades.chairs %>% filter (chair.question != "no"))/count(dades.chairs) # 30.0% talks with chair questions

dades.speaker.women = dades.chairs %>% filter (speaker.sex == "W") %>%
                                       select (talk.id, speaker.sex, chair.sex, chair.question) %>%
                                       mutate (chair.question.YN = ifelse(chair.question == "no", "no", "yes"))
                  
dades.speaker.men = dades.chairs %>% filter (speaker.sex == "M") %>%
                                     select (talk.id, speaker.sex, chair.sex, chair.question) %>%
                                     mutate (chair.question.YN = ifelse(chair.question == "no", "no", "yes"))

chisq.test(dades.speaker.women$chair.question.YN, dades.speaker.women$chair.sex) # p = 0.004, dif when speaker = F
chisq.test(dades.speaker.men$chair.question.YN, dades.speaker.men$chair.sex) # p = 0.274, no dif when speaker = M

#------------------------------------------------------------------------------------
# # FIGURES
#------------------------------------------------------------------------------------

# PREPARATION ####

#Set theme we will use for all plots

theme_gender<-  theme_minimal()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=15, margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(size=15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")

theme_set(theme_gender)

# Define  colours, ploygons position & arrows

colours <- c("#6E30AD", "#FABD6C")#c("#9419FF","#F1FF00")
tmp <- data.frame(x=c(0,0,0.9), y=c(0.1,1,1))
tmp2 <- data.frame(x=c(0.1,1,1), y=c(0,0,0.9))
a <- data.frame(x=c(0.1,0.9), y=c(0.1,0.9), xend=c(0.1,0.9), yend=c(0.4,0.6))


# FIGURE 4 ####
# The figure shows the proportion of questions asked by Women plotted against the proportion of Women attendees. 
# The line shows the theoretical proportional relationship between the two parameters.
# Most data points fall below the line = in proportion, men asked more questions than women.

# Data preparation

setwd(DataPath)
dades_all = read.csv(file="data_2B.csv", header=TRUE, sep=";")
dades = dades_all %>% select (talk.id, talk.type, session, speaker.sex, chair.sex, attendees, attendees.women, attendees.men, total.questions, questions.women, questions.men)

dades = dades %>% mutate (propA.women = attendees.women/attendees)
dades = dades %>% mutate (propQ.women = questions.women/total.questions)

model = lm(propQ.women~propA.women, dades) 
predicted_df = data.frame(propQ.pred = predict(model, dades), propA.women=dades$propA.women) # save predictions of the model 


# Plot
(g4 <- ggplot(dades, aes(x=propA.women, y=propQ.women)) + 
    geom_polygon(data = tmp, aes(x, y), alpha=0.2, fill=colours[1]) + #Add polygon
    geom_polygon(data = tmp2, aes(x, y), alpha=0.2, fill=colours[2])+ #Add polygon
    geom_point(size = 5, color="grey50", alpha=.75) +  #second define the points
    geom_segment(data=a, mapping=aes(x=x, y=y, xend=xend, yend=yend), arrow=arrow(), size=2, color=colours) + #Add the arrows
    annotate("text", x = a$xend[1], y = a$yend[1]+0.05, label = "Women bias", size=6, color=colours[1]) + #Add the text indicating the bias
    annotate("text", x = a$xend[2], y = a$yend[2]-0.05, label = "Men bias", size=6, color=colours[2]) + #Add the text indicating the bias
    geom_abline(intercept = 0, slope = 1, color="black", size=1.2) + #Add diagonal theoretical line
    labs(x="Proportion of women attendees", y = "Proportion of questions asked \nby women") + #Define the axis label
    lims(x=c(0,1), y=c(0,1))) +
  geom_line(color=colours[2], size=2, data = predicted_df, aes(x=propA.women, y=propQ.pred)) # Add line from  linear regression

#ggsave(g4, filename="Figure 4.pdf", path=ResultPath, width = 8, height = 6) #Save it 

# FIGURE 5 ####
## Panel A: Gender speaker  ----------------------------------------------------------------------------------------------------------
# Boxplot showing the number of questions per talk (standardized by the attendees' proportion)
# made by women and men considering the gender of speakers. 

# Data preparation

setwd(DataPath)
dades_all = read.csv(file="data_2B.csv", header=TRUE, sep=";")

dades.women.questions = dades_all %>% mutate (question.sex = "W") %>%
                                      mutate (q.total.corr = questions.women/attendees.women) %>%
                                      select (talk.id, speaker.sex, question.sex, q.total.corr) 

dades.men.questions = dades_all %>% mutate (question.sex = "M") %>%
                                    mutate (q.total.corr = questions.men/attendees.men) %>%
                                    select (talk.id, speaker.sex, question.sex, q.total.corr) 

dades = full_join (dades.women.questions, dades.men.questions)

dades = dades %>%  mutate (speaker.sex=fct_recode(speaker.sex, Women="W", Men="M"),
                           question.sex=fct_recode(question.sex, Women="W", Men="M"))
data.plot = subset(dades,speaker.sex=="Women" | speaker.sex=="Men")

# Plot
(g5a = ggplot(data.plot, aes(x=speaker.sex, y=q.total.corr)) +
       geom_boxplot(aes(fill = question.sex)) + 
       scale_fill_manual(name="", values = colours) +
       xlab("Gender of the speaker") + 
       ylab("Number of questions")  +
       theme_minimal()+
       theme_gender) 

# Now we add the significance into the plot
t.test.res = data.frame(sign = 1:2, speaker.sex = 1:2, y = 1:2)
t.test.res = t.test.res %>% mutate (sign = c("ns", "**")) %>% 
                            mutate (speaker.sex = c("Women", "Men")) %>%
                            mutate (y = max(data.plot$q.total.corr, na.rm = T) - 0.1)
(panel_A <- g5a + 
    geom_text(data = t.test.res, aes(x=speaker.sex, y=y, label = sign), size=6))

## Panel B: Gender chair -------------------------------------------------------------------------------------------------------
# Boxplot showing the number of questions per talk (standardized by the attendees' proportion)
# made by women and men considering the gender of chairs. 

# Data preparation

setwd(DataPath)
dades_all = read.csv(file="data_2B.csv", header=TRUE, sep=";")

dades.women.questions = dades_all %>% mutate (question.sex = "W") %>%
                                      mutate (q.total.corr = questions.women/attendees.women) %>%
                                      select (talk.id, chair.sex, question.sex, q.total.corr) 

dades.men.questions = dades_all %>% mutate (question.sex = "M") %>%
                                    mutate (q.total.corr = questions.men/attendees.men) %>%
                                    select (talk.id, chair.sex, question.sex, q.total.corr) 

dades = full_join (dades.women.questions, dades.men.questions)

dades = dades %>%  mutate (chair.sex=fct_recode(chair.sex, Women="W", Men="M"),
                           question.sex=fct_recode(question.sex, Women="W", Men="M"))
data.plot = subset(dades,chair.sex=="Women" | chair.sex=="Men")

#Plot
(g5b <- ggplot(data.plot, aes(x=chair.sex, y=q.total.corr))+
    geom_boxplot(aes(fill = question.sex)) + 
    scale_fill_manual(name="", values = colours) + 
    xlab("Gender of the chair") + 
    ylab("Number of questions") + #\n(corrected by attendees' proportion)
    theme_minimal()+
    theme_gender) 

#We add the results of the t-tests
t.test.res = data.frame(sign = 1:2, chair.sex = 1:2, y = 1:2)
t.test.res = t.test.res %>% mutate (sign = c("ns", "**")) %>% 
                            mutate (chair.sex = c("Women", "Men")) %>%
                            mutate (y = max(data.plot$q.total.corr, na.rm = T) - 0.1)
(panel_B <- g5b + geom_text(data = t.test.res, aes(x=chair.sex, y=y, label = sign), size=6))

## Panel C: Gender speaker and chair asking ------------------------------------------------------------
# Plot showing the number of questions raised by women (purple) and men (yellow) chairs considering the gender of speakers. 

#Prepare data

setwd(DataPath)
dades_all = read.csv(file="data_2B.csv", header=TRUE, sep=";")


dades.speaker.women = dades_all %>% filter (speaker.sex == "W") %>%
                                    mutate (chair.question.YN = ifelse(chair.question == "no", "no", "yes")) %>%
                                    select (talk.id, speaker.sex, chair.sex, chair.question.YN)

dades.speaker.men = dades_all %>% filter (speaker.sex == "M") %>%
                                  mutate (chair.question.YN = ifelse(chair.question == "no", "no", "yes")) %>%
                                  select (talk.id, speaker.sex, chair.sex, chair.question.YN)

data.plot = full_join(dades.speaker.women, dades.speaker.men)

data.plot = data.plot %>% mutate(speaker.sex=fct_recode(speaker.sex, Women="W", Men="M")) #change speaker sex
data.plot = na.omit(data.plot)
data.plot <- data.plot %>%  filter(chair.question.YN =="yes") %>% 
                            group_by(speaker.sex, chair.sex) %>%
                            tally() %>%
                            mutate(prop = prop.table(n)*100) # proportion of questions corrected by number of chairs W and M.

# Plot
(g5c<- ggplot(data.plot, aes(x=speaker.sex, y=prop)) +
    geom_bar(stat="identity", aes(fill = chair.sex), colour="black", position="dodge")+  
    xlab("Gender of the speaker") + ylab("Proportion of questions \nraised by the chair") +
    scale_fill_manual(name="Gender of the questioner", values = colours, labels = c("Women", "Men")) +
    #Start axis at 0
    scale_y_continuous(expand = c(0, 0), limits = c(0,100)))

# Add the significance into the plot
test.res = data.frame(sign = 1:2, speaker.sex = 1:2, y = 1:2)
test.res = test.res %>% mutate (sign = c("**", "ns")) %>% 
           mutate (speaker.sex = c("Women", "Men")) %>%
           mutate (y = max(data.plot$prop, na.rm = T) - 0.1)
(panel_C <- g5c + geom_text(data = test.res, aes(x=speaker.sex, y=y, label = sign), size=6))

## Final Figure ----------------------------------------------------------------------------------

#Get the legend from the plot
legend <- get_legend(g5c+theme(legend.position = "bottom",
                               legend.title = element_text(size=14),
                               legend.title.align=0.5,
                               legend.background = element_blank(),
                               legend.box.background = element_rect(colour = "black"),
                               legend.text=element_text(size=12))+
                       guides(fill = guide_legend(title.position = "top")))


(g5 <- plot_grid(panel_B, panel_A + ylab(""),  
                 panel_C, legend,
                 nrow=2, labels=c('A', 'B', 'C'), label_size = 12,
                 rel_widths = c(1, 1), align = "v"))

# Save it

ggsave(g5, 
       filename="Figure 5.pdf",
       path = ResultPath,
       width = 8, height = 7)
