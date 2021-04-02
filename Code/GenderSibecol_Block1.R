#------------------------------------------------------------------------------
# PROJECT: SIBECOL GENDER ANALYSIS / BLOCK 1
# DESCRIPTION: Code to analyze the contributions to the conference.
# Authors: SIBECOL Diversity and Inclusion Commission (diversity@sibecol.org)
#------------------------------------------------------------------------------

#Clean memory

rm(list = ls())

# Load packages 

library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)
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
library(hier.part)
library(car)

# Set working directories
path = gsub("/Code", "", dirname(rstudioapi::getActiveDocumentContext()$path))

DataPath = paste0(path,"/Data")
ResultPath = paste0(path, "/Results") 

#  SECTION 1: CONTRIBUTIONS AND PRESENTERS #################################### 

## Data file = Data_1B_AL.csv
## Figures = figure 1.
## Stats: Descriptors + chi-squared test of % women btw career stages + t-test for each career stage btw %W and %M

# Q: NUMBER OF CONTRIBUTIONS BY GENDER AND STAGE CAREER ####
# stats: Descriptors + chi-squared test of % women btw career stages + t-test for each career stage btw %W and %M

# Data Preparation

setwd(DataPath)
data_all = read.csv("Data_1B_AL.csv")

data = data_all %>% 
  dplyr::select(talk.id, talk.type, presenter.stage, presenter.sex) %>%
  mutate(presenter.stage = ordered(data$presenter.stage, c("pre-doctoral", "post-doctoral", "senior non-permanent","senior permanent", "other")))

## All contributions ----------------------------------------------

data %>% group_by(presenter.sex) %>%  
          summarise(n = n()) %>% mutate(freq = n / sum(n)) # 285 (50%) by women

data %>% group_by(presenter.stage) %>%
          summarise(n = n()) %>% mutate(freq = n / sum(n)) # % by career stage

## Posters  ---------------------------------------------------

## Data preparation

data_subset = data %>% filter(talk.type =="poster") %>% 
  filter (presenter.stage != "other") %>%                         
  mutate (gender = ifelse (presenter.sex == "W", 1, 0))

## Descriptors

data_subset %>% group_by(presenter.sex) %>%  
                 summarise(n = n()) %>%  mutate(freq = n / sum(n)) # 86 (53%) by women
data_subset  %>% group_by(presenter.stage, presenter.sex) %>% 
                  summarise(n = n()) %>% mutate(freq = n / sum(n)) # % W = 36-66% depending on the career stages

## Chi-squared for % W btw career stages

mod = glm(gender~presenter.stage, data_subset, family=binomial)  
Anova(mod, type=3) # p = 0.003, dif.

post_hoc = glht(mod, mcp(presenter.stage="Tukey")) # Post-hoc test
(post_hoc_res = summary(post_hoc)) # dif btw pre-doc and post-doc.

## Oral Talks ---------------------------------------------------

## Data preparation

data_subset = data %>% filter(talk.type =="oral") %>% 
  filter (presenter.stage != "other") %>%
  mutate (gender = ifelse (presenter.sex == "W", 1, 0))

## Descriptors

data_subset %>% group_by(presenter.sex) %>%
                 summarise(n = n()) %>%  mutate(freq = n / sum(n)) # 180 (49%) by women
data_subset  %>% group_by(presenter.stage, presenter.sex) %>% 
                  summarise(n = n()) %>% mutate(freq = n / sum(n)) # % W = 40-60% in all career stages


## Chi-squared for % W btw career stages

mod = glm(gender~presenter.stage, data_subset, family=binomial) 
Anova(mod, type=3) # p = 0.093, no dif.

## Invited Talks ---------------------------------------------------

## Data preparation

data_subset = data %>% filter(talk.type =="invited") %>% filter (presenter.stage != "other") %>%
                         mutate (gender = ifelse (presenter.sex == "W", 1, 0))

## Descriptors

data_subset  %>% group_by(presenter.sex) %>%
                  summarise(n = n()) %>%  mutate(freq = n / sum(n)) # 7 (33%) by women
data_subset  %>% group_by(presenter.stage) %>%
  summarise(n = n()) %>%  mutate(freq = n / sum(n)) 

data_subset  %>% group_by(presenter.stage, presenter.sex) %>% 
                  summarise(n = n()) %>% mutate(freq = n / sum(n)) # 5 non-permanent (2 women) // % women 30-50% depending on the career stage.

# Chi-squared for % W btw career stages

mod = glm(gender~presenter.stage, data_subset, family=binomial) 
Anova(mod, type=3) # p = 0.87, no dif.

## Plenary Talks ---------------------------------------------------

## Data preparation

data_subset = data %>% filter(talk.type =="plenary") %>% filter (presenter.stage != "other") %>%
  mutate (gender = ifelse (presenter.sex == "W", 1, 0))

## Descriptors

data_subset  %>% group_by(presenter.sex) %>%
                  summarise(n = n()) %>%  mutate(freq = n / sum(n)) # 4 (44%) by women
data_subset  %>% group_by(presenter.stage, presenter.sex) %>% 
                  summarise(n = n()) %>% mutate(freq = n / sum(n)) # 1 post-doc (1 woman)

# Chi-squared for % W btw career stages

mod = glm(gender~presenter.stage, data_subset, family=binomial) 
Anova(mod, type=3) # p = 0.40, no dif.


#  SECTION 2: COAUTHORS ###################################################### 
## Data file = Data_1B_AL.csv
## Figures = figure 2a.
## Stats: Descriptors + t-test (we treated sessions as replicates + only for invited, oral and poster contribution)

# Data Preparation ####

setwd(DataPath)
data_all = read.csv("Data_1B_AL.csv")

data = data_all %>% dplyr::select (talk.id, talk.type, session, presenter.sex, last.author.sex)

# Q: WHAT WAS THE GENDER DISTRIBUTION OF PRESENTERS (FIRST AUTHOR)?  ####

prop_first = data %>% 
  filter(talk.type%in%c("poster","oral","invited")) %>%
  mutate (gender = ifelse (presenter.sex == "W", 1, 0))

# Mean proportion of women as first authors

mean(prop_first$gender)

# Test the significance

t.test(prop_first$gender,mu = 0.5) # p = 0.93, no dif. W = 50%

# Q: WHAT WAS THE GENDER DISTRIBUTION OF PI (LAST AUTHOR)?  ####

prop_last = data %>% filter(talk.type%in%c("poster","oral","invited")) %>%
  mutate (gender = ifelse (last.author.sex == "W", 1, 0))

# Mean proportion of women as first authors

mean(prop_last$gender)

# Test the significance

t.test(prop_last$gender,mu = 0.5) # p <0.001, no dif. W = 50%

# Q: COMBINATION OF FIRST AND LAST AUTHORS (First/Last) ####
# Data preparation 

data = data %>% mutate (sex.comb = ifelse (presenter.sex == "W" & last.author.sex == "W", "WW", 
                                     ifelse (presenter.sex == "W" & last.author.sex == "M", "WM",
                                     ifelse (presenter.sex == "M" & last.author.sex == "W", "MW", "MM"))))

# Descriptors

data %>% group_by (sex.comb) %>% 
          summarise(n = n()) %>% mutate(freq = n / sum(n)) # MM = 40%, MW = 11%, WM = 32%, WW = 17%

# Stats: chi.-squared btw different combinations

proptot = data %>% mutate (sex.comb = as.factor (sex.comb)) %>% 
  count(sex.comb) %>% 
  mutate(prop = prop.table(n)*100)

mod = glm(prop~sex.comb, proptot,family = gaussian) # Chi-squared test 
Anova(mod, type=3) # p <0.01, diff

post_hoc = glht(mod, mcp(sex.comb="Tukey")) # Post-hoc test
(post_hoc = summary(post_hoc)) # diff btw MW-WM MW-MM, WW-MM

prop.test(proptot$n, n=rep(sum(proptot$n),4), p=c(0.25, 0.25, 0.25, 0.25))

#  SECTION 3: Gender biases among awards ########################################### 
## Data file = Data_1B_AL.csv
## Stats: Descriptors 

# Q: GENDER DISTRIBUTION AMONG AWARD CANDIDATES  ####

# Data preparation

setwd(DataPath)
data_all = read.csv("Data_1B_AL.csv")

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
## Data file = Data_1B_AL.csv
## Figures = figure 2b.
## Stats: Descriptors + linear regression models to know relation between % women chairs and % women presenters 

# Data Preparation 

setwd(DataPath)
data_all= read.csv("Data_1B_AL.csv") 

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

dat_subset = data %>% distinct(session, .keep_all = T) %>% 
  group_by(bias) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100) #15%W, 37% balanced, 48%M

mod = glm(freq~bias, dat_subset,family = gaussian) # Chi-squared test 
Anova(mod, type=3) # p <0.01, diff

post_hoc = glht(mod, mcp(bias="Tukey")) # Post-hoc test
(post_hoc = summary(post_hoc)) 

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
  mutate(prop.presenter.men = (presenter.men / sum(presenter.men))*100) %>% 
  filter (presenter.sex == "M") %>%
  dplyr::select (session, prop.presenter.men)

data_subset_2 = data %>%  mutate(prop.chair.men = 100- prop.chair.women) %>% 
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

# Q: RELATION BETWEEN % CHAIRWOMEN AND % INVITED SPEAKERS ####
## Stats: linear regression model

data_subset_1 = data %>% dplyr::select (talk.type, session) %>% 
  group_by(session, talk.type) %>% 
  summarise (presenter.invited = n()) %>%  
  mutate(prop.presenter.invited = (presenter.invited / sum(presenter.invited))*100) %>% 
  filter (talk.type == "invited") %>%
  dplyr::select (session, prop.presenter.invited)

data_subset_2 = data %>%  
  dplyr::select (session, prop.chair.women) %>% 
  group_by(session) %>%  
  summarise_all (mean)

data_subset = full_join (data_subset_1, data_subset_2)

mod = lm(data_subset$prop.presenter.invited ~data_subset$prop.chair.women)
(mod.res1 = summary(mod)) # 
(mod.res2 = Anova(mod, type=3)) # p = 0.849, no relation

# FIGURES -------------------------------------------------------------------

# Theme that we will use for the plots 

theme_set(theme_minimal()+
  theme(axis.title.x = element_text(size=15, margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(size=15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color="black", size = 12),
        axis.text.y = element_text(color="black", size = 12),
        strip.text.x = element_text(size = 12),
        axis.ticks = element_line(color="black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),
        legend.position = "none"))

# FIGURE 1 ####
## Data preparation  -------------------------------------------------------------

setwd(DataPath)
data_all = read.csv("Data_1B_AL.csv")

data = data_all %>% dplyr::select (talk.type, presenter.stage, presenter.sex) %>%
                      filter (presenter.stage != "other") %>%
                      mutate (gender = ifelse (presenter.sex == "W", 1, 0))
data = na.omit (data)
data$presenter.stage = ordered(data$presenter.stage,
                              c("pre-doctoral", "post-doctoral", "senior non-permanent","senior permanent"))

data_fig = data %>% group_by(talk.type, presenter.stage) %>%
                      count(presenter.sex,.drop = FALSE) %>%
                      mutate(freq = (n / sum(n))*100) %>% 
                      filter(talk.type!="Plenary", 
                             presenter.stage !="other",
                             !is.na(presenter.sex))
  
# Define the colours for Women and Men (respectively)
colours = c( "#FABD6C", "#6E30AD")

## PANEL A  -------------------------------------------------------------
(g1.a = data_fig %>% filter(talk.type=="poster") %>%filter(presenter.stage !="other") %>% 
                       ggplot(aes(x=presenter.stage, y=freq,
                       colour=presenter.sex, 
                       group=presenter.sex)) + 
    geom_line(alpha=0.5)+ #Add points and lines
    geom_point(size=5)+ #Add points and lines       
    scale_color_manual("", values = colours)+ #colours
        labs(x="", y = "Proportion (%)") + #Define the axis label
        scale_y_continuous(expand = c(0, 0), limits=c(0,105))+ #Start axis at 0
        theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),#Define the format of the plot
          axis.text.y = element_text(color="black"),
          strip.text.x = element_text(size = 12),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.spacing.x = unit(0,"line"),
          panel.border = element_rect(fill=NA),
          legend.position = "none"))

## PANEL B  -------------------------------------------------------------
(g1.b = data_fig %>% filter(talk.type=="oral") %>% filter(presenter.stage !="other") %>% 
    ggplot(aes(x=presenter.stage, y=freq,
               colour=presenter.sex, 
               group=presenter.sex)) + 
    geom_line(alpha=0.5)+ #Add points and lines
    geom_point(size=5)+ #Add points and lines       
    scale_color_manual("", values = colours)+ #colours
    labs(x="", y = "Proportion (%)") + #Define the axis label
    scale_y_continuous(expand = c(0, 0), limits=c(0,105))+ #Start axis at 0
    theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),#Define the format of the plot
          axis.text.y = element_text(color="black"),
          strip.text.x = element_text(size = 12),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.spacing.x = unit(0,"line"),
          panel.border = element_rect(fill=NA),
          legend.position = "none"))

## PANEL C  -------------------------------------------------------------
(g1.c = data_fig %>% filter(talk.type=="invited") %>% 
        ggplot(aes(x=presenter.stage, y=freq,
               colour=presenter.sex, 
               group=presenter.sex)) + 
    #Add points and lines
    geom_line(alpha=0.5)+
    geom_point(size=5)+        
    #colours
    scale_color_manual("", values = colours)+
    #Define the levels in x 
    #scale_x_discrete(labels=c("Pre-doctoral", "Early career", "Senior non-permanent", "Senior permanent"))+
    #Start axis at 0
    scale_y_continuous(expand = c(0, 0), limits = c(0, 105))+
    #Define the axis label
    labs(x="", 
         y = "Proportion (%)") + 
    #Define the format of the plot
    theme(axis.text.x = element_text(color="black",angle = 45, hjust = 1),
          axis.text.y = element_text(color="black"),
          strip.text.x = element_text(size = 12),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          panel.spacing.x = unit(0,"line"),
          panel.border = element_rect(fill=NA),
          legend.position = "right"))

## FINAL PLOT  -------------------------------------------------------------
# extract the legend from one of the plots
legend = get_legend(# create some space at the bottom and remove some at the top
  g1.c + theme(legend.box.margin = margin(-25, 0, 95, -12),
               legend.position = "right"))

# Arrange the plots 
(grid = plot_grid(g1.a, 
                   g1.b+ylab(""), 
                   g1.c+ylab("") +theme(legend.position = "none"), 
                       nrow = 1,   
                      labels = "AUTO", 
                      label_size = 12))

# Combine plots with legend
(Figure_1 = plot_grid(grid, legend, rel_widths =  c(4, .4)))

#Save it 

ggsave(Figure_1, 
       filename="Figure 1.pdf",
       path = ResultPath,
       width = 12, height = 4)



# FIGURE 2 ####
## PANEL A ------------------------------------------------------------------------
# Data Preparation

setwd(DataPath)
data_all = read.csv("Data_1B_AL.csv")

data = data_all %>% dplyr::select (talk.type, session, presenter.sex, last.author.sex) %>%
                      filter (talk.type%in%c("oral","poster")) %>%
                      mutate (sex.comb = ifelse (presenter.sex == "W" & last.author.sex == "W", "Woman x Woman", 
                                         ifelse (presenter.sex == "W" & last.author.sex == "M", "Woman x Man",
                                         ifelse (presenter.sex == "M" & last.author.sex == "W", "Man x Woman", "Man x Man"))))
data_fig_2 = data %>% count(sex.comb) %>%
                         mutate(prop = prop.table(n)*100)

data_fig_2 = data_fig_2 %>%  mutate(sex.comb = factor(data_fig_2$sex.comb, #Sort the factor according to the proportion
                                levels=unique(data_fig_2$sex.comb[order(proptot$n)]), 
                                ordered=TRUE))

colours_2 = c("#6E30AD", "#5C00B8","#FABF79", "#FA9C38") #Create a colour gradient from 

#Plot it

(g2.a = ggplot(data_fig_2, aes(x=sex.comb, 
                                 y=prop, fill=sex.comb)) +
        geom_bar(stat = "identity", colour="black", alpha=0.65) + #Add the bars
        scale_fill_manual("",values = colours_2)+ #Modify the colour scale
        labs(x="Gender combination", y= "Proportion (%)")+  #Axis labels
        scale_y_continuous(expand = c(0, 0), limits=c(0,60))+  #Start axis at 0
        theme(legend.position="none",
          axis.text.x = element_text(color="black"),
          axis.text.y = element_text(color="black"),
          strip.text.x = element_text(size = 12),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5)))

## PANEL B ------------------------------------------------------------------------------------------------------------------

## Data preparation

setwd(DataPath)
data_all= read.csv("Data_1B_AL.csv") 

data_fig = data_all %>% dplyr::select (session, prop.chair.women) %>% 
  mutate (bias = ifelse (prop.chair.women > 60, 
                         "Women > Men",    # "Bias against women"
                         ifelse (prop.chair.women >= 40 & prop.chair.women <= 60, 
                                 "Women = Men", "Women < Men"))) %>%    #"Balanced", "Bias against men"
  distinct(session, .keep_all = T) %>%
  filter (session != "PT") %>% 
  group_by(bias) %>% 
  summarise(n = n()) %>% 
  mutate(freq = (n / sum(n))*100)
data_fig$bias = ordered(data_fig$bias, c("Women < Men", "Women = Men", "Women > Men"))   # c("Bias against men", "Balanced", "Bias against women")

# Colours 

colours_3 = c(colours[1], "grey85", colours[2])

# The plot 

(g2.b = ggplot(data_fig, 
                aes(x=bias, y=freq, fill=bias)) +
    geom_bar(stat = "identity", colour="black") +  #Add the bars
    geom_text(aes( label = paste(round(freq,0), "%", sep=""), y= freq), vjust = -.5) + #Add the percentage
    scale_fill_manual("Bias",values = colours_3)+ #Modify the colour scale
    labs(x="Gender bias in chairs", #Axis labels
         y= "Proportion of chairs (%)")+ 
    scale_y_continuous(expand = c(0, 0), limits=c(0,60)))  #Start axis at 0

## FINAL PLOT -------------------------------------------------------------

# Arrange the plots 
(Figure_2 = plot_grid(g2.a, g2.b, nrow = 1, labels = "AUTO", label_size = 12))

# Save it
ggsave(Figure_2,  filename="Figure 2.pdf", path = ResultPath, width = 12.5, height = 6)

