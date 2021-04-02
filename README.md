# SIBECOL_gender_inequality
This repository contains the code for the statistical analyses in "Towards women-inclusive Ecology: Representation, behavior, and perception of women at an international conference".

Authors: Iberian Ecological Society Diversity and Inclusion Commission 

#### Contact: diversity@sibecol.org

# Aim
_In this work, we explored women participation in ecology conferences by using the 1st Meeting of the Iberian Society of Ecology (SIBECOL, www.sibecol.org) as a testing ground. Specifically, we aimed to assess if women and men were equally represented, participative and visible, and if not, to what extent these differences were due to demographic inertia or to other non-recognized aspects. To answer these questions, we analyzed: (i) the participation of women in the attendance, speakers and organization panels, (ii) the audience behavior during Q&A times, and (iii) the perception of the attendees on women participation and visibility, gender barriers, and conference environment._

# Data
To answer the abovementioned questions we used three different datasets: 

- __`Data_1B_AL.csv`__: contains information of the contributions to the conference, such as type of contribution (e.g. oral, poster), presenters gender, presenters career stage. 
- __`data_B2.csv`__: contains information about each of the sessions, such as speaker and gender (see Appendix S1 in the main manuscript). 
- __`Data_3B.txt`__: contains answers to the questionaire about the perception of the attendants to the conference (see Appendix S2 in the main manuscript). 

_All the data used in this study is anonymous and respected the privacy of the participants to the conference._

# Code

To answer the abovementioned questions, we compiled and analized data in three main blocks using three indpendent R scripts: 

- __`GenderSibecol_Block1.R`__: to analyze the contributions to the conference.
- __`GenderSibecol_Block2.R`__: to analyze the attendance, gender bias in sessions and behavior during Q&A times.
- __`GenderSibecol_Block3.R`__: to analyze the the perception of the attendees on women participation and visibility, gender barriers, and conference environment.

_For a detailed explanation of the methods, please see the methods section of the main manuscript._

# Software
_R version 3.6.1 or greater_

To download `R`, go to https://www.r-project.org and for `RStudio`, go to https://www.rstudio.com/products/rstudio/download/ .

