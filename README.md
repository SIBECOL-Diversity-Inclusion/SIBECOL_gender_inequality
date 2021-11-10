# SIBECOL_gender_inequality
This repository contains the code for the statistical analyses in "Towards women-inclusive Ecology: Representation, behavior, and perception of women at an international conference".

__Authors__: Anna Lupon<sup>1</sup>, Pablo Rodríguez-Lozano<sup>2,3</sup>, Mireia Bartrons<sup>4</sup>, Alba Anadon-Rosell<sup>5,6</sup>, Meritxell Batalla<sup>6</sup>, Susana Bernal<sup>1</sup>, Andrea G. Bravo<sup>7</sup>, Pol Capdevila<sup>8,9</sup>, Miguel Cañedo-Argüelles<sup>10</sup>, Núria Catalán<sup>11</sup>, Ana Genua-Olmedo<sup>12</sup>, Cayetano Gutiérrez-Cánovas<sup>13</sup>, Maria João Feio<sup>14</sup>, Federica Lucati<sup>1,15,16</sup>, Gabriela Onandia<sup>17,18</sup>, Sílvia Poblador<sup>19</sup>, Roser Rotchés-Ribalta<sup>6</sup>, Anna Sala-Bubaré<sup>20</sup>, María Mar Sánchez-Montoya<sup>21,22</sup>, Marta Sebastián<sup>7</sup>, Aitziber Zufiaurre<sup>6,23</sup>, Ada Pastor<sup>24</sup> 

<sup>1</sup> Integrative Freshwater Ecology Group, Centre d’Estudis Avançats de Blanes (CEAB-CSIC), Blanes, Spain

<sup>2</sup> Department of Geography, University of the Balearic Islands, Palma, Spain

<sup>3</sup> Department of Environmental Science, Policy, and Management, University of California at Berkeley, Berkeley, CA 94720, USA

<sup>4</sup> Aquatic Ecology Group, University of Vic—Central University of Catalonia (Uvic-UCC), Vic, Spain

<sup>5</sup> Landscape Ecology and Ecosystem Dynamics, Institute of Botany and Landscape Ecology, University of Greifswald, Greifswald, Germany

<sup>6</sup> CREAF, E08193 Bellaterra (Cerdanyola del Vallès), Catalonia, Spain

<sup>7</sup> Department of Marine Biology and Oceanography, Institut de Ciències del Mar (ICM-CSIC), Barcelona, Spain

<sup>8</sup> School of Biological Sciences, University of Bristol, Bristol, UK

<sup>9</sup> Department of Zoology, University of Oxford, Oxford OX1 3SZ, UK

<sup>10</sup> FEHM-Lab, Departament de Biologia Evolutiva, Ecologia i Ciències Ambientals, Institut de Recerca de l’Aigua (IdRA), Universitat de Barcelona, Barcelona, Spain

<sup>11</sup> Laboratoire des Sciences du Climat et de l’Environnement, LSCE, CNRS-UMR 8212, Gif Sur Yvette, France

<sup>12</sup> Centre for Environmental and Marine Studies (CESAM), Department of Biology, University of Aveiro, Aveiro, Portugal

<sup>13</sup> Doñana Biological Station (EBD-CSIC), Sevilla, Spain

<sup>14</sup> Department of Life Sciences, MARE-Marine and Environmental Sciences Centre, University of Coimbra, Coimbra, Portugal

<sup>15</sup> Centre for Ecology, Evolution and Environmental Changes (cE3c), University of Lisbon, Lisbon, Portugal 

<sup>16</sup> Department of Political and Social Sciences, Universitat Pompeu Fabra (UPF), Barcelona, Spain

<sup>17</sup> Research Platform Data Analysis and Simulation, Leibniz Centre for Agricultural Landscape Research (ZALF), Müncheberg, Germany

<sup>18</sup> Berlin-Brandenburg Institute of Advanced Biodiversity Research (BBIB), Berlin, Germany

<sup>19</sup> Plants and Ecosystems (PLECO), Biology Department, University of Antwerp, Wilkrijk, Belgium

<sup>20</sup> Faculty of Psychology, Education and Sports Sciences Blanquerna, Ramon Llull University, Barcelona, Spain

<sup>21</sup> Department of Ecology and Hydrology, International Excellence Campus for Higher Education and Research of the University of Murcia, Murcia, Spain

<sup>22</sup> Department of Biodiversity, Ecology, and Evolution, Universidad Complutense de Madrid, Madrid, Spain

<sup>23</sup> Área de Biodiversidad, Gestión Ambiental de Navarra-Nafarroako Ingurumen kudeaketa (GAN-NIK), Pamplona-Iruñea, Navarra.

<sup>24</sup> Department of Biology, Aarhus University, Aarhus, Denmark



#### Contact: diversity@sibecol.org

# Aim
_In this work, we explored women participation in ecology conferences by using the 1st Meeting of the Iberian Society of Ecology (SIBECOL, www.sibecol.org) as a testing ground. Specifically, we aimed to assess if women and men were equally represented, participative and visible, and if not, to what extent these differences were due to demographic inertia or to other non-recognized aspects. To answer these questions, we analyzed: (i) the participation of women in the attendance, speakers and organization panels, (ii) the audience behavior during Q&A times, and (iii) the perception of the attendees on women participation and visibility, gender barriers, and conference environment._

# Data
To answer the abovementioned questions we used three different datasets: 

- __`Data_Block1.csv`__: contains information of the contributions to the conference, such as type of contribution (e.g. oral, poster), presenters gender, presenters career stage. 
- __`Data_Block2.csv`__: contains information about each of the sessions, such as speaker and gender (see Appendix S1 in the main manuscript). 
- __`Data_Block3.txt`__: contains answers to the questionnaire about the perception of the attendants to the conference (see Appendix S2 in the main manuscript). 

_All the data used in this study is anonymous and respected the privacy of the participants at the conference._

# Code

To answer the abovementioned questions, we compiled and analized data in three main blocks using three independent R scripts: 

- __`Rcode_Block1.R`__: to analyze the contributions to the conference.
- __`Rcode_Block2.R`__: to analyze the attendance, gender bias in sessions and behavior during Q&A times.
- __`Rcode_Block3.R`__: to analyze the perception of the attendees on women participation and visibility, gender barriers, and conference environment.

_For a detailed explanation of the methods, please see the methods section of the main manuscript._

# Software
_R version 3.6.1 or greater_

To download `R`, go to https://www.r-project.org and for `RStudio`, go to https://www.rstudio.com/products/rstudio/download/ .

