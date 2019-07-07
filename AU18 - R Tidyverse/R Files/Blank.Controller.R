#Author:

#This file serves to gain anlytical insight from Franklin County Crash data in the year  2016. This data can be foun at: 
#If there are any questions regarding the content of this analysis email: braggs.4@osu.edu

############################################################################################################################################################################################
#Install/Load Packages
############################################################################################################################################################################################
#install.packages("tidyverse")
#install.packages("plotly")
#install.packages("zoo")
#install.packages("RColorBrewer")
library(zoo)
library(plotly)
library(tidyverse)
library(RColorBrewer)



############################################################################################################################################################################################
#Data Acquisition
############################################################################################################################################################################################
#Stores the name of working directory
wd = getwd()

#Concatenates wd and file path. done to ensure repeatability
filename = paste0(wd, "/Data/Franklin County Crashes 2015-2017.csv")

#reads in Franklin County Crash Data set.
crashes = read_csv(filename)

###############################################################################################################################################################################################################################################################################
#Data Cleansing
###############################################################################################################################################################################################################################################################################
#Adds column to decide whether or not crashes took place at night

#filters out missing values in Route type column  


#Creates column for month and year
#This column will allow for grouping by month


#Initially the data set holds zeros and ones for weekday
#This transforms that into its proper days and orders them as they occur

###############################################################################################################################################################################################################################################################################
#Data Manipulation and Wrangling
###############################################################################################################################################################################################################################################################################
#Counts the number of crashes by weekday


#Counts the number of accidents by city


#calculate severity of car accidents by weekday


#calculate severity of car accidents by month


#counts the number of crashes per month

############################################################################################################################################################################################
#Data Visualization
############################################################################################################################################################################################
#Crashes per month
#This visulaization uses a barplot with no stat = identity, grey theme, 




#Visualization of crashes by weekday.
#This visualization utilizes the barplot, black and white theme, and paired rcolor



#severity of crahes by weekday
#This visualization utilizes the barplot, black and white theme, and paired rcolor



#Jitter plot of crash severity by number of cars involved
#This visualization utilizes the jitter plot, black and white theme




