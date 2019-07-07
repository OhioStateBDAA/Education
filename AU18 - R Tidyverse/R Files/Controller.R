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
options(stringsAsFactors = F)



############################################################################################################################################################################################
#Data Acquisition
############################################################################################################################################################################################
#Stores the name of working directory
wd = getwd()

#Concatenates wd and file path. done to ensure repeatability
filename = paste0(wd, "/Tidyverse Workshop/Data/Franklin County Crashes 2015-2017.csv")

#reads in Franklin County Crash Data set.
crashes = read_csv(filename)


save(crashes, file = paste0(wd, "/Data/crashes.Rdata"))

###############################################################################################################################################################################################################################################################################
#Data Cleansing
###############################################################################################################################################################################################################################################################################
#Adds column to decide whether or not crashes took place at night
crashes = crashes %>%
  mutate(CRASH_DT = as.Date(CRASH_DT, '%m/%d/%Y'))%>%
  arrange(CRASH_DT)%>%
  mutate(Time = if_else(TIME_OF_CRASH >= 2100 | TIME_OF_CRASH <= 500, "Night", "Day"))

#filters out missing values in Route type column  
crashes = crashes%>%
  filter(NLF_ROUTE_TYPE_CD != "")

#Creates column for month and year
#This column will allow for grouping by month
crashes = crashes%>%
  mutate(Month_Year = paste0(CRASH_YR, "-", MONTH_OF_CRASH))%>%
  mutate(Month_Year = as.yearmon(Month_Year))

#Initially the data set holds zeros and ones for weekday
#This transforms that into its proper days and orders them as they occur
crashes = crashes %>%
  mutate(DAY_IN_WEEK_CD = recode(DAY_IN_WEEK_CD, `1` = "Sunday", `2` = "Monday", `3` = "Tuesday", `4` = "Wednesday", `5` = "Thursday", `6` = "Friday", `7` = "Saturday"))%>%
  mutate(DAY_IN_WEEK_CD = factor(DAY_IN_WEEK_CD, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
###############################################################################################################################################################################################################################################################################
#Data Manipulation and Wrangling
###############################################################################################################################################################################################################################################################################
#Counts the number of crashes by weekday
weekday = crashes %>%
  count(DAY_IN_WEEK_CD) %>%
  rename(Day = DAY_IN_WEEK_CD)

#Counts the number of accidents by city
city = crashes %>%
  count(ODPS_CITY_VILLAGE_TWP_NME)%>%
  rename(City = ODPS_CITY_VILLAGE_TWP_NME)

#calculate severity of car accidents by weekday
weekday.severity = crashes %>%
  group_by(DAY_IN_WEEK_CD)%>%
  summarise(Severity = round(mean(SEVERITY_BY_TYPE_CD), 2))%>%
  rename(Day = DAY_IN_WEEK_CD)%>%
  mutate(Severity = factor(Severity))

#calculate severity of car accidents by month
month.severity = crashes %>%
  group_by(Month_Year)%>%
  summarise(Severity = round(mean(SEVERITY_BY_TYPE_CD), 2))%>%
  rename(Month = Month_Year)%>%
  mutate(Severity = factor(Severity))

#counts the number of car crashes per month
month.count = crashes %>%
  count(Month_Year)
############################################################################################################################################################################################
#Data Visualization
############################################################################################################################################################################################
#Crashes per month
#This visulaization uses a barplot with no stat = identity, grey theme, 

crashes %>%
  ggplot()+
  geom_bar(aes(x = Month_Year, fill = Month_Year))+
  theme_bw()+
  scale_x_yearmon(format = "%b %Y", n = 4)



#Visualization of crashes by weekday.
#This visualization utilizes the barplot, black and white theme, and paired rcolor
weekday %>%
ggplot()+
  geom_bar(aes(x = Day, y = n, fill = Day), stat = "identity")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  labs(y = "Count of Crashes", title = "Count of Crashes by Weekday")+
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = .5, size = 15))


#severity of crahes by weekday
weekday.severity %>%
  ggplot()+
  geom_bar(aes(x = Day, y = Severity, fill = Day), stat = "identity")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  labs(y = "Severity of Crashes", title = "Severity of Crashes by Weekday")+
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = .5, size = 15))



#Jitter plot of crash severity by number of cars involved
#This visualization utilizes the jitter plot, black and white theme
crashes %>%
  ggplot()+
  geom_jitter(aes(x = NUMBER_OF_UNITS_NBR, y = SEVERITY_BY_TYPE_CD))+
  theme_bw()+
  labs(x = "Number of Cars", y = "Severity of Accident", title = "Severity of Accident by Number of Cars")+
  theme(plot.title = element_text(hjust = .5, size = 15))
  

  
  
  
  
  
  
