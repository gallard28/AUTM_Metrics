#AUTM Metrics Data and Analysis ####
#Author: Grant A. Allard 
#Purpose: Get to know the STATT Database 


#Libraries####
library(tidyverse)
library(ggplot)

#Data####
file<-"/Users/GrantAllard/Documents/Data Science/AUTM_Metrics/AUTM STATT Data/AUTM_1991_2017_AllVariables.csv"
data<-read.csv(file)
data_df<-data

#Clean Data####
data_df %>% 
  filter(is.na(X.ID.)) %>% 
  View

#Remove Totals: 1) Create df of totals, 2) remove from data_df
Totals<-data_df %>% 
  filter(INSTITUTION=="Totals")

data_df<-data_df %>% 
  filter(!INSTITUTION=="Totals")

#Remove Confidentials: 1) Create df of Confidentials, 2) remove from data_df
Confidentials<-data_df %>% 
  filter(INSTITUTION=="Confidentials")

data_df<-data_df %>% 
  filter(!INSTITUTION=="Confidentials")


#Quick overview of data####

names(data_df)
#By year - # of institutions in data
Year<-data_df %>% 
  group_by(YEAR) %>% 
  count() %>% 
  arrange(YEAR)

colnames(Year)<-c("Year", "Institutions")

YearPlot<-Year %>% 
  ggplot(aes(x=Year, y=Institutions))+
  geom_bar(stat="identity")

#By number of appearances
Appearances<-data_df %>% 
  group_by(X.ID.) %>% 
  count() %>% 
  arrange(desc(n))

#Histogram of Institutions by Number of Years in Data
Appearances %>% 
  ggplot(aes(x=n))+
  geom_histogram(binwidth=2)+
  geom_vline(xintercept=mean(Appearances$n), color="red")


#How many institutions have reported for 27 years?
Appearances %>% 
  filter(n==27) %>% 
  nrow()
#37 institutions reported all 27 years


#How many institutions have reported for 2 years or less?
Appearances %>% 
  filter(n<=2) %>% 
  nrow()
#47 institutions, which ones? 
LTE_Two<-Appearances %>% 
  filter(n<=2) 
LTE_Two$n<-NULL

data_df %>% 
  filter(X.ID. %in% LTE_Two$X.ID.) %>%
  View()

#Workshop
data_df %>% 
  filter(str_detect(INSTITUTION, "California")) %>% 
  View

#Geographic Exploration#### 



  