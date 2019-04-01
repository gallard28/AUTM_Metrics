#AUTM Metrics Data and Analysis ####
#Author: Grant A. Allard 
#Purpose: Get to know the STATT Database 


#Libraries####
library(tidyverse)
library(ggplot2)

#Data####
file<-"/Users/GrantAllard/Documents/Data Science/AUTM_Metrics/AUTM STATT Data/AUTM_1991_2017_AllVariables.csv"
data<-read.csv(file)

#create back up
data_df<-data

#Clean Data####
data_df %>% 
  filter(is.na(X.ID.)) 

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

#Make Variables Correct Data Types
str(data_df)

#Subset columns that need to become numeric
to_numeric<-data_df %>% 
  select(-YEAR, -X.ID., -INSTITUTION, -MEDSCHL, -STATE, -INSTTYPE) %>% 
  names()

#Factor columns to character
data_df[,to_numeric]<-lapply(data_df[,to_numeric],as.character)

#Remove Symbols
data_df[,to_numeric]<-data.frame(lapply(data_df[,to_numeric],function(x) {
  gsub("\\$", "", x)
}))
data_df[,to_numeric]<-lapply(data_df[,to_numeric],as.character)

#Convert to numeric
data_df[,to_numeric]<-lapply(data_df[,to_numeric],as.numeric)

#Convert State to Character
data_df$STATE<-as.character(data_df$STATE)

#Convert Institution to Character
data_df$INSTITUTION<-as.character(data_df$INSTITUTION)

names(data_df)

#Big Six
BigSix<-c("X.ID.","YEAR","INSTITUTION", "MEDSCHL", "STATE", "INSTTYPE", "Tot.Res.Exp","Tot.Discl.Lic","Gross.Lic.Inc", "INVDISRES","New.Pat.App.Fld","St.Ups.Formed")

#str(data_df[,to_numeric])

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

#Build Sample Shiny Graphics####
#Create df w/ Big Six Variables 
data_shiny_df<-data_df %>% 
  select(one_of(BigSix))

#Standardize names in dagta
Institution_df<-data_shiny_df %>% 
  filter(YEAR == 2017) %>% 
  select(X.ID., INSTITUTION)

#Disambiguate Names
data_shiny_df<-left_join(data_shiny_df, Institution_df, by="X.ID.")
data_shiny_df$INSTITUTION.x<-NULL
data_shiny_df$INSTITUTION<-data_shiny_df$INSTITUTION.y
data_shiny_df$INSTITUTION.y<-NULL

#Identify Dependent Variables
DepVar<-data_shiny_df %>% 
  select(-X.ID., -YEAR, -INSTITUTION, -MEDSCHL, -STATE, -INSTTYPE) %>% 
  names()

#Reduce Number of Institutions in Data
Sample<-c("Albert Einstein College of Medicine Inc.", "Arizona State Univ.", "Clemson Univ.","Georgia Inst. of Technology", "Massachusetts Inst. of Technology (MIT)", "Ohio State Univ.")

data_shiny_df<-data_shiny_df %>% 
  filter(INSTITUTION %in% Sample)

#Save Data####
save(data_shiny_df, file="/Users/GrantAllard/Documents/Data Science/AUTM_Metrics/AUTM_Metrics/AUTM_Data_Explorer2/data_shiny_df.Rdata")
#save(DepVar, file="DepVar.Rdata")


#Federal Spending and Licensing Revenue - Graph 
data_shiny_df %>% 
  filter(str_detect(INSTITUTION, "Clemson")) %>% 
  ggplot(aes(x=YEAR)) +
  geom_area(aes(y=Tot.Res.Exp, fill="#FF1493"), alpha=.5)+
  geom_area(aes(y=Gross.Lic.Inc, fill="#7FFFD4"), alpha=1)+
  scale_fill_discrete(name="Legend",
                      breaks=c("#FF1493", "#7FFFD4"),
                      labels=c("Total Research Expenditure", "Gross Licensing Income"))



  