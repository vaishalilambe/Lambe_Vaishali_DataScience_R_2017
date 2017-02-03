########################################################################
#          Author: Vaishali Lambe NUID:001286444                       #
########################################################################

#see current working directory
getwd()
#set the working directory
setwd("C:/Users/Admin/Documents/DataScience/R-tutorial")

#save output to console file
sink("output.txt")

#load data from Classdata.csv to R
ClassData<-read.csv("ClassData.csv", header = TRUE)
#display data
ClassData

#########################################################################
#                1. Min, Max, Median, Avg for GPA                       #
#########################################################################

#First way
meanGPA<-mean(ClassData$GPA)
meanGPA  #display
minGPA<-min(ClassData$GPA)
minGPA   #display
maxGPA<-max(ClassData$GPA)
maxGPA   #display
medGPA=median(ClassData$GPA)
medGPA   #display

#Second way using dplyr
library(dplyr)
meanGPA<-ClassData$GPA %>% mean()
meanGPA  #display  
minGPA<-ClassData$GPA %>% min()
minGPA   #display
maxGPA<-ClassData$GPA %>% max()
maxGPA   #display
medGPA<-ClassData$GPA %>% median()
medGPA   #display

#########################################################################
#          1. Min, Max, Median, Avg for Years of Work Experience        #
#########################################################################

#change name of the column Years of Work Experience to YWEx
names(ClassData)[3]<-"YWEx"
ClassData  #display

#First way
meanYWEx<-mean(ClassData$YWEx)
meanYWEx  #display
minYWEx<-min(ClassData$YWEx)
minYWEx   #display
maxYWEx<-max(ClassData$YWEx)
maxYWEx   #display
medYWEx=median(ClassData$YWEx)
medYWEx   #display

#Second way using dplyr
meanYWEx<-ClassData$YWEx %>% mean()
meanYWEx  #display
minYWEx<-ClassData$YWEx %>% min()
minYWEx   #display
maxYWEx<-ClassData$YWEx %>% max()
maxYWEx   #display
medYWEx<-ClassData$YWEx %>% median()
medYWEx   #display

#########################################################################
#                      2. mode of a salary latest                       # 
#########################################################################

#chane name of column from latest salary per year to LSPY
names(ClassData)[4]<-"LSPY"
ClassData  #display

# Create the function getmode to calculate mode of latest salary per year
getmode <- function(LSPY) {
  uniqv <- unique(LSPY)
  uniqv[which.max(tabulate(match(LSPY, uniqv)))]
}

modeSalary<-getmode(ClassData$LSPY)
modeSalary   #display

#mode of a salary expected after graduation
#change name of colunm form salary expected after graduation to ESAG
names(ClassData)[9]<-"ESAG"
ClassData   #display

# Create the function to clculate mode of ESAG
getmodeExp <- function(ESAG) {
  uniqv <- unique(ESAG)
  uniqv[which.max(tabulate(match(ESAG, uniqv)))]
}

modeSalaryExp<-getmodeExp(ClassData$ESAG)
modeSalaryExp   #display

#########################################################################
#         3. co-op % of students done co op and didnt do co op          #
#########################################################################

#change name of the colunm from coop internship Y/N to Coop 
names(ClassData)[7]<-"Coop"
ClassData  #display

#convert all 'y' to 'Y'
ClassData <- mutate_each(ClassData, funs(toupper))
ClassData  #display
#create vector Coop
Coop <- c(ClassData$Coop)

#percentage of people dont have co op
Non_Coop<- (table(Coop)[1]/length(ClassData$Coop)) * 100
round(Non_Coop,2)

#percentage of people on co op
On_Coop<- (table(Coop)[2]/length(ClassData$Coop)) * 100
round(On_Coop,2)

#pie chart to display number of students on co op and not on co-op
Coop_counts<-table(Coop)
pie(Coop_counts)

#########################################################################
#     4. No of students with more than 500 LinkedIn contacts            #
#########################################################################
MoreContacts<-filter(ClassData,ClassData$Number.of.contacts.on.Linkedin>500)
MoreContacts
#number of people having more than 500 contacts
count(MoreContacts)

#########################################################################
#     5.Find the Inter Quartile Range for the Expected Salary Range     #
#########################################################################

#first way of IQR
IQR_ESAG<-IQR(ClassData$ESAG)
IQR_ESAG  # display 


