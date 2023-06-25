#1) Installing and loading libararies
#Installing the following packages using tools menu: tidyverse, openxlsx, plotly, and zoo.
#Call the required libraries for the assignment.

library(tidyverse)
library(openxlsx)
library(plotly)
library(zoo)


#2) Reading data and assigning its data frame
#Using read.xlsx to read data from xlsx file saved in local hard drive.
survey <- read.xlsx("C:/Users/kylie/Documents/R/Practicum/survey.xlsx")


#3) Get rid of non-informative columns
#The purpose of the survey is to measure attitudes towards mental health and frequency of mental health disorders in the tech workplace.
#Select only informative columns for the research topic using select function, and by "-" the not helpful column for the insight.
survey2 <- select(survey, -state, -self_employed, -phys_health_consequence, -coworkers, -supervisor, -phys_health_interview, -mental_vs_physical, -obs_consequence, -comments)


#4) Assign better column name
#Renaming columns using "rename" function to make them easier to understand.
survey2 <- rename(survey2, Date = Timestamp, quitting = leave)


#5) Get rid of blank rows

#Count the number of missing values per column
colSums(is.na(survey2))

#Create a subset to exclude missing values in all columns
survey3 <- survey2[complete.cases(survey2), ]

#Re-count the number of missing values per column to make sure there is no blank values
colSums(is.na(survey3))


#6) Convert the Date field to the correct datatype
#Correct the date column to the right format.
survey3$Date <- as.Date (survey3$Date, origin = "1899-12-30")

#Plot to detect the bad date rows.
ggplot(survey3, aes(x = Date, y = Age)) + geom_point()

#It appears there are rows dated in 1900.
#The workbook contains of two worksheets for for two different survey periods
#Sheet 2 contain data that collected in 2014. #Sheet 1 contain data that collected in 2015 and 2016.
#Remove bad date rows by using the subset function with start date as of 2014-01-01
removebad_dates <- subset(survey3, Date >='2014-01-01')

#Re-plot to make sure all the bad date rows are removed.
ggplot(removebad_dates, aes(x = Date, y = Age)) + geom_point()


#7) Clean the Age column
#The above plot chart also shows Age columns contains scientific notation and numbers that do not make sense such as 0, 2, or 9999.

#Modify global options in R to turn off scientific notation and convert them to integer.
options(scipen = 999)

#The working age range should be between 18 & 68. The following code filters values that do not make sense.
filer_age <- removebad_dates[removebad_dates$Age > 18 & removebad_dates$Age < 68,]

#Re-plot to make sure the values in Age column is within acceptable range.
ggplot(filer_age, aes(x = Age, y = Date)) + geom_point()


#8) Clean the Gender column
#There words or term to describe the same gender. They can be misspelled or synonyms.
#The following steps are to group all related variables to main groups.
#Note: The code line in 2nd step doesn't recognize some characters. 
#Therefore, I removed the () and other characters in Excel file for the following values: Guy ^_^, Female (cis), and Female (trans).

#1st: A frequency table shows the number of occurrences of each category of a variable.
table(filer_age$Gender)

#2nd: Remove values that do not make sense
Gender1 = filter(filer_age, !(Gender %in% c("2", "Nah", "ostensibly male, unsure what that really means")))

#3rd: store all related values in the following vector.

Female = c("cis-female/femme", "Cis Female", "f", "F", "femail", "female", "Female ", "woman", "Woman", "Female")
Male = c("cis male", "Cis Male", "Cis Man", "Guy", "m", "M", "Mail", "Make", "Mal", "male", "Male", "Male-ish", "Male ", "Malr", "msle")
Female_trans = c("Female trans", "Trans-FeMale", "Trans FeMale")
Queer = c("queer", "queer/she/they", "Gender queer")

#4th: replace all variables in the vectors to the main categories using the "mutate" function.
Gender2 <- Gender1 %>% 
  mutate(Gender = str_replace_all(Gender, paste(Female, collapse = "|", "$", sep = ""), "Female"))

Gender3 <- Gender2 %>% 
  mutate(Gender = str_replace_all(Gender, paste(Male, collapse = "|", "$", sep = ""), "Male"))

Gender4 <- Gender3 %>% 
  mutate(Gender = str_replace_all(Gender, paste(Female_trans, collapse = "|", "$", sep = ""), "Female-trans"))

Gender5 <- Gender4 %>% 
  mutate(Gender = str_replace_all(Gender, paste(Queer, collapse = "|", "$", sep = ""), "Queer"))

#5th: Check to make sure the 4 above categories are grouped properly.
table(Gender5$Gender)


#9) Clean the no_employees column
#The no_employees column contains date values, which need to remove.
#The column should only contain the the range of number of employees in the company that respondents work in.
#Using"filter" feature to only include these values: 26-100, 100-500, 500-1000, and More than 1000

survey4 <- filter(Gender5, no_employees=="26-100" | no_employees=="100-500" | no_employees=="500-1000" | no_employees=="More than 1000")


#10) Cleaning remaining columns

#List datatype of each data.
str(survey4)

#A frequency table shows the number of occurrences of each category of a variable.
#Use it to check if there is any unnecessary symbol or duplicated values.
table(survey4$treatment)
table(survey4$work_interfere)
table(survey4$remote_work)
table(survey4$tech_company)

#According to the tables shown above, there are few things that need to adjust.

#1st: Remove "-" sign from columns treatment and remote_work & remove "0" from work_interfere.
survey5 = filter(survey4, treatment != "-" & remote_work != "-" & work_interfere != "0")  

#2nd: Replace Y = Yes in the treatment column.
survey5$treatment[survey5$treatment == 'Y'] <- 'Yes'

#Check to see if the columns contain only proper values.
table(survey5$treatment)
table(survey5$work_interfere)
table(survey5$remote_work)
table(survey5$tech_company)


#11) Visuals

#A-Bar Chart
# mental_health_consequence
ggplot(data = survey5) + 
  geom_bar(mapping = aes(x = mental_health_consequence, fill = mental_health_consequence))

# Gender
ggplot(data = survey5) + 
  geom_bar(mapping = aes(x = Gender, fill = Gender))

#B-Histogram
survey5 %>% 
  ggplot(aes(Age)) + 
  geom_freqpoly(binwidth = 0.1)

#C-Plot a chart using ggplot function to visualize the covariation between categorical variables.
# mental_health_consequence and Gender.
ggplot(data = survey5) +
  geom_count(mapping = aes(x = mental_health_consequence, y = Gender))

# mental_health_consequence and Age.
ggplot(data = survey5) +
  geom_count(mapping = aes(x = mental_health_consequence, y = Age))


#12) Writing data from R to Excel files (xls|xlsx)
# Write the data set in a new workbook
write.xlsx(survey5, file = "survey-cleaned.xlsx",
           sheetName = "cleaned dataset", append = FALSE)
