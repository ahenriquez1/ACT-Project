# ECON 151 ACT PROJECT
# Angel Henriquez and Skanda Shastri

#clear working space
rm(list = ls())

# call packages
library(stargazer)
library(maps)
library(ggplot2)
library(rvest)
library(plyr)
library(readxl)
library(dplyr)

#setting working directory
setwd("~/Desktop/#ECON 151/ECON 151 Project Data")

cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)}

# turn off scientific notation except for big numbers. 
options(scipen = 9)

# downloading ACT data
# act 2017-2018
act18 <- read_excel("~/Downloads/act18.xls")
# act18 <- read_excel("act18.xls")
act18$year <- "2017-2018"
# only school data
act18 <- subset(act18, rtype == "S")
# changing column names
colnames(act18)[9] <- "grade12_2018"
colnames(act18)[10] <- "num_tested_2018"
colnames(act18)[15] <- "num_prof_2018"
colnames(act18)[16] <- "perc_prof_2018"
act18 <- act18[c(1:4, 6:10, 15:16)]

# act 2011-2012
act12 <- read_excel("~/Downloads/act12.xls")
# creating year column
act12$year <- "2011-2012"
# changing column names
colnames(act12)[1] <- "cnum"
colnames(act12)[2] <- "dnum"
colnames(act12)[3] <- "snum"
colnames(act12)[4] <- "cname"
colnames(act12)[5] <- "dname"
colnames(act12)[6] <- "sname"
colnames(act12)[7] <- "grade12_2012"
colnames(act12)[8] <- "num_tested_2012"
colnames(act12)[9] <- "perc_tested_2012"
colnames(act12)[10] <- "avg_score_2012"
colnames(act12)[11] <- "num_prof_2012"
colnames(act12)[12] <- "perc_prof_2012"
act12 <- act12[-c(1:4),] 

# merging 2010-2011 and 2017-2018 school data based on school name and county name
act_1218 <- merge(act12,act18, by.x=c("cnum", "cname", "snum", "sname", "dname"), by.y=c("Ccode", "cname", "Scode", "sname", "dname"), all = TRUE)
act_1218 <- act_1218[c(14,1,6,3,2,4:5,13,7:12,16:19)]

# Available High School Age Bracket population data (14 to 17 yrs old)
# government data file source: https://www.cde.ca.gov/ds/sd/sd/filesenr.asp

# 2011-2012 High School Population Data
school_enrollment_2011.2012 <- read.delim("~/Desktop/#ECON 151/ECON 151 Project Data/school_enrollment_2011_2012.txt")
high_school_enrollment_2011.2012 <- subset(school_enrollment_2011.2012, GR_9 != 0 & GR_10 != 0 & GR_11 != 0 & GR_12 != 0)
high_school_total_enrollment_2011.2012 <- high_school_enrollment_2011.2012[,c(4,22)]

# 2017-2018 High School Population Data
school_enrollment_2017.2018 <- read.delim("~/Desktop/#ECON 151/ECON 151 Project Data/school_enrollment_2017_2018.txt")
high_school_enrollment_2017.2018 <- subset(school_enrollment_2017.2018, GR_9 != 0 & GR_10 != 0 & GR_11 != 0 & GR_12 != 0)
high_school_total_enrollment_2017.2018 <- high_school_enrollment_2017.2018[,c(4,22)]

rm("school_enrollment_2011.2012")
rm("school_enrollment_2017.2018")

# population data/enrollment will be based on number of test takers
# made a sum by merging data variables based on school characteristics

# school total enrollments for 2011 - 2012 school year
# grouping variables by CDS number
population_by_cds_code_2011.2012 <- high_school_enrollment_2011.2012[c(1,22)]
# number of test takers per county
school_test_taker_number_2011.2012 <- population_by_cds_code_2011.2012 %>% 
  group_by(CDS_CODE) %>% 
  summarise_all(funs(sum))

# school total enrollments for 2017 - 2018 school year
# grouping variables by CDS number
population_by_cds_code_2017.2018 <- high_school_enrollment_2017.2018[c(1,22)]
# number of test takers per county
school_test_taker_number_2017.2018 <- population_by_cds_code_2017.2018 %>% 
  group_by(CDS_CODE) %>% 
  summarise_all(funs(sum))

rm("population_by_cds_code_2011.2012")
rm("population_by_cds_code_2017.2018")
rm("high_school_enrollment_2011.2012")
rm("high_school_enrollment_2017.2018")
rm("high_school_total_enrollment_2011.2012")
rm("high_school_total_enrollment_2017.2018")

# 2011-2012 Data Set
# creating clean data set for 2011-2012
all_enrollment_data <- merge(school_test_taker_number_2011.2012, school_test_taker_number_2017.2018, by.x=("CDS_CODE"), by.y=("CDS_CODE"), all = TRUE)
colnames(all_enrollment_data)[2] <- "total_enrollment_2012"
colnames(all_enrollment_data)[3] <- "total_enrollment_2018"
act_1218$cds <- as.numeric(act_1218$cds)

act_1218 <- merge(act_1218, all_enrollment_data, by.x=c("cds"), by.y = c("CDS_CODE"), all=TRUE)

rm("act12")
rm("act18")
rm("all_enrollment_data")
rm("school_test_taker_number_2011.2012")
rm("school_test_taker_number_2017.2018")

act_1218 <- act_1218[c(1:8,19,9:14,20,15:18)]
act_1218$cnum <- as.numeric(act_1218$cnum)
act_1218$dnum <- as.numeric(act_1218$dnum)
act_1218$snum <- as.numeric(act_1218$snum)
act_1218 <- subset(act_1218, cnum > 0)
act_1218$num_prof_2012 <- as.numeric(act_1218$num_prof_2012)
act_1218$num_prof_2018 <- as.numeric(act_1218$num_prof_2018)
act_12_data <- subset(act_1218, num_prof_2012 >= 0)
act_12_data$perc_prof_2012 <- as.numeric(act_12_data$perc_prof_2012)
act_18_data <- subset(act_1218, num_prof_2018 >= 0)
act_18_data$perc_prof_2018 <- as.numeric(act_18_data$perc_prof_2018)

# proficency percentage
act_12_data$perc_prof_2012 <- as.numeric(act_12_data$perc_prof_2012)
act_18_data$perc_prof_2018 <- as.numeric(act_18_data$perc_prof_2018)
avg_county_prof_2012 <- aggregate(act_12_data$perc_prof_2012,by=list(name=act_12_data$cname),data=act_12_data,FUN=mean)
avg_county_prof_2018 <- aggregate(act_18_data$perc_prof_2018,by=list(name=act_18_data$cname),data=act_18_data,FUN=mean)
act_12_data$cnum <- as.numeric(act_12_data$cnum)
prof_perc_1218 <- merge(avg_county_prof_2012, avg_county_prof_2018, by.x = c("name"), by.y = c("name"))
colnames(prof_perc_1218)[2] <- "avg_perc_proficient_12"
colnames(prof_perc_1218)[3] <- "avg_perc_proficient_18"

# number of schools
num_of_schools_2012 <- act_12_data %>% 
  group_by(cname) %>%
  dplyr::summarise(number_of_schools_2012 = sum(cnum))

school_numbers_act_12 <- act_12_data[c(5,2)]
school_numbers_act_12 <- school_numbers_act_12 %>% distinct()
school_numbers_act_12 <- merge(num_of_schools_2012,school_numbers_act_12, by.x=c("cname"), by.y=c("cname"))
school_numbers_act_12$number_of_schools_2012 <- school_numbers_act_12$number_of_schools_2012 / school_numbers_act_12$cnum
school_numbers_act_12 <- school_numbers_act_12[c(1,2)]

num_of_schools_2018 <- act_18_data %>% 
  group_by(cname) %>%
  dplyr::summarise(number_of_schools_2018 = sum(cnum))
school_numbers_act_18 <- act_18_data[c(5,2)]
school_numbers_act_18 <- school_numbers_act_18 %>% distinct()
school_numbers_act_18 <- merge(num_of_schools_2018,school_numbers_act_18, by.x=c("cname"), by.y=c("cname"))
school_numbers_act_18$number_of_schools_2018 <- school_numbers_act_18$number_of_schools_2018 / school_numbers_act_18$cnum
school_numbers_act_18 <- school_numbers_act_18[c(1,2)]

school_numbers_act_1218 <- merge(school_numbers_act_12, school_numbers_act_18, by.x = c("cname"), by.y = c("cname"))

# average number of test takers
avg_test_takers_2012 <- act_12_data %>% 
  group_by(cname) %>%
  dplyr::summarise(avg_test_takers_2012 = mean(total_enrollment_2012, na.rm = TRUE))

avg_test_takers_2018 <- act_18_data %>% 
  group_by(cname) %>%
  dplyr::summarise(avg_test_takers_2018 = mean(total_enrollment_2018, na.rm = TRUE))

avg_test_takers_1218 <- merge(avg_test_takers_2012, avg_test_takers_2018, by.x=c("cname"), by.y=c("cname"))

all_relevant_data_1218 <- merge(avg_test_takers_1218, school_numbers_act_1218, by.x=c("cname"), by.y=c("cname"))
all_relevant_data_1218 <- merge(all_relevant_data_1218,prof_perc_1218, by.x=c("cname"),by.y=c("name"))

# differences between both years
all_relevant_data_1218$diff_test_takers <- all_relevant_data_1218$avg_test_takers_2018 - all_relevant_data_1218$avg_test_takers_2012
all_relevant_data_1218$diff_number_of_schools <- all_relevant_data_1218$number_of_schools_2018 - all_relevant_data_1218$number_of_schools_2012
all_relevant_data_1218$diff_perc_prof <- all_relevant_data_1218$avg_perc_proficient_18 - all_relevant_data_1218$avg_perc_proficient_12

rm('act_12_data')
rm('act_1218')
rm('act_18_data')
rm('avg_county_prof_2012')
rm('avg_county_prof_2018')
rm('avg_test_takers_1218')
rm('avg_test_takers_2012')
rm('avg_test_takers_2018')
rm('num_of_schools_2012')
rm('num_of_schools_2018')
rm('prof_perc_1218')
rm('school_numbers_act_12')
rm('school_numbers_act_18')
rm('school_numbers_act_1218')

# Regression Formulas

# Percent Proficiency = B0 + B1 (Number of schools) + B2 (# of enrolled high school students)
# Percent Proficiency = B0 + B1 (Number of schools) + B2 (Average # of test takers)
reg1 <- lm(avg_perc_proficient_12 ~  number_of_schools_2012, data = all_relevant_data_1218)
reg2 <- lm(avg_perc_proficient_12 ~  number_of_schools_2012 + avg_test_takers_2012, data = all_relevant_data_1218)
reg3 <- lm(avg_perc_proficient_18 ~  number_of_schools_2018, data = all_relevant_data_1218)
reg4 <- lm(avg_perc_proficient_18 ~  number_of_schools_2018 + avg_test_takers_2018, data = all_relevant_data_1218)
reg5 <- lm(diff_perc_prof ~  diff_number_of_schools, data = all_relevant_data_1218)
reg6 <- lm(diff_perc_prof ~  diff_number_of_schools + diff_test_takers, data = all_relevant_data_1218)

# stargazer of all regressions
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "text")

# plots of specific data
# 2012: number of schools on county name
qplot(all_relevant_data_1218$number_of_schools_2012, all_relevant_data_1218$cname, xlab = "Number of Schools in 2012", ylab = "County Name", main = "Number of Schools per County in 2012")
# 2018: number of schools on county name
qplot(all_relevant_data_1218$number_of_schools_2018, all_relevant_data_1218$cname, xlab = "Number of Schools in 2018", ylab = "County Name", main = "Number of Schools per County in 2018")
# 2012: Test Taker Average on County Name
qplot(all_relevant_data_1218$avg_test_takers_2012, all_relevant_data_1218$cname, xlab = "Average Number of Test Takers", ylab = "County Name", main = "Average Number of Test Takers per County in 2012")
# 2018: Test Taker Average on County Name
qplot(all_relevant_data_1218$avg_test_takers_2018, all_relevant_data_1218$cname, xlab = "Average Number of Test Takers", ylab = "County Name", main = "Average Number of Test Takers per County in 2018")
# 2012: Percent Proficient on County Name
qplot(all_relevant_data_1218$avg_perc_proficient_12, all_relevant_data_1218$cname, xlab = "Average Percent of Students Proficient", ylab = "County Name", main = "Average Percent of Students Proficient in 2012")
# 2018: Percent Proficient on County Name
qplot(all_relevant_data_1218$avg_perc_proficient_18, all_relevant_data_1218$cname, xlab = "Average Percent of Students Proficient", ylab = "County Name", main = "Average Percent of Students Proficient in 2018")

# stargazers of specific data
# stargazer of 2012 schools
school_stargazer_2012 <- stargazer(all_relevant_data_1218[c("number_of_schools_2012","cname")], type="text",
                                   digits=2, title="Data of Number of Schools in 2012")
# stargazer of 2018 schools
school_stargazer_2018 <- stargazer(all_relevant_data_1218[c("number_of_schools_2018","cname")], type="text",
                                   digits=2, title="Data of Number of Schools in 2018")
# stargazer of 2012 test takers
test_takers_stargazer_2012 <- stargazer(all_relevant_data_1218[c("avg_test_takers_2012","cname")], type="text",
                                        digits=2, title="Data of Number of Test Takers in 2012")
# stargazer of 2018 test takers
test_takers_stargazer_2018 <- stargazer(all_relevant_data_1218[c("avg_test_takers_2018","cname")], type="text",
                                        digits=2, title="Data of Number of Test Takers in 2018")
# stargzaer of 2012 percentage proficient
test_takers_stargazer_2012 <- stargazer(all_relevant_data_1218[c("avg_perc_proficient_12","cname")], type="text",
                                        digits=2, title="Data of Average Percent Proficient in 2012")
# stargzaer of 2018 percentage proficient
test_takers_stargazer_2018 <- stargazer(all_relevant_data_1218[c("avg_perc_proficient_18","cname")], type="text",
                                        digits=2, title="Data of Average Percent Proficient in 2018")

# plots of specific data
# 2012: test taker barplot
test_takers_2012 <- c(all_relevant_data_1218$avg_test_takers_2012)
barplot(test_takers_2012, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "green", main = "Average Number of Test Takers per County in 2012", xlab = "Test Takers")
# 2018: test taker barplot
test_takers_2018 <- c(all_relevant_data_1218$avg_test_takers_2018)
barplot(test_takers_2018, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "green", main = "Average Number of Test Takers per County in 2018", xlab = "Test Takers")
# 2012: percentage proficient barplot
percent_prof_2012 <- c(all_relevant_data_1218$avg_perc_proficient_12)
barplot(percent_prof_2012, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "pink", main = "Average Percent Proficient per County in 2012", xlab = "Percent Proficient")
# 2018: percentage proficient barplot
percent_prof_2018 <- c(all_relevant_data_1218$avg_perc_proficient_18)
barplot(percent_prof_2018, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "pink", main = "Average Percent Proficient per County in 2018", xlab = "Percent Proficient")
# 2012: number of schools barplot
schools_2012 <- c(all_relevant_data_1218$number_of_schools_2012)
barplot(schools_2012, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "light blue", main = "Average Number of Schools per County in 2012", xlab = "Number of Schools")
# 2018: number of schools barplot
schools_2018 <- c(all_relevant_data_1218$number_of_schools_2018)
barplot(schools_2018, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "light blue", main = "Average Number of Schools per County in 2018", xlab = "Number of Schools")
# Difference in Number of Schools
diff_schools <- c(all_relevant_data_1218$diff_number_of_schools)
barplot(diff_schools, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "yellow",  main = "Difference in Number of Schools per County from 2012 to 2018", xlab = "Difference in Number of Schools")
# difference in number of test takers
diff_act_takers <- c(all_relevant_data_1218$diff_test_takers)
barplot(diff_act_takers, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "purple", main = "Difference in Number of Test Takers per County from 2012 to 2018", xlab = "Difference in Number of Test Takers")
# difference in percent proficiency
diff_proficiency <- c(all_relevant_data_1218$diff_perc_prof)
barplot(diff_proficiency, names.arg = all_relevant_data_1218$cname, horiz = TRUE, las = 1, col = "orange", main = "Difference in Percent Proficient per County from 2012 to 2018", xlab = "Difference in Percent Proficient")
