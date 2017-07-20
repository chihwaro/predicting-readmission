library(caret)
library(psych)
library(car)
library(ggplot2)

#always set seed to make code reproducable
set.seed(3456)

#set wd and import diabetes dataset into R
setwd("")
diabetes_data <- read.csv("diabetic_data.csv")

#view first few rows of data and structure of dataset
head(diabetes_data)
str(diabetes_data)
colnames(diabetes_data)
#view number of rows and columns in initial dataset
dim(diabetes_data)

#######################
# DATA PREPARATION#####
#######################

# remove weight variable [,6] due to 97% missing data
# remove medical_specialty variable - diff. control admitting physician
diabetes_data[1:10,c(1,2,6,11,12)]
filteredDiabetes <- diabetes_data[,-c(1,2,6,11,12)]
dim(filteredDiabetes) 
# 45 columns remaining "-encounter id;-patient #; -weight; -payer code; & -medical_specialty"

# initial data cleaning for analysis of whether data is missing at random
# conducted to determine if imputation should be applied towards our final dataset
# replace missing gender records with NA
summary(filteredDiabetes$gender)
filteredDiabetes$gender[!(filteredDiabetes$gender=="Male"|filteredDiabetes$gender=="Female")] <- NA
summary(filteredDiabetes$gender)

# redefine gender variable as factor with 2 levels Male & Female
filteredDiabetes$gender<-factor(filteredDiabetes$gender)     
str(filteredDiabetes$gender)
summary(filteredDiabetes$gender)

#view race distributions for dataset
#2273 missing value / 1506 Other
#replace 2273 missing values with NA
table(filteredDiabetes$race)
str(filteredDiabetes$race)
filteredDiabetes$race[filteredDiabetes$race=="?"] <- NA
summary(filteredDiabetes$race)

# redefine race variable as factor with 5 levels AA,Asian,Caucasian,Hispanic,Other
filteredDiabetes$race<-factor(filteredDiabetes$race)
str(filteredDiabetes$race)
summary(filteredDiabetes$race)

## use caucasian as reference category in our analysis

#view filteredDiabetes data structure
dim(filteredDiabetes) #45 variables left
str(filteredDiabetes)

# removes features for medication with near zero variance 
# help us remove irrelevant features
nzv <- nearZeroVar(filteredDiabetes) #no metrics just return identified columns indexes
str(nzv)
filteredDiabetes <- filteredDiabetes[, -nzv]
dim(filteredDiabetes) #45 to 27 variables
str(filteredDiabetes)


##RECODE DISCHARGE DISPOSITION##
# define variable as factor
filteredDiabetes$discharge_disposition_id <- as.factor(filteredDiabetes$discharge_disposition_id)
str(filteredDiabetes$discharge_disposition_id)
#Levels: 1-20, 22-25, 27, 28
head(filteredDiabetes$discharge_disposition_id)
table(filteredDiabetes$discharge_disposition_id)
# New levels:
# Discharge Home = '1' Same 
# Admitted to Any Healthcare Provider / Facility = 2
# <- 2-5,9-10,12,22-24,27-30 
# Discharge Home with Care = '6,8' <- 3 
# Missing (18,25,26) <- N/A 
# Remove Left Against Medical Advice ='7'  
# uncontrollable and skew results
# Remove Expired (11,13,14,19-21)
# remove expired and left against medical advice discharge disposition
filteredDiabetes <- filteredDiabetes[!(filteredDiabetes$discharge_disposition_id %in% c(7,11,13,14,19,20,21)),]
table(filteredDiabetes$discharge_disposition_id)
filteredDiabetes$discharge_disposition_id <-factor(filteredDiabetes$discharge_disposition_id)
table(filteredDiabetes$discharge_disposition_id)

#recode discharge disposition variable to factor with 3 levels
# 1 - Discharge Home; 2 - Admitted to Any other Healthcare Provider; 3 - Discharge Home w/ Care
filteredDiabetes$discharge_disposition_id<-recode(filteredDiabetes$discharge_disposition_id,
     "c('1')='1';
     c('2','3','4','5','9','10','12','22','23','24','27','28')='2';
     c('6','8')='3';
     else=NA")
str(filteredDiabetes$discharge_disposition_id)
table(filteredDiabetes$discharge_disposition_id)

##RECODE ADMISSION TYPE ID##
table(filteredDiabetes$admission_type_id)
# define variable as factor
filteredDiabetes$admission_type_id <- as.factor(filteredDiabetes$admission_type_id)
str(filteredDiabetes$admission_type_id)
table(filteredDiabetes$admission_type_id)
# New levels:
# 1 - emergency (1) / trauma center (7)
# 2- urgent (2)
# 3 - elective (3) & newborn (4)
# Missing data (5,6,8) <- NA
# 10,396 missing values
table(filteredDiabetes$admission_type_id)
# recode admission type id to 1 emergency / trauma, 2 urgent, 3 elective / newborn, NA missing
# new factor with 3 levels
filteredDiabetes$admission_type_id <- recode(filteredDiabetes$admission_type_id, "
  c('1','7')='1'; c('2')='2'; c('3','4')='3' ; else=NA")
table(filteredDiabetes$admission_type_id)
summary(filteredDiabetes$admission_type_id)

##RECODE ADMISSION SOURCE##

# define variable as factor
filteredDiabetes$admission_source_id <- as.factor(filteredDiabetes$admission_source_id)
str(filteredDiabetes$admission_source_id)
#Levels: 1-11, 13, 14, 17, 20, 22, 25
table(filteredDiabetes$admission_source_id)
# New levels:
# Admitted by Referral = 1
# 1 <- 1-3,11-12,23
# Transfered from Other Care Center = 2
# 2 <- 4-6,8,10,18,19,22,25,26
# Emergency Room / Visit =3
# 3 <- 7,13,14,24
# Missing (9,15,17,20-21) <-NA #7,067 missing values
# recode admission source id to factor with 3 levels
# Admitted by Referral = 1; Transfered from Other Care Center = 2; Emergency Room / Visit =3
filteredDiabetes$admission_source_id <-recode(filteredDiabetes$admission_source_id,
    "c('1','2','3','11','12','23')='1';
     c('4','5','6','8','10','18','19','22','25','26')='2';
     c('7','13','14','24')='3';  
     else=NA")
summary(filteredDiabetes$admission_source_id)
str(filteredDiabetes$admission_source_id)
table(filteredDiabetes$admission_source_id)

# view age distribution for diabetes dataset
table(filteredDiabetes$age)
summary(filteredDiabetes$age) #age factor w/10 levels
levels(filteredDiabetes$age) # 10 levels of age

# Transform age into smaller groups - factor w/ 3 levels
# "young" = [0-40], "middle-age" = [40-70], "old" = [70-100]
# recode intervals for age and define as factor variable
filteredDiabetes$age <-ifelse(filteredDiabetes$age %in%
   c("[0-10)","[10-20)","[20-30)","[30-40)"),"young",
   ifelse(filteredDiabetes$age %in% c("[40-50)","[50-60)","[60-70)"), "middle-age","old"))
filteredDiabetes$age <- as.factor(filteredDiabetes$age)
# View structure new $agegroup variable and table of class distributions
# Factor with 3 levels
str(filteredDiabetes$age) 
table(filteredDiabetes$age)

# view Diabetes data diag_1 structure 
str(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_3)

# icd9 codes
# change all missing values ("?") into NA
filteredDiabetes$diag_1[filteredDiabetes$diag_1=="?"] <- NA
filteredDiabetes$diag_2[filteredDiabetes$diag_2=="?"] <- NA
filteredDiabetes$diag_3[filteredDiabetes$diag_3=="?"] <- NA
summary(filteredDiabetes$diag_1)
summary(filteredDiabetes$diag_2)
summary(filteredDiabetes$diag_3)

# change all diabetes 250.xx codes to "Z"
# way to combine the two regex into one?
filteredDiabetes$diag_1 <- gsub("(\\b250\\...)", "Z", filteredDiabetes$diag_1)
filteredDiabetes$diag_1 <- gsub("(\\b250\\..)", "Z", filteredDiabetes$diag_1)
filteredDiabetes$diag_2 <- gsub("(\\b250\\...)", "Z", filteredDiabetes$diag_2)
filteredDiabetes$diag_2 <- gsub("(\\b250\\..)", "Z", filteredDiabetes$diag_2)
filteredDiabetes$diag_3 <- gsub("(\\b250\\...)", "Z", filteredDiabetes$diag_3)
filteredDiabetes$diag_3 <- gsub("(\\b250\\..)", "Z", filteredDiabetes$diag_3)
table(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_2)
table(filteredDiabetes$diag_3)

# recode diag_1 variable
filteredDiabetes$diag_1 <- ifelse(filteredDiabetes$diag_1 %in% 390:459 |filteredDiabetes$diag_1 %in% 785, 
"circulatory",ifelse(filteredDiabetes$diag_1 %in% 460:519 |filteredDiabetes$diag_1==786,
"respiratory",ifelse(filteredDiabetes$diag_1 %in% 520:579 |filteredDiabetes$diag_1==787,
"digestive",ifelse(filteredDiabetes$diag_1=="Z",
"diabetes", ifelse(filteredDiabetes$diag_1 %in% 800:999, 
"injury", ifelse(filteredDiabetes$diag_1 %in% 710:739,
"musculoskeletal", ifelse(filteredDiabetes$diag_1 %in% 580:629 |filteredDiabetes$diag_1==788,
"genitourinary","Other")
))))))
# define coded diag_1 as factor
filteredDiabetes$diag_1 <-as.factor(filteredDiabetes$diag_1)
str(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_1)
summary(filteredDiabetes$diag_1)

# recode diag_2 variable
filteredDiabetes$diag_2 <- ifelse(filteredDiabetes$diag_2 %in% 350:459 |filteredDiabetes$diag_2 %in% 785, 
"circulatory",ifelse(filteredDiabetes$diag_2 %in% 460:519 |filteredDiabetes$diag_2==786,
"respiratory",ifelse(filteredDiabetes$diag_2 %in% 520:579 |filteredDiabetes$diag_2==787,
"digestive",ifelse(filteredDiabetes$diag_2=="Z",
"diabetes", ifelse(filteredDiabetes$diag_2 %in% 800:999, 
"injury", ifelse(filteredDiabetes$diag_2 %in% 710:739,
"musculoskeletal", ifelse(filteredDiabetes$diag_2 %in% 580:629 |filteredDiabetes$diag_2==788,
"genitourinary","Other")
))))))
# define coded diag_2 as factor
filteredDiabetes$diag_2 <-as.factor(filteredDiabetes$diag_2)
str(filteredDiabetes$diag_2)
table(filteredDiabetes$diag_2)
summary(filteredDiabetes$diag_2)

# recode diag_3 variable
filteredDiabetes$diag_3 <- ifelse(filteredDiabetes$diag_3 %in% 350:459|filteredDiabetes$diag_3 %in% 785, 
"circulatory",ifelse(filteredDiabetes$diag_3 %in% 460:519 |filteredDiabetes$diag_3==786,
"respiratory",ifelse(filteredDiabetes$diag_3 %in% 520:579 |filteredDiabetes$diag_3==787,
"digestive",ifelse(filteredDiabetes$diag_3=="Z",
"diabetes", ifelse(filteredDiabetes$diag_3 %in% 800:999, 
"injury", ifelse(filteredDiabetes$diag_3 %in% 710:739,
"musculoskeletal", ifelse(filteredDiabetes$diag_3 %in% 580:629 |filteredDiabetes$diag_3==788,
"genitourinary","Other")
))))))
# define coded diag_3 as factor
filteredDiabetes$diag_3 <-as.factor(filteredDiabetes$diag_3)
str(filteredDiabetes$diag_3)
table(filteredDiabetes$diag_3)
summary(filteredDiabetes$diag_3)

###Investigating whether we should impute data:

#look at ammount of missing cases
completeDiabetes<-complete.cases(filteredDiabetes)
summary(completeDiabetes)
ccn(filteredDiabetes)
md.pattern(filteredDiabetes) #pattern of missing variables
md.pairs(filteredDiabetes)

#exploring randomness of missing variables:
filteredDiabetes %>% select(diag_3, diag_2) %>% marginplot
filteredDiabetes %>% select(diag_1, diag_2) %>% marginplot
filteredDiabetes %>% select(diag_3, diag_1) %>% marginplot
filteredDiabetes %>% select(discharge_disposition_id, diag_1) %>% marginplot
filteredDiabetes %>% select(discharge_disposition_id, admission_source_id) %>% marginplot
filteredDiabetes %>% select(discharge_disposition_id, race) %>% marginplot
filteredDiabetes %>% select(discharge_disposition_id, admission_type_id) %>% marginplot
filteredDiabetes %>% select(discharge_disposition_id, gender) %>% marginplot
pbox(filteredDiabetes, pos=1)
pbox(filteredDiabetes, pos=2)
pbox(filteredDiabetes, pos=3)
# distributions indicate that variables are not missing at random. 
# thus imputation can not be used in data preparation

# Need to reprocess our data, this time deleting missing data instead of turning them into NA
# Clear our environment:
rm(list = ls())

#######################################
## DATA PREPARATION: NO IMPUTATION ####
#######################################

#set wd and import diabetes dataset into R
setwd("")
diabetes_data <- read.csv("diabetic_data.csv")

#view first few rows of data and structure of dataset
head(diabetes_data)
str(diabetes_data)
colnames(diabetes_data)
#view number of rows and columns in initial dataset
dim(diabetes_data)

# remove weight variable [,6]  due to 97% missing data
# remove medical_specialty variable - diff. control admitting physician
diabetes_data[1:10,c(1,2,6,11,12)]
filteredDiabetes <- diabetes_data[,-c(1,2,6,11,12)]
dim(filteredDiabetes) 
# 45 columns remaining "-encounter id;-patient #; -weight; -payer code; & -medical_specialty"

# remove missing gender records
nrow(filteredDiabetes)
table(filteredDiabetes$gender)
filteredDiabetes <- subset(filteredDiabetes, filteredDiabetes$gender=="Male"|filteredDiabetes$gender=="Female")
table(filteredDiabetes$gender)

# redefine gender variable as factor with 2 levels Male & Female
filteredDiabetes$gender<-factor(filteredDiabetes$gender)
str(filteredDiabetes$gender)

#view race distributions for dataset
#2273 missing value / 1506 Other
#remove 2273 missing values from initial dataset
table(filteredDiabetes$race)
str(filteredDiabetes$race)
filteredDiabetes <- subset(filteredDiabetes, filteredDiabetes$race!="?")
table(filteredDiabetes$race)
# redefine race variable as factor with 5 levels AA,Asian,Caucasian,Hispanic,Other
table(droplevels(filteredDiabetes)$race)
filteredDiabetes$race<-factor(filteredDiabetes$race)
str(filteredDiabetes$race)

#view filteredDiabetes data structure
dim(filteredDiabetes) #45 variables left
str(filteredDiabetes)

# removes features for medication with near zero variance for the most part
nzv <- nearZeroVar(filteredDiabetes) #no metrics just return identified columns indexes
str(nzv)
filteredDiabetes <- filteredDiabetes[, -nzv]
dim(filteredDiabetes) #45 to 27 variables
str(filteredDiabetes)

# Recode Discharge Disposition
# define variable as factor
filteredDiabetes$discharge_disposition_id <- as.factor(filteredDiabetes$discharge_disposition_id)
str(filteredDiabetes$discharge_disposition_id)
#Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 22 23 24 25 27 28
head(filteredDiabetes$discharge_disposition_id)
table(filteredDiabetes$discharge_disposition_id)

# Discharge Home = '1' Same 
# Admitted to Any Healthcare Provider / Facility = 2
# <- 2-5,9-10,12,22-24,27-30 
# Discharge Home with Care = '6,8' <- 3 
# Left Against Medical Advice ='7' <- 4 - should remove this as well 
  # uncontrollable and skew results
# Missing (18,25,26) <-5 # 4680 N/A 
# Expired (11,13,14,19-21) <-NA (Remove Not Relevant) #2247 Expired AMA not very much
# compared with rest of categories - what to do? want method that uneven class
# distributions not issue
# remove missing and expired and left against medical advice discharge disposition
filteredDiabetes <- filteredDiabetes[!(filteredDiabetes$discharge_disposition_id %in% c(7,11,13,14,18,19,20,21,25,26)),]
table(filteredDiabetes$discharge_disposition_id)
filteredDiabetes$discharge_disposition_id <-factor(filteredDiabetes$discharge_disposition_id)
table(filteredDiabetes$discharge_disposition_id)

#recode discharge disposition variable to factor with 3 levels
# 1 - Discharge Home; 2 - Admitted to Any other Healthcare Provider; 3 - Discharge Home w/ Care
filteredDiabetes$discharge_disposition_id<-recode(filteredDiabetes$discharge_disposition_id,
     "c('1')='1';
     c('2','3','4','5','9','10','12','22','23','24','27','28')='2';
     else= '3'")
str(filteredDiabetes$discharge_disposition_id)
table(filteredDiabetes$discharge_disposition_id)

# Recode Admission Type ID
table(filteredDiabetes$admission_type_id)
  # define variable as factor
filteredDiabetes$admission_type_id <- as.factor(filteredDiabetes$admission_type_id)
str(filteredDiabetes$admission_type_id)
table(filteredDiabetes$admission_type_id)
# 1 - emergency '1' / trauma center '7'
# 2- urgent
# 3 - elective & 4 - newborn
# 6 remove missing data <- unknown 5,6,8 -
# want to also combine emergency and trauma center
# change newborn to elective
   
# 10,396 missing values
#remove observations with missing values for admission type id
filteredDiabetes <- filteredDiabetes[!(filteredDiabetes$admission_type_id %in% c(5,6,8)),]
table(filteredDiabetes$admission_type_id)
filteredDiabetes$admission_type_id <-factor(filteredDiabetes$admission_type_id)
table(filteredDiabetes$admission_type_id)

# recode admission type id to 1 emergency / trauma, 2 urgent, 3 elective / newborn
# new factor with 3 levels
# using data with missing / unknown removed
filteredDiabetes$admission_type_id <- recode(filteredDiabetes$admission_type_id, "
  c('1','7')='1'; c('2')='2'; else='3'")
table(filteredDiabetes$admission_type_id)

# RECODE ADMISSION SOURCE
# define variable as factor
filteredDiabetes$admission_source_id <- as.factor(filteredDiabetes$admission_source_id)
str(filteredDiabetes$admission_source_id)
#Levels: 1 2 3 4 5 6 7 8 9 10 11 13 14 17 20 22 25
table(filteredDiabetes$admission_source_id)
# Admitted by Referral = 1
# 1 <- 1-3,11-12,23
# Transfered from Other Care Center = 2
# 2 <- 4-6,8,10,18,19,22,25,26
# Emergency Room / Visit =3
# 3 <- 7,13,14,24

# Missing (9,15,17,20-21) <-NA #7,067 missing values
# remove observations with missing values for admission source 
filteredDiabetes <- filteredDiabetes[!
  (filteredDiabetes$admission_source_id %in% c(9,15,17,20,21)),]
table(filteredDiabetes$admission_source_id)
filteredDiabetes$admission_source_id <-factor(filteredDiabetes$admission_source_id)
table(filteredDiabetes$admission_source_id)

#recode admission source id to factor with 3 levels
# Admitted by Referral = 1; Transfered from Other Care Center = 2; Emergency Room / Visit =3
filteredDiabetes$admission_source_id <-recode(filteredDiabetes$admission_source_id,
      "c('1','2','3','11','12','23')='1';
       c('4','5','6','8','10','18','19','22','25','26')='2';else='3'")
str(filteredDiabetes$admission_source_id)
table(filteredDiabetes$admission_source_id)

# view age distribution for diabetes dataset
table(filteredDiabetes$age)
str(filteredDiabetes$age) #age factor w/10 levels
levels(filteredDiabetes$age) # 10 levels of age

# Transform age into smaller groups - factor w/ 3 levels
# "young" = [0-40], "middle-age" = [40-70], "old" = [70-100]
# recode intervals for age and define as factor variable
table(filteredDiabetes$age)
filteredDiabetes$age <-ifelse(filteredDiabetes$age %in%
   c("[0-10)","[10-20)","[20-30)","[30-40)"),"young",
   ifelse(filteredDiabetes$age %in% c("[40-50)","[50-60)","[60-70)"), "middle-age","old")
    )
filteredDiabetes$age <- as.factor(filteredDiabetes$age)
# View structure age and table of class distributions
# Factor with 3 levels
str(filteredDiabetes$age) 
table(filteredDiabetes$age)

#view Diabetes data diag_1 structure 
str(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_3)

#icd9 codes
#check for missing values for diagnosis
# still all character do we need to assign factor to use dummy variable
filteredDiabetes <- filteredDiabetes[!(filteredDiabetes$diag_1 %in% c("?")),]
filteredDiabetes$diag_1 <- factor(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_1)

filteredDiabetes <- filteredDiabetes[!(filteredDiabetes$diag_2 %in% c("?")),]
filteredDiabetes$diag_1 <- factor(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_2)

filteredDiabetes <- filteredDiabetes[!(filteredDiabetes$diag_3 %in% c("?")),]
filteredDiabetes$diag_1 <- factor(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_3)

#cleaning up diabetes 250.xx code to z
#need to see if there is a way to combine the two regex into one (searching for one or two digits after the dot)
filteredDiabetes$diag_1 <- gsub("(\\b250\\...)", "Z", filteredDiabetes$diag_1)
filteredDiabetes$diag_1 <- gsub("(\\b250\\..)", "Z", filteredDiabetes$diag_1)
filteredDiabetes$diag_2 <- gsub("(\\b250\\...)", "Z", filteredDiabetes$diag_2)
filteredDiabetes$diag_2 <- gsub("(\\b250\\..)", "Z", filteredDiabetes$diag_2)
filteredDiabetes$diag_3 <- gsub("(\\b250\\...)", "Z", filteredDiabetes$diag_3)
filteredDiabetes$diag_3 <- gsub("(\\b250\\..)", "Z", filteredDiabetes$diag_3)
table(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_2)
table(filteredDiabetes$diag_3)

# recode diag_1 variable
filteredDiabetes$diag_1 <- ifelse(filteredDiabetes$diag_1 %in% 390:459 |filteredDiabetes$diag_1 %in% 785, 
    "circulatory",ifelse(filteredDiabetes$diag_1 %in% 460:519 |filteredDiabetes$diag_1==786,
    "respiratory",ifelse(filteredDiabetes$diag_1 %in% 520:579 |filteredDiabetes$diag_1==787,
    "digestive",ifelse(filteredDiabetes$diag_1=="Z",
    "diabetes", ifelse(filteredDiabetes$diag_1 %in% 800:999, 
    "injury", ifelse(filteredDiabetes$diag_1 %in% 710:739,
    "musculoskeletal", ifelse(filteredDiabetes$diag_1 %in% 580:629 |filteredDiabetes$diag_1==788,
    "genitourinary","Other")
    ))))))
# define coded diag_1 as factor
filteredDiabetes$diag_1 <-as.factor(filteredDiabetes$diag_1)
str(filteredDiabetes$diag_1)
table(filteredDiabetes$diag_2)

# recode diag_2 variable
filteredDiabetes$diag_2 <- ifelse(filteredDiabetes$diag_2 %in% 350:459 |filteredDiabetes$diag_2 %in% 785, 
"circulatory",ifelse(filteredDiabetes$diag_2 %in% 460:519 |filteredDiabetes$diag_2==786,
"respiratory",ifelse(filteredDiabetes$diag_2 %in% 520:579 |filteredDiabetes$diag_2==787,
"digestive",ifelse(filteredDiabetes$diag_2=="Z",
"diabetes", ifelse(filteredDiabetes$diag_2 %in% 800:999, 
"injury", ifelse(filteredDiabetes$diag_2 %in% 710:739,
"musculoskeletal", ifelse(filteredDiabetes$diag_2 %in% 580:629 |filteredDiabetes$diag_2==788,
"genitourinary","Other")
       ))))))
# define coded diag_2 as factor
filteredDiabetes$diag_2 <-as.factor(filteredDiabetes$diag_2)
str(filteredDiabetes$diag_2)
table(filteredDiabetes$diag_2)

# recode diag_3 variable
filteredDiabetes$diag_3 <- ifelse(filteredDiabetes$diag_3 %in% 350:459|filteredDiabetes$diag_3 %in% 785, 
"circulatory",ifelse(filteredDiabetes$diag_3 %in% 460:519 |filteredDiabetes$diag_3==786,
"respiratory",ifelse(filteredDiabetes$diag_3 %in% 520:579 |filteredDiabetes$diag_3==787,
"digestive",ifelse(filteredDiabetes$diag_3=="Z",
"diabetes", ifelse(filteredDiabetes$diag_3 %in% 800:999, 
"injury", ifelse(filteredDiabetes$diag_3 %in% 710:739,
"musculoskeletal", ifelse(filteredDiabetes$diag_3 %in% 580:629 |filteredDiabetes$diag_3==788,
"genitourinary","Other")
          ))))))
# define coded diag_3 as factor
filteredDiabetes$diag_3 <-as.factor(filteredDiabetes$diag_3)
str(filteredDiabetes$diag_3)
table(filteredDiabetes$diag_3)

#######################
# MODEL PLANNING  #####
#######################

# coding outcome variable
table(filteredDiabetes$readmitted)

# new outcome variable 1(yes readmitted) & 0(not readmitted)
filteredDiabetes$readmittedbinary <- ifelse(filteredDiabetes$readmitted=="NO","NR","R")
table(filteredDiabetes$readmittedbinary)
# define new outcome variable as factor
filteredDiabetes$readmittedbinary <- as.factor(filteredDiabetes$readmittedbinary)
str(filteredDiabetes$readmittedbinary)
# relevel readmitted binary class variable with Readmitted 1st level
filteredDiabetes$readmittedbinary <- relevel(filteredDiabetes$readmittedbinary,"R")
str(filteredDiabetes$readmittedbinary)

#view dataset nrows and ncols
dim(filteredDiabetes)

# remove uncoded readmitted variable from dataset
filteredDiabetes2 <- filteredDiabetes[,-c(27)]

## test for highly correlated variables - defined numeric variables to factor
diabetes_numeric <- filteredDiabetes2[,sapply(filteredDiabetes2,is.numeric)]
ncol(diabetes_numeric)
descrCor <-  cor(diabetes_numeric)
descrCor
# no highly correlated numeric variables in dataset that need to be removed

# summary of final dataset (filteredDiabetes2)
str(filteredDiabetes2)
dim(filteredDiabetes2)
table(filteredDiabetes2$readmittedbinary) 
# readmitted = 38,385 and not readmitted = 42,246

# view dataset summary stats
dim(filteredDiabetes2) # final dataset 27 variables and 80,631 observations
table(filteredDiabetes$gender) # Female (43,696) & Male (36,935)
table(filteredDiabetes$race) # AA (16,527)
table(filteredDiabetes$age) # young (5,234); middle-age (40,286); old (36,299)
# "young" = [0-40], "middle-age" = [40-70], "old" = [70-100]

#A1ctest results, Med Change, Diabetes Med summary stats
table(filteredDiabetes$A1Cresult) 
table(filteredDiabetes$change) 
table(filteredDiabetes$diabetesMed) 

# diagnosis data prep Numeric Data 
table(filteredDiabetes$number_diagnoses)

# view time in hospital summary statistics
summary(filteredDiabetes$time_in_hospital)
table(filteredDiabetes$time_in_hospital)

#Perserve class distribution and split 80% and 20% subsets
trainIndex <- createDataPartition(filteredDiabetes2$readmittedbinary, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

#create training set subset (80%)
diabetesTrain <- filteredDiabetes2[ trainIndex,]
dim(diabetesTrain)
#create training set subset (20%)
diabetesTest  <- filteredDiabetes2[-trainIndex,]
dim(diabetesTest)

#######################
# MODEL BUILDING  #####
#######################

#setup of parameters to control the sampling during parameter tuning and testing
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3,
                         classProbs=TRUE,
                         summaryFunction = twoClassSummary, 
                         allowParallel = FALSE)

ctrldown <- trainControl(method="repeatedcv", number=10, repeats=3,
           classProbs=TRUE,
           summaryFunction = twoClassSummary,
           sampling="down",
           allowParallel = FALSE)

ctrlup <- trainControl(method="repeatedcv", number=10, repeats=3,
            classProbs=TRUE,
            summaryFunction = twoClassSummary, 
            sampling="up",
            allowParallel = FALSE) 

ctrlsmote <-  trainControl(method="repeatedcv", number=10, repeats=3,
                classProbs=TRUE,
                summaryFunction = twoClassSummary, 
                sampling="smote",
                allowParallel = FALSE)

ctrlrose <-  trainControl(method="repeatedcv", number=10, repeats=3,
                classProbs=TRUE,
                summaryFunction = twoClassSummary, 
                sampling="rose",
                allowParallel = FALSE)

# Conduct chisquare test for hypothesis 1
h1Atest <- chisq.test(filteredDiabetes2$race,filteredDiabetes2$readmittedbinary)
h1Btest <- chisq.test(filteredDiabetes2$age,filteredDiabetes2$readmittedbinary)
h1Ctest <- chisq.test(filteredDiabetes2$gender,filteredDiabetes2$readmittedbinary)
# view results of hypothesis 1 testing
h1Atest
h1Btest
h1Ctest
# Conduct logistic regression for hypothesis 2
# rose performance worse, up/down/
h2test <- train(readmittedbinary ~ insulin, 
                   trControl = ctrl,
                   metric = "ROC", 
                   data = diabetesTrain, 
                   method = "glm")
p.h2test <- predict(h2test,diabetesTest)
confusionMatrix(p.h2Btest,diabetesTest$readmittedbinary)
summary(h2test)

# hyp. 2 w/ up-sampling for class
h2test.up <- train(readmittedbinary ~ insulin, 
                trControl = ctrlup,
                metric = "ROC",
                data = diabetesTrain, 
                method = "glm")
p.h2test.up <- predict(h2test.up,diabetesTest)
h2test.up$finalModel
confusionMatrix(p.h2test.up,diabetesTest$readmittedbinary)
summary(h2test.up)

# hyp. 2 w/ down-sampling for class
h2test.down <- train(readmittedbinary ~ insulin, 
                trControl = ctrldown,
                metric = "ROC", 
                data = diabetesTrain, 
                method = "glm")
p.h2test.down <- predict(h2test.down,diabetesTest)
confusionMatrix(p.h2test.down,diabetesTest$readmittedbinary)

# hyp. 2 w/ rose-sampling for class
h2test.rose <- train(readmittedbinary ~ insulin, 
                   trControl = ctrlrose,
                   metric = "ROC", 
                   data = diabetesTrain, 
                   method = "glm")
p.h2test.rose <- predict(h2test.rose,diabetesTest)
confusionMatrix(p.h2test.rose,diabetesTest$readmittedbinary)

# hyp. 2 w/ smote-sampling for class
h2test.smote <- train(readmittedbinary ~ insulin, 
                   trControl = ctrlsmote,
                   metric = "ROC", 
                   data = diabetesTrain, 
                   method = "glm")
p.h2test.smote <- predict(h2test.smote,diabetesTest)
confusionMatrix(p.h2test.smote,diabetesTest$readmittedbinary)

# Conduct logistic regression for hypothesis 3
h3test <- train(readmittedbinary ~ num_lab_procedures, 
                 trControl = ctrl,
                 metric = "ROC", #using AUC to find best performing parameters
                 data = diabetesTrain, 
                 method = "glm")
p.h3test <- predict(h3test,diabetesTest)
confusionMatrix(p.h3test,diabetesTest$readmittedbinary)

#up-sampling readmission class for hypothesis 3 testing
h3test.up <- train(readmittedbinary ~ num_lab_procedures, 
                   trControl = ctrlup,
                   metric = "ROC", #using AUC to find best performing parameters
                   data = diabetesTrain, 
                   method = "glm")
p.h3test.up <- predict(h3test.up,diabetesTest)
confusionMatrix(p.h3test.up,diabetesTest$readmittedbinary)

#down-sampling readmission class for hypothesis 3 testing
h3test.down <- train(readmittedbinary ~ num_lab_procedures, 
                trControl = ctrldown,
                metric = "ROC", #using AUC to find best performing parameters
                data = diabetesTrain, 
                method = "glm")
p.h3test.down <- predict(h3test.down,diabetesTest)
confusionMatrix(p.h3test.down,diabetesTest$readmittedbinary)

#rose-sampling readmission class for hypothesis 3 testing
h3test.rose <- train(readmittedbinary ~ num_lab_procedures, 
                     trControl = ctrlrose,
                     metric = "ROC", #using AUC to find best performing parameters
                     data = diabetesTrain, 
                     method = "glm")
p.h3test.rose <- predict(h3test.rose,diabetesTest)
confusionMatrix(p.h3test.rose,diabetesTest$readmittedbinary)

#smote-sampling readmission class for hypothesis 3 testing
h3test.smote <- train(readmittedbinary ~ num_lab_procedures, 
                     trControl = ctrlsmote,
                     metric = "ROC", #using AUC to find best performing parameters
                     data = diabetesTrain, 
                     method = "glm")
p.h3test.smote <- predict(h3test.smote,diabetesTest)
confusionMatrix(p.h3test.smote,diabetesTest$readmittedbinary)

# Conduct logistic regression for hypothesis 4 testing
h4test.down <- train(readmittedbinary ~ change, 
                 trControl = ctrldown,
                 metric = "ROC", #using AUC to find best performing parameters
                 data = diabetesTrain, 
                 method = "glm")
p.h4test.down <- predict(h4test.down,diabetesTest)
confusionMatrix(p.h4test.down,diabetesTest$readmittedbinary)
summary(h4test.down$finalModel)

# hyp. 4 w/ up-sampling for class
h4test.up <- train(readmittedbinary ~ change, 
                trControl = ctrlup,
                metric = "ROC", #using AUC to find best performing parameters
                data = diabetesTrain, 
                method = "glm")
p.h4test.up <- predict(h4test.up,diabetesTest)
confusionMatrix(p.h4test.up,diabetesTest$readmittedbinary)
summary(h4test.up)

# hyp. 4 w/ down-sampling for class
h4test.down <- train(readmittedbinary ~ change, 
                   trControl = ctrldown,
                   metric = "ROC", #using AUC to find best performing parameters
                   data = diabetesTrain, 
                   method = "glm")
p.h4test.down <- predict(h4test.down,diabetesTest)
confusionMatrix(p.h4test.down,diabetesTest$readmittedbinary)
summary(h4test.down)

# hyp. 4 w/ rose-sampling for class
h4test.rose <- train(readmittedbinary ~ change, 
                   trControl = ctrlrose,
                   metric = "ROC", #using AUC to find best performing parameters
                   data = diabetesTrain, 
                   method = "glm")
p.h4test.rose <- predict(h4test.rose,diabetesTest)
confusionMatrix(p.h4test.rose,diabetesTest$readmittedbinary)
summary(h4test.rose)

# hyp. 4 w/ smote-sampling for class
h4test.smote <- train(readmittedbinary ~ change, 
                     trControl = ctrlsmote,
                     metric = "ROC", #using AUC to find best performing parameters
                     data = diabetesTrain, 
                     method = "glm")
p.h4test.smote <- predict(h4test.smote,diabetesTest)
confusionMatrix(p.h4test.smote,diabetesTest$readmittedbinary)
summary(h4test.smote)

# Conduct logistic regression for hypothesis 5 testing
h5test <- train(readmittedbinary ~ number_inpatient+number_outpatient+number_emergency, 
          trControl = ctrl,
          metric = "ROC", 
          data = diabetesTrain, 
          method = "glm")
p.h5test <- predict(h5test,diabetesTest)
confusionMatrix(p.h5test,diabetesTest$readmittedbinary)

# hyp. 5 w/ up-sampling for class
h5test.up <- train(readmittedbinary ~ number_inpatient+number_outpatient+number_emergency, 
                   trControl = ctrlup,
                   metric = "ROC", 
                   data = diabetesTrain, 
                   method = "glm")
p.h5test.up <- predict(h5test.up,diabetesTest)
confusionMatrix(p.h5test.up,diabetesTest$readmittedbinary)
summary(h5test.up)

# hyp. 2 w/ down-sampling for class
h5test.down <- train(readmittedbinary ~ number_inpatient+number_outpatient+number_emergency, 
                     trControl = ctrldown,
                     metric = "ROC", 
                     data = diabetesTrain, 
                     method = "glm")
p.h5test.down <- predict(h5test.down,diabetesTest)
confusionMatrix(p.h5test.down,diabetesTest$readmittedbinary)
summary(h5test.down)

# hyp. 5 w/ rose-sampling for class
h5test.rose <- train(readmittedbinary ~ number_inpatient+number_outpatient+number_emergency, 
                     trControl = ctrlrose,
                     metric = "ROC", #using AUC to find best performing parameters
                     data = diabetesTrain, 
                     method = "glm")
p.h5test.rose <- predict(h5test.rose,diabetesTest)
confusionMatrix(p.h5test.rose,diabetesTest$readmittedbinary)
summary(h5test.rose)

# hyp. 5 w/ smote-sampling for class
h5test.smote <- train(readmittedbinary ~ number_inpatient+number_outpatient+number_emergency, 
                      trControl = ctrlsmote,
                      metric = "ROC", #using AUC to find best performing parameters
                      data = diabetesTrain, 
                      method = "glm")
p.h5test.smote <- predict(h5test.smote,diabetesTest)
confusionMatrix(p.h5test.smote,diabetesTest$readmittedbinary)
summary(h5test.smote)

# Building decision tree for predicting Readmission
library(rpart)
library(rpart.plot)
#down - sampling decision tree
m.rpart.down <- train(readmittedbinary ~ ., 
                 trControl = ctrldown,
                 metric = "ROC", #using AUC to find best performing parameters
                 data = diabetesTrain, 
                 method = "rpart")
p.rpart.down <- predict(m.rpart.down,diabetesTest)
confusionMatrix(p.rpart.down,diabetesTest$readmittedbinary)
rpart.plot(m.rpart.down$finalModel,
           type=2, extra=100, clip.right.labs=FALSE,varlen = 0,gap=0)

#up- sampling decision tree
m.rpart.up <- train(readmittedbinary ~ ., 
                 trControl = ctrlup,
                 metric = "ROC", #using AUC to find best performing parameters
                 data = diabetesTrain, 
                 method = "rpart")
p.rpart.up <- predict(m.rpart.up,diabetesTest)
confusionMatrix(p.rpart.up,diabetesTest$readmittedbinary)
rpart.plot(m.rpart.up$finalModel, 
        type=2, extra=100, clip.right.labs=FALSE,varlen = 0,gap=0)

#smote- sampling decision tree
m.rpart.smote <- train(readmittedbinary ~ ., 
                    trControl = ctrlsmote,
                    metric = "ROC", #using AUC to find best performing parameters
                    data = diabetesTrain, 
                    method = "rpart")
p.rpart.smote <- predict(m.rpart.smote,diabetesTest)
confusionMatrix(p.rpart.smote,diabetesTest$readmittedbinary)

#rose- sampling decision tree
m.rpart.rose <- train(readmittedbinary ~ ., 
                    trControl = ctrlrose,
                    metric = "ROC", #using AUC to find best performing parameters
                    data = 
                    diabetesTrain, 
                    method = "rpart")
p.rpart.rose <- predict(m.part.rose,diabetesTest)
confusionMatrix(p.rpart.rose,diabetesTest$readmittedbinary)

# logistic regression model for predicting readmission
# identify significant predictors from dataset
# logistic regression model building using down sampling for even class distribution
m.logreg.down <- train(readmittedbinary ~ ., 
                 trControl = ctrldown,
                 metric = "ROC", 
                 data = diabetesTrain, 
                 method = "glm")
p.logreg.down <- predict(m.logreg.down,diabetesTest)
confusionMatrix(p.logreg.down,diabetesTest$readmittedbinary)
varImp(m.logreg.down$finalModel)

# logistic regression model building using up sampling for even class distribution
m.logreg.up <- train(readmittedbinary ~ ., 
                  trControl = ctrlup,
                  metric = "ROC", 
                  data = diabetesTrain, 
                  method = "glm")
p.logreg.up <- predict(m.logreg.up,diabetesTest)
confusionMatrix(p.logreg.up,diabetesTest$readmittedbinary)
m.logreg.up$finalModel

# logistic regression model building using rose sampling for even class distribution
m.logreg.rose <- train(readmittedbinary ~ ., 
                  trControl = ctrlrose,
                  metric = "ROC", 
                  data = diabetesTrain, 
                  method = "glm")
p.logreg.rose <- predict(m.logreg.rose,diabetesTest)
confusionMatrix(p.logreg.rose,diabetesTest$readmittedbinary)

# logistic regression model building using smote sampling for even class distribution
m.logreg.smote <- train(readmittedbinary ~ ., 
                       trControl = ctrlsmote,
                       metric = "ROC", 
                       data = diabetesTrain, 
                       method = "glm")
p.logreg.smote <- predict(m.logreg.smote,diabetesTest)
confusionMatrix(p.logreg.smote,diabetesTest$readmittedbinary)

#Random Forest
m.rf.up <- train(readmittedbinary ~ ., 
                 trControl = ctrlup,
                 metric = "ROC", 
                 data = diabetesTrain, 
                 method = "rf")
p.rf.up <- predict(m.rf.up,diabetesTest)
confusionMatrix(p.rf.up,diabetesTest$readmittedbinary)
m.rf.up$finalModel

#svm
m.svm <- train(readmittedbinary ~ ., 
                       trControl = ctrl,
                       metric = "ROC", 
                       data = diabetesTrain, 
                       method = "svmRadial")
p.logreg.down <- predict(m.logreg.down,diabetesTest)
confusionMatrix(p.logreg.down,diabetesTest$readmittedbinary)
varImp(m.logreg.down$finalModel)

# Building ROC Curve for Log Regression
predObj <- prediction(nb_prediction,diabetesTest$readmittedbinary)
# Building TPR/FPR for 

# Comparing Model Performance
rValues <- resamples(list(rpart=m.rpart, logreg=m.logreg.down))
bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity higher for DT
bwplot(rValues, metric="Spec") #Specificity higher for logreg

prediction <- predict(m.logreg.down,
                         # remove column "subscribed"
                         diabetesTest[,-ncol(diabetesTest)],
                         type='raw')
score <- prediction[c("R")]
actual_class <- diabetesTest$readmittedbinary == 'R'
pred <- prediction(score, actual_class)
perf <- performance(pred, "tpr", "fpr")

plot(perf, lwd=2, xlab="False Positive Rate (FPR)",
     ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)

## corresponding AUC score
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc


#roc curve for decision tree
pred <- predict(m.rpart.down,
                diabetesTest[,-ncol(diabetesTest)])
prediction <- as.numeric(pred)
roc(response,prediction,plot=TRUE)
response<-(as.numeric(diabetesTest$readmittedbinary))

#roc curve for logreg
pred.log <- predict(m.logreg.down,
                    diabetesTest[,-ncol(diabetesTest)])
prediction.log <- as.numeric(pred.log)
roc(response,prediction.log,auc=TRUE, plot=TRUE)
response<-(as.numeric(diabetesTest$readmittedbinary))

#roc curve for rf
pred.log <- predict(m.rf.up,
                    diabetesTest[,-ncol(diabetesTest)])
prediction.log <- as.numeric(pred.log)
roc(response,prediction.log,auc=TRUE, plot=TRUE)
response<-(as.numeric(diabetesTest$readmittedbinary))

##############################
# COMMUNICATING RESULTS  #####
##############################

# visualizing final dataset
str(filteredDiabetes2)
summary(filteredDiabetes2)

boxplot(filteredDiabetes$time_in_hospital)
barplot(table(filteredDiabetes$time_in_hospital), 
        main="Distribution of Time Patient Spent in Hospital",
        xlab="Number of Days",ylab="Number of Patients", col="purple")

filteredDiabetes2$age <- relevel(filteredDiabetes2$age, "young")

ggplot(filteredDiabetes2, aes(race, fill=age)) +
  geom_bar(colour="black")+
  scale_y_log10() +
  scale_fill_brewer(name="Age") +
  facet_wrap(~gender) +
  coord_flip() +
  ylab ("Number of Patients (Log Scale)") +
  xlab ("Race") +
  theme_classic(base_size = 18) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
ggsave("demographics.png", w=17.4, h=7.04, dpi=1000)

ggplot(filteredDiabetes2, aes(x = factor(0), y = num_medications)) + 
  geom_violin(fill="#0066CC", alpha=0.3, color="black") + 
  xlab("Patient Frequency") +
  ylab("Number of Medications Administered") +
  scale_x_discrete(label="") + 
  coord_flip() +
  theme_classic() +
  theme(legend.position="none", strip.background = element_blank())
ggsave("medicationviolin.png", w=17.4, h=7.04, dpi=1000)

ggplot(filteredDiabetes2, aes(num_medications)) + 
  geom_histogram(fill="#0066CC", alpha=0.3, color="black") + 
  ylab("Number of Patients") +
  xlab("Number of Medications Administered") +
  theme_classic(base_size = 18) +
  theme(legend.position="none", strip.background = element_blank())
ggsave("medicationhistogram.png", w=17.4, h=7.04, dpi=1000)

ggplot(filteredDiabetes2, aes(admission_type_id)) +
  geom_bar(fill="#0066CC", colour="black", alpha=0.3)+
  coord_flip() +
  ylab ("Number of Patients") +
  xlab ("Admission Type") +
  scale_x_discrete(label = c("Emergency/Trauma","Urgent Care", "Elective/Newborn")) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
ggsave("admissiontype.png", w=17.4, h=7.04, dpi=1000)

ggplot(filteredDiabetes2, aes(A1Cresult)) +
  geom_bar(fill="#0066CC", colour="black", alpha=0.3)+
  coord_flip() +
  ylab ("Number of Patients") +
  xlab ("Result") +
  theme_classic(base_size = 18) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
ggsave("A1ctest.png", w=17.4, h=7.04, dpi=1000)