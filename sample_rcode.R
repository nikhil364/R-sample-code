library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(BSDA)
library (epiDisplay)
library (abd)
library(epiR)
library(epitools)
library (janitor)
library (tidyverse)
library (caret)
library(car)
library(lmtest)
library (mice)
library (miceadds)
library (gtsummary)
library(openxlsx)
library(readxl)
library (rcompanion)

mydata <- read_excel("/Downloads/ModelData2.xlsx")
View(mydata)

#Formatting Data 
mydata$Age <- as.numeric(gsub("[^0-9.]", "", mydata$Age))
mydata$total <- as.numeric(gsub("[^0-9.]", "", mydata$total))
mydata$Develop <- as.numeric(gsub("[^0-9.]", "", mydata$Develop))
mydata$Physical <- as.numeric(gsub("[^0-9.]", "", mydata$Physical))
mydata$Emotion <- as.numeric(gsub("[^0-9.]", "", mydata$Emotion))
mydata$Social <- as.numeric(gsub("[^0-9.]", "", mydata$Social))
mydata$TimeDepend <- as.numeric(gsub("[^0-9.]", "", mydata$TimeDepend))
mydata$Tasks <- as.numeric(gsub("[^0-9.]", "", mydata$Tasks))
mydata$Comorbid <- as.numeric(gsub("[^0-9.]", "", mydata$Comorbid))

mydata$Gender <- as.factor(mydata$Gender) 
levels(mydata$Gender) <- c("Male", "Female")

mydata$Carefreq <- factor(mydata$Carefreq, levels= c("0", "1", "2", "3", "4"), labels= c("1", "0", "0", "0", "0")) 
levels(mydata$Carefreq) <- c("More than once", "Once or less")

mydata$Liverec <- as.factor(mydata$Liverec) 
levels(mydata$Liverec) <- c("Yes", "No")

mydata$Youngchild <- as.factor(mydata$Youngchild) 
levels(mydata$Youngchild) <- c("Yes", "No")

mydata$Employ <- factor(mydata$Employ, levels= c("0", "1", "2", "3", "4", "5"), labels= c("1", "0", "0", "0", "0", "1"))
levels(mydata$Employ) <- c("Currently working full-time", "Not currently working full-time")

mydata$Fitness <- factor(mydata$Fitness, levels= c("0", "1", "2"), labels= c("0", "0", "1")) 
levels(mydata$Fitness) <- c("Low to medium", "High")

mydata$Weightbear <- as.factor(mydata$Weightbear) 
levels(mydata$Weightbear) <- c("None", "Partial")

str(mydata)

#imputations
method_list <- c("pmm", "logreg", "logreg", "logreg", "logreg", "pmm", "logreg", "logreg", "logreg", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "pmm")
tempData <- mice(mydata,m=10, method = method_list)
summary(tempData)
tempData$imp$Age
tempData$imp$Gender
tempData$imp$Carefreq
tempData$imp$Youngchild
tempData$imp$Employ
tempData$imp$Fitness
tempData$imp$Weightbear
tempData$imp$Liverec
tempData$imp$TimeDepend
tempData$imp$Develop
tempData$imp$Physical
tempData$imp$total
complete(tempData,1)

#Model 1
modelFit1 <- with(tempData,lm(total~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid))
modelFit1P <- pool(modelFit1)
summary(modelFit1P, conf.int = TRUE)
pool.r.squared(modelFit1, adjusted = TRUE)
pool.r.squared(modelFit1, adjusted = FALSE)

model1 <- lm(total~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid, data=mydata)
summary (model1, conf.int = TRUE)

tab1<-
  suppressWarnings(tempData)%>%
  with(lm(total~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid))%>%
  tbl_regression(exponentiate = FALSE, intercept=TRUE, pvalue_fun = ~ style_pvalue(.x, digits = 2), estimate_fun=~ style_sigfig(.x, digits = 3),
                 label=list(Age~"Age", Gender~"Gender", Carefreq~"Frequency provided", Youngchild~"Children <18 years of age", Employ~"Employment status", Fitness~"Fitness level before surgery", Weightbear~"Weight status", Liverec~"Lives with recipient", Tasks~"Number of tasks provided", Comorbid~"Number of comorbidities")) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(label ~ "**Predictor**")%>%
  italicize_levels() %>%
  modify_caption("Association Between the 1st Score and Characteristics")
tab1

#Model 2
modelFit2 <- with(tempData,lm(TimeDepend~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid))
modelFit2P <- pool(modelFit2)
summary(modelFit2P, conf.int = TRUE)
pool.r.squared(modelFit2, adjusted = TRUE)
pool.r.squared(modelFit2, adjusted = FALSE)

model2 <- lm(TimeDepend~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid, data=mydata)
summary (model2)

tab2<-
  suppressWarnings(tempData)%>%
  with(lm(TimeDepend~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid))%>%
  tbl_regression(exponentiate = FALSE, intercept=TRUE, pvalue_fun = ~ style_pvalue(.x, digits = 2), estimate_fun=~ style_sigfig(.x, digits = 3), 
                 label=list(Age~"Age", Gender~"Gender", Carefreq~"Frequency provided", Youngchild~"Children <18 years of age", Employ~"Employment status", Fitness~"Fitness level before surgery", Weightbear~"Weight status", Liverec~"Lives with recipient", Tasks~"Number of tasks provided", Comorbid~"Number of comorbidities")) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(label ~ "**Predictor**")%>%
  italicize_levels() %>%
  modify_caption("Association Between the 2nd Score and Characteristics")
tab2

#Model 3
modelFit3 <- with(tempData,lm(Develop~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid))
modelFit3P <- pool(modelFit3)
summary(modelFit3P, conf.int = TRUE)
pool.r.squared(modelFit3, adjusted = TRUE)
pool.r.squared(modelFit3, adjusted = FALSE)

model3 <- lm(Develop~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid, data=mydata)
summary (model3)

tab3<-
  suppressWarnings(tempData)%>%
  with(lm(Develop~ Age+Gender+Carefreq+Youngchild+Employ+Fitness+Weightbear+Liverec+Tasks+Comorbid))%>%
  tbl_regression(exponentiate = FALSE, intercept=TRUE, pvalue_fun = ~ style_pvalue(.x, digits = 2), estimate_fun=~ style_sigfig(.x, digits = 3), 
                 label=list(Age~"Age", Gender~"Gender", Carefreq~"Frequency provided", Youngchild~"Children <18 years of age", Employ~"Employment status", Fitness~"Fitness level before surgery", Weightbear~"Weight status", Liverec~"Lives with recipient", Tasks~"Number of tasks provided", Comorbid~"Number of comorbidities")) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(label ~ "**Predictor**")%>%
  italicize_levels() %>%
  modify_caption("Association Between the 3rd Score and Characteristics")
tab3


