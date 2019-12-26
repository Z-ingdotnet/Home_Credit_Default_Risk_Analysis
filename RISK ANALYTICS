#library(bannerCommenter)
#banner("Title:", "R code","Author: Iverson ZHOU","Date: 2019-12-23","Version 1","Topic: Risk Analytics with R on Bank Data (initial exploration)"
#       ,"Dataset: Bank Home Credit Data", emph = TRUE)

############################################################################
############################################################################
###                                                                      ###
###                                TITLE:                                ###
###                                R CODE                                ###
###                         AUTHOR: IVERSON ZHOU                         ###
###                           DATE: 2019-12-23                           ###
###                              VERSION 1                               ###
###   TOPIC: RISK ANALYTICS WITH R on BANK DATA (INITIAL EXPLORATION)    ###
###                    DATASET: BANK HOME CREDIT DATA                    ###
###                                                                      ###
############################################################################
############################################################################


#save.image()
#load(".RData", envir=tmp.env)

#set work dir
mem_used()
getwd()
setwd("X:/Users/119987/HC/")

list.files(path = "./", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

.libPaths()
.libPaths("C:/Program Files/R/R-3.6.1/library")

#library
Packages <- c("readr","pryr","RODBC","dplyr","dbplyr","lubridate","ggplot2","ggvis","sqldf","tidyr","cluster","fpc","fastR","graphics","pracma","NbClust","randomForest","ggplot2","NbClust","h2o","caret","dbscan","gmm","TDA","flexclust","skmeans","akmeans","ElemStatLearn"
				,"mltools","vtreat","ggcorrplot","GGally","mice","Hmisc","le1071","missForest","mi")
lapply(Packages, library, character.only = TRUE)



# import data
application_train <- read_csv("./application_train.csv")
head(application_train,10)

application_test = read_csv("./application_test.csv")
head(application_test,10)



#bureau <- read_csv("./bureau.csv")
#bureau_balance <- read_csv("./bureau_balance.csv")
#credit_card_balance <- read_csv("./credit_card_balance.csv")
#HomeCredit_columns_description <- read_csv("./HomeCredit_columns_description.csv")
#installments_payments <- read_csv("./installments_payments.csv")
#POS_CASH_balance <- read_csv("./POS_CASH_balance.csv")
#previous_application <- read_csv("./previous_application.csv")

#save.image("X:/Users/119987/R_HC.RData")

#create a backup
#application_train_bk<- application_train
#application_test_bk<- application_test

names(application_train)

str(application_train)
hist(application_train$TARGET)
table(application_train[,c(2)]) #an imbalanced class problem


#examine missing values by columns
sapply(data.frame(application_train), function(x) sum(is.na (x)))


#percentage of total
options(scipen = 999)
sapply(data.frame(application_train), function(x) (100*sum(is.na (x)))/length(x))

str(application_train)
sapply(data.frame(application_train), function(x) is.factor(x))



#categorical variables encoding processs

##st_count=1
##for (f in st_count :length(names(application_train))) {
##  
##  if (
##         (class(application_train[[f]]) == 'character') 
##        )  {
##        
##        levels <- sort(unique(c(application_train[[f]], application_test[[f]])))
##        application_train[[f]] <- as.integer(factor(application_train[[f]], levels = levels))
##        application_test[[f]] <- as.integer(factor(application_test[[f]], levels = levels))
##      }
##}


encoding_count=0
#label encoding  (just found out there are packages that have butit in label encoding function e.g. caret)
for (f in (names(application_train))) {

  if (
  		 #(class(application_train[[f]]) == 'character') 
      (is.character(application_train[[f]])=='TRUE') &
      ((length(unique(levels(as.factor(application_train[[f]]))))<=2))=='TRUE'
        )  {
        
        levels <- sort(unique(c(application_train[[f]], application_test[[f]])))
        application_train[[f]] <- as.integer(factor(application_train[[f]], levels = levels))
        application_test[[f]] <- as.integer(factor(application_test[[f]], levels = levels))
        encoding_count=encoding_count +1
      }
}



# one-hot encoding of categorical variables
#create treatment plan

varnames_onehot=NULL
for (f in (names(application_train))) {
  
  if (
    #(class(application_train[[f]]) == 'character') 
    (is.character(application_train[[f]])=='TRUE') &
    ((length(unique(levels(as.factor(application_train[[f]]))))>2))=='TRUE'
  )  {
    
    varnames_onehot=
      paste(colnames(application_train[f])
        #paste("\"",, "\"",sep="")
            
            ,varnames_onehot,sep = ",")
      
  		}
}
varnames_onehot

# only want to create the catP variables

#vartypes <- c(#'clean', 'isBAD', 'poolN', 
#				'catN')

#treatplan <- designTreatmentsZ(application_train
#                               ,c('WALLSMATERIAL_MODE','HOUSETYPE_MODE','FONDKAPREMONT_MODE','ORGANIZATION_TYPE','WEEKDAY_APPR_PROCESS_START','OCCUPATION_TYPE','NAME_HOUSING_TYPE','NAME_FAMILY_STATUS','NAME_EDUCATION_TYPE','NAME_INCOME_TYPE','NAME_TYPE_SUITE','CODE_GENDER')
#                               #,codeRestriction = vartypes
#)

##treatplan <- designTreatmentsN(application_train, 
##                               varlist = c('WALLSMATERIAL_MODE','HOUSETYPE_MODE','FONDKAPREMONT_MODE','ORGANIZATION_TYPE','WEEKDAY_APPR_PROCESS_START','OCCUPATION_TYPE','NAME_HOUSING_TYPE','NAME_FAMILY_STATUS','NAME_EDUCATION_TYPE','NAME_INCOME_TYPE','NAME_TYPE_SUITE','CODE_GENDER'), 
##                               #outcomename = 'activity',
##                               codeRestriction = vartypes,
##                               customCoders = customCoders, 
##                               verbose=FALSE)

#scoreFrame <- treatplan %>% 
#  magrittr::use_series(scoreFrame) %>% 
#  select(varName, origName, code)


#scoreFrame2 = treatplan$scoreFrame
#scoreFrame2 %>% select(varName, sig, origName, code)
#newvarlist <- scoreFrame %>% 
#  filter(code %in% c("catP" #,"clean", "lev"
#  )) %>%
#    magrittr::use_series(varName)

#data.treat <- prepare(treatplan, data, varRestrictions = newvarlist)



for (f in (names(application_train))) {
  
  if (
    #(class(application_train[[f]]) == 'character') 
    (is.character(application_train[[f]])=='TRUE') &
    ((length(unique(levels(as.factor(application_train[[f]]))))>2))=='TRUE'
  )
    for(unique_value in unique(levels(as.factor(application_train[[f]])))){
      application_train[paste(colnames(application_train[f]), unique_value, sep = ".")] <- ifelse(application_train$f == unique_value, 1, 0)
    }
  
}


# to do
#algin training and testing data strcuture 




#Anomalies detection
summary(application_train['DAYS_EMPLOYED'])
hist(application_train$DAYS_EMPLOYED)


Anomalies<-application_train%>% 
  filter(DAYS_EMPLOYED>=300000)
  
NonAnomalies<-application_train%>% 
  filter(DAYS_EMPLOYED<300000)

#The non-anomalies default of loans% 
mean(NonAnomalies[['TARGET']])

#The anomalies default of loans% 
mean(Anomalies[['TARGET']])

#anomalous days of employment
length(Anomalies$TARGET)




# Create an anomalous flag column
application_train['DAYS_EMPLOYED_ANOM'] <- application_train["DAYS_EMPLOYED"] >=300000

# Replace the anomalous values with na
application_train$DAYS_EMPLOYED[application_train$DAYS_EMPLOYED== 365243] <- NA

hist(application_train$DAYS_EMPLOYED,main = 'Days Employment Histogram', xlab='Days Employment')


#apply same with test data
application_test['DAYS_EMPLOYED_ANOM'] <- application_test["DAYS_EMPLOYED"] >=300000

application_test$DAYS_EMPLOYED[application_test$DAYS_EMPLOYED>=300000] <- NA


#process
#treat categorical encoding, missing values and the outliers
r=cor(application_train$TARGET,application_train[sapply(application_train, function(x) is.numeric(x))], use="pairwise.complete.obs")


r <- cor(df, use="complete.obs")
round(r,2)

x <- data.frame(round(r,2))
sort(x[1,], decreasing = TRUE)[1:5]

ggcorrplot(sort(x[1,], decreasing = TRUE)[1:5])


application_train['DAYS_BIRTH'] = abs(application_train['DAYS_BIRTH'])
application_train['DAYS_BIRTH'].corr(application_train['TARGET'])


#a negative linear relationship with the target and client's age

cor(application_train['DAYS_BIRTH'],application_train['TARGET'], use="complete.obs")



hist(application_train$DAYS_BIRTH/365,main = 'Age of Client', xlab='Age (years)',ylab='count', bins = 25)




sm.density.compare(pplication_train$DAYS_BIRTH, target, xlab="Miles Per Gallon")
title(main="Distribution of Ages'")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfill)


#curve skews towards the younger end of the range, days_birth variable useful with negative corrolation
density_plot<-density(application_train$DAYS_BIRTH[application_train$TARGET==1] / 365, bw = "nrd0", adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
density_plot<-density(application_train$DAYS_BIRTH[application_train$TARGET==0] / 365, bw = "nrd0", adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
plot(density_plot)


#average failure to repay loans by age group
#create Bins Label
age_data = application_train[,c('TARGET', 'DAYS_BIRTH')]
age_data['YEARS_BIRTH'] = age_data['DAYS_BIRTH'] / 365

bins <- seq0, 70, by=5)
labels <- bins
rangelabels <- paste(head(labels,-1), tail(labels,-1), sep="-")
age_data['YEARS_BINNED'] <- cut(age_data$YEARS_BIRTH, bins, rangelabels)

age_data_group <- age_data %>%
  group_by(YEARS_BINNED) %>%
  summarise_all("mean")

#younger applicants are more likely to not repay the loan
barplot(age_data_group$TARGET *100~age_data_group$YEARS_BINNED,xlab='Age Group (years)',ylab='Failure to Repay (%)')


#variables with strongest corrolation
cor(application_train['EXT_SOURCE_1'],application_train['TARGET'], use="complete.obs")
cor(application_train['EXT_SOURCE_2'],application_train['TARGET'], use="complete.obs")
cor(application_train['EXT_SOURCE_3'],application_train['TARGET'], use="complete.obs")


#All three EXT_SOURCE features are negatively correlated with the dependent variable
EXT_SOURCE_data = application_train[,c('TARGET', 'EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH')]
EXT_SOURCE_data_corrs = cor(EXT_SOURCE_data, use="complete.obs")



density_plot<-density(na.omit(application_train$EXT_SOURCE_1[application_train$TARGET==1]) / 365, bw = "nrd0", adjust = 1,main='target == 0',
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )

plot(density_plot)
density_plot<-density(na.omit(application_train$EXT_SOURCE_1[application_train$TARGET==0]) / 365, bw = "nrd0", adjust = 1,main='target == 0',
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
plot(density_plot)
density_plot<-density(na.omit(application_train$EXT_SOURCE_2[application_train$TARGET==1]) / 365, bw = "nrd0", adjust = 1,main='target == 1',
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
plot(density_plot)

density_plot<-density(na.omit(application_train$EXT_SOURCE_2[application_train$TARGET==0]) / 365, bw = "nrd0", adjust = 1,main='target == 0',
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
plot(density_plot)
density_plot<-density(na.omit(application_train$EXT_SOURCE_3[application_train$TARGET==1]) / 365, bw = "nrd0", adjust = 1,main='target == 1',
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
plot(density_plot)

density_plot<-density(na.omit(application_train$EXT_SOURCE_3[application_train$TARGET==0]) / 365, bw = "nrd0", adjust = 1,main='target == 0',
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        )
plot(density_plot)


ggpairs(EXT_SOURCE_data) 



#Feature Engineering
poly_features <- application_train[,c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH', 'TARGET')]

poly_features_test <- application_test[,c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH')]

poly_target <- poly_features['TARGET']
poly_features <- poly_features[,-5]


poly_target_pre_impute_bk <- poly_target
poly_features_pre_impute_bk <- poly_features


poly_features.imp <- missForest(as.data.frame(poly_features))


#poly_features$imputed_EXT_SOURCE_1 <- with(poly_features, impute(EXT_SOURCE_1, mean))
#poly_features$imputed_EXT_SOURCE_2 <- with(poly_features, impute(EXT_SOURCE_2, mean))
#poly_features$imputed_EXT_SOURCE_3 <- with(poly_features, impute(EXT_SOURCE_3, mean))
#poly_features$imputed_DAYS_BIRTH   <- with(poly_features, impute(DAYS_BIRTH, mean))


poly_features<- poly(poly_features,degree = 3, raw = FALSE, simple = FALSE)
poly_features_test<- poly(poly_features_test,degree = 3, raw = FALSE, simple = FALSE)





