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

#Packages2 <- c(
#"haven","foreign","lubridate","stringr","grammar of graphics","ggvis"
#,"rgl","htmlwidgets","lme4/nlme","car","mgcv","multcomp","vcd","glmnet","caret","shiny","R Markdown","infer","janitor","BioConductor","Knitr","Mlr","Quanteda.dictionaries","quanteda ","DT","RCrawler","Caret","Leaflet","Janitor","Text2Vec","DataScienceR","SnowballC","Magrittr"
#)
#lapply(Packages2, install.packages, character.only = TRUE)
#save.image()
#load(".RData", envir=tmp.env)
#load("X:/Users/119987/R_HC.RData")
#rm(list =  c("POS_CASH_balance","previous_application","installments_payments","bureau"
#        ,"bureau_balance","credit_card_balance"))


##############################
# 0 - set work dir
##############################

mem_used()
getwd()
setwd("X:/Users/119987/HC/")
#setwd("C:/Users/Zing/Downloads/home-credit-default-risk/")

list.files(path = "./", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

.libPaths()
.libPaths("C:/Program Files/R/R-3.6.1/library")

##################################################
# 1 - Install Package Automatically
##################################################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


##################################################
## 2 - Load Librarys at once
##
## 
##################################################
Packages <- c("readr","pryr","RODBC","dplyr","dbplyr","lubridate","ggplot2","ggvis","sqldf","tidyr","cluster","fpc","fastR","graphics","pracma","NbClust","randomForest","ggplot2","NbClust","h2o","caret","dbscan","gmm","TDA","flexclust","skmeans","akmeans","ElemStatLearn"
				,"data.table","mltools","vtreat","ggcorrplot","GGally","mice","Hmisc","e1071","missForest","mi",
				"haven","foreign","lubridate","stringr","ggvis","rgl","htmlwidgets","lme4/nlme","car","mgcv","multcomp","vcd","glmnet","caret","shiny","R Markdown","infer","janitor","BioConductor","Knitr","Mlr","Quanteda.dictionaries","quanteda ","DT","RCrawler","Caret","Leaflet","Janitor","Text2Vec","DataScienceR","SnowballC","magrittr"
				,"imputeR"
				，"tidyverse"
              ,"class","randomForest","RColorBrewer","scales","data.table","readr","plyr","sqldf","ggpcorrplot","ggplot2","cluster","HSAUR","fpc","openxlsx"
				)
#Packages <- c("tidyverse"
#              ,"class","randomForest","RColorBrewer","scales","data.table","readr","plyr","sqldf","ggpcorrplot","ggplot2","cluster","HSAUR","fpc","openxlsx")

lapply(Packages, library, character.only = TRUE)
#ipak(Packages)


##############################
# 3 import data
##############################
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

##############################
# 4 Examine data
##############################

names(application_train)

str(application_train)
hist(application_train$TARGET)
table(application_train[,c(2)]) #an imbalanced class problem


##############################
# 4.1 examine missing values by columns
##############################
sapply(data.frame(application_train), function(x) sum(is.na (x)))

#percentage of total
options(scipen = 999)
sapply(data.frame(application_train), function(x) (100*sum(is.na (x)))/length(x))

str(application_train)
sapply(data.frame(application_train), function(x) is.factor(x))




##############################################
# 5. categorical variables encoding processs
# 
#
##############################################

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

applicationdata=application_train

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




varnames_onehot_test=NULL
for (f in (names(application_test))) {

  if (
    #(class(application_test[[f]]) == 'character') 
    (is.character(application_test[[f]])=='TRUE') &
    ((length(unique(levels(as.factor(application_test[[f]]))))>2))=='TRUE'
  )  {

    varnames_onehot_test=
      paste(colnames(application_test[f])
        #paste("\"",, "\"",sep="")

            ,varnames_onehot_test,sep = ",")

  		}
}
varnames_onehot_test

for (f in (names(application_test))) {

  if (
    #(class(application_test[[f]]) == 'character') 
    (is.character(application_test[[f]])=='TRUE') &
    ((length(unique(levels(as.factor(application_test[[f]]))))>2))=='TRUE'
  )
    for(unique_value in unique(levels(as.factor(application_test[[f]])))){
      application_test[paste(colnames(application_test[f]), unique_value, sep = ".")] <- ifelse(application_test$f == unique_value, 1, 0)
    }

}


##############################################
# to do
#come back to check if method with treatment plan should be used instead of a simple loop application
#algin training and testing data strcuture 
##############################################



##############################################
# 6. Anomalies detection and treatment
#
#
##############################################

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


r <- cor(application_train, use="complete.obs")
round(r,2)

x <- data.frame(round(r,2))
sort(x[1,], decreasing = TRUE)[1:5]

ggcorrplot(sort(x[1,], decreasing = TRUE)[1:5])


application_train['DAYS_BIRTH'] = abs(application_train['DAYS_BIRTH'])
application_train['DAYS_BIRTH'].corr(application_train['TARGET'])


#a negative linear relationship with the target and client's age

cor(application_train['DAYS_BIRTH'],application_train['TARGET'], use="complete.obs")



hist(application_train$DAYS_BIRTH/365,main = 'Age of Client', xlab='Age (years)',ylab='count', bins = 25)





# add legend via mouse click
#colfill<-c(2:(2+length(levels(cyl.f))))
#legend(locator(1), levels(cyl.f), fill=colfill)


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


##############################################
# 7. Exploration
#
#
##############################################

#average failure to repay loans by age group
#create Bins Label
age_data = application_train[,c('TARGET', 'DAYS_BIRTH')]
age_data['YEARS_BIRTH'] = age_data['DAYS_BIRTH'] / 365

bins <- seq（0, 70, by=5)
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


##############################################
# 8. Feature Engineering
# 8.1. Poly/ Interaction Terms
# 8.2. Domain Knowledge Features
##############################################

poly_features <- application_train[,c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH', 'TARGET')]

poly_features_test <- application_test[,c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3', 'DAYS_BIRTH')]

poly_target <- poly_features['TARGET']

poly_features <- poly_features[,-5]


#poly_target_pre_impute_bk <- poly_target
#poly_features_pre_impute_bk <- poly_features


#poly_features.imp <- missForest(as.data.frame(poly_features))


##############################################
# 8.1. imputation using mean
##############################################
#poly_features$imputed_EXT_SOURCE_1 <- Hmisc::impute(poly_features$EXT_SOURCE_1,mean)
#poly_features$imputed_EXT_SOURCE_2 <- Hmisc::impute(poly_features$EXT_SOURCE_2,mean)
#poly_features$imputed_EXT_SOURCE_3 <- Hmisc::impute(poly_features$EXT_SOURCE_3,mean)
#poly_features$imputed_DAYS_BIRTH   <- Hmisc::impute(poly_features$DAYS_BIRTH,mean)

poly_features$EXT_SOURCE_1 <- Hmisc::impute(poly_features$EXT_SOURCE_1,mean)
poly_features$EXT_SOURCE_2 <- Hmisc::impute(poly_features$EXT_SOURCE_2,mean)
poly_features$EXT_SOURCE_3 <- Hmisc::impute(poly_features$EXT_SOURCE_3,mean)
poly_features$DAYS_BIRTH   <- Hmisc::impute(poly_features$DAYS_BIRTH,mean)

poly_features_test$EXT_SOURCE_1 <- Hmisc::impute(poly_features_test$EXT_SOURCE_1,mean)
poly_features_test$EXT_SOURCE_2 <- Hmisc::impute(poly_features_test$EXT_SOURCE_2,mean)
poly_features_test$EXT_SOURCE_3 <- Hmisc::impute(poly_features_test$EXT_SOURCE_3,mean)
poly_features_test$DAYS_BIRTH   <- Hmisc::impute(poly_features_test$DAYS_BIRTH,mean)


##########################create Poly Features#################################
#
#poly_features<- poly(as.matrix(poly_features),degree = 3, raw = FALSE, simple = FALSE)
#poly_features_test<- poly(as.matrix(poly_features_test),degree = 3, raw = FALSE, simple = FALSE)

#poly_features %>%
#  dplyr::mutate(
#    Linear    = poly(as.matrix(poly_features), degree = 3, raw = TRUE)[ ,1]
#  , Quadratic = poly(as.matrix(poly_features), degree = 3, raw = TRUE)[ ,2]  
#  , Cubic     = poly(as.matrix(poly_features), degree = 3, raw = TRUE)[ ,3]
#    )
#poly_features %>%
#  mutate(as.data.frame(poly(x =as.matrix(poly_features), degree = 3, raw = TRUE))) 
#%>%
#  setNames(c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","DAYS_BIRTH"
#             ,"Linear", "Quadratic", "Cubic"))
#poly_features$imputed_EXT_SOURCE_1 <- with(poly_features, impute(EXT_SOURCE_1, mean))
#poly_features$imputed_EXT_SOURCE_2 <- with(poly_features, impute(EXT_SOURCE_2, mean))
#poly_features$imputed_EXT_SOURCE_3 <- with(poly_features, impute(EXT_SOURCE_3, mean))
#poly_features$imputed_DAYS_BIRTH   <- with(poly_features, impute(DAYS_BIRTH, mean))

formula = as.formula(paste(' ~ .^3 + ',paste('poly(',colnames(poly_features),',2, raw=TRUE)[, 2]',collapse = ' + ')))
#as.formula(paste(' ~ A:B + ',paste('poly(',colnames(data),',2, raw=TRUE)[, 2]',collapse = ' + ')))
poly_features_2=data.frame(model.matrix(formula, data=as.data.frame(poly_features)))
poly_features_2['TARGET'] = poly_target

formula_test = as.formula(paste(' ~ .^3 + ',paste('poly(',colnames(poly_features_test),',2, raw=TRUE)[, 2]',collapse = ' + ')))
poly_features_test_2=data.frame(model.matrix(formula_test, data=as.data.frame(poly_features_test)))


poly_corrs = poly_features.corr()['TARGET'].sort_values()
poly_corrs <- cor(poly_features_2[,-1], use="complete.obs")
x <- data.frame(round(poly_corrs,2))
sort(x[1,], decreasing = TRUE)[1:10]
ggcorrplot(sort(x[1,], decreasing = TRUE)[1:10])
#
##########################create Poly Features#################################

#Merge polynomial features back to training set
poly_features_2['SK_ID_CURR'] = application_train['SK_ID_CURR']

application_train_poly<- Reduce(function (a,b) merge(a,b,all.a=TRUE, by="SK_ID_CURR"), list(application_train,poly_features_2[,-c(1,20)]) )
#application_train_poly <- sqldf("SELECT * FROM application_train LEFT JOIN poly_features_2[,-20] USING(SK_ID_CURR)")

poly_features_test_2['SK_ID_CURR'] = application_test['SK_ID_CURR']
application_test_poly<- Reduce(function (a,b) merge(a,b,all.a=TRUE, by="SK_ID_CURR"), list(application_test,poly_features_test_2[,-c(1)]) )


application_test_poly <- dplyr::bind_cols(application_test_poly, head(application_train_poly[!names(application_train_poly) %in% names(application_test_poly)],48744))

dim(application_train_poly)
dim(application_test_poly)

##############################################
# to do
# apply encoding process to test dataset as well
# make sure test data has the same strcutre as the training
##############################################

##############################################
# 8.2. create Domain Knowledge Features
##############################################
application_train_dk_features <- application_train
application_test_dk_features <-  application_test
application_train_dk_features['CREDIT_INCOME_PERCENT'] = application_train_dk_features['AMT_CREDIT'] / application_train_dk_features['AMT_INCOME_TOTAL']
application_train_dk_features['ANNUITY_INCOME_PERCENT'] = application_train_dk_features['AMT_ANNUITY'] / application_train_dk_features['AMT_INCOME_TOTAL']
application_train_dk_features['CREDIT_TERM'] = application_train_dk_features['AMT_ANNUITY'] / application_train_dk_features['AMT_CREDIT']
application_train_dk_features['DAYS_EMPLOYED_PERCENT'] = application_train_dk_features['DAYS_EMPLOYED'] / application_train_dk_features['DAYS_BIRTH']

#align the test data
application_test_dk_features ['CREDIT_INCOME_PERCENT'] = application_test_dk_features ['AMT_CREDIT'] / application_test_dk_features ['AMT_INCOME_TOTAL']
application_test_dk_features ['ANNUITY_INCOME_PERCENT'] = application_test_dk_features ['AMT_ANNUITY'] / application_test_dk_features ['AMT_INCOME_TOTAL']
application_test_dk_features ['CREDIT_TERM'] = application_test_dk_features ['AMT_ANNUITY'] / application_test_dk_features ['AMT_CREDIT']
application_test_dk_features ['DAYS_EMPLOYED_PERCENT'] = application_test_dk_features ['DAYS_EMPLOYED'] / application_test_dk_features ['DAYS_BIRTH']
