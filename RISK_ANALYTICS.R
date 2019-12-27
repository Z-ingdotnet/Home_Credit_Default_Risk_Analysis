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
				,"mltools","vtreat","ggcorrplot","GGally","mice","Hmisc","e1071","missForest","mi",
				"haven","foreign","lubridate","stringr","grammar of graphics","ggvis","rgl","htmlwidgets","lme4/nlme","car","mgcv","multcomp","vcd","glmnet","caret","shiny","R Markdown","infer","janitor","BioConductor","Knitr","Mlr","Quanteda.dictionaries","quanteda ","DT","RCrawler","Caret","Leaflet","Janitor","Text2Vec","DataScienceR","SnowballC","magrittr"
				,"imputeR"
				)
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


#poly_features.imp <- missForest(as.data.frame(poly_features))


#imputation

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











###############################################################################################
#
#
#
#
#
poly_features<- poly(as.matrix(poly_features),degree = 3, raw = FALSE, simple = FALSE)
#poly_features_test<- poly(as.matrix(poly_features_test),degree = 3, raw = FALSE, simple = FALSE)


poly_features %>%
  dplyr::mutate(
    Linear    = poly(as.matrix(poly_features), degree = 3, raw = TRUE)[ ,1]
  , Quadratic = poly(as.matrix(poly_features), degree = 3, raw = TRUE)[ ,2]  
  , Cubic     = poly(as.matrix(poly_features), degree = 3, raw = TRUE)[ ,3]
    )




formula <- as.formula(paste(' ~ A:B + ',paste('poly(',colnames(data),',2, raw=TRUE)[, 2]',collapse = ' + ')))

 model.matrix(formula, data=data)




poly_features %>%
  mutate(as.data.frame(poly(x =as.matrix(poly_features), degree = 3, raw = TRUE))) 
%>%
  setNames(c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","DAYS_BIRTH"
             ,"Linear", "Quadratic", "Cubic"))
#
#
#
#
#
###############################################################################################
