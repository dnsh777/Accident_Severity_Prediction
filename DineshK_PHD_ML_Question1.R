rm(list=ls(all=TRUE))
setwd("C:/Users/dnsh7/Desktop/Analytics/INSOFE/PHD/ML")

#Load 
train_initial = read.csv("train_PHD.csv")
test_initial = read.csv("test_NoTarget_PHD.csv")
validation_initial = read.csv("validation_PHD.csv")

#peek at the data
head(train_initial,n = 10)

#peek at the predictors
colnames(train)

#peek at the structure
str(train_initial)
 ############################### Deal with NAs ####################################

#Column wise NAs count
NA_count = sapply(train_initial, function(x) sum(length(which(is.na(x)))))  
NA_count

#Percent NAs
percent_NA <-sapply(train_initial, function(y) ((sum(is.na(y))/NROW(y))*100 ))
percent_NA

#Total NAs
sum(is.na(train_initial))
sum(is.na(validation_initial))
sum(is.na(test_initial))

#Imputation 
library(DMwR)
train = centralImputation(train_initial)
test = centralImputation(test_initial)
validation = centralImputation(validation_initial)

# Tried to use MICE initially, but CI gives better results on accuracy.

#Check
sum(is.na(train))
sum(is.na(validation))
sum(is.na(test))

########################### Preliminary glance at the data ##########################

#dimensions of data
dim(train)
# 8562 x 17
dim(validation)
# 1835 x 17
dim(test)
# 1833 x 16

#looking at the classes of data
sapply(train,class)

#Checking for class imbalance 
# 3 - 89.11% , 2 - 9.59% , 1 - 1.28%
y = train$Collision.Severity
cbind(frequency = table(y), percentage = prop.table(table(y))*100)

PA = train$Policing.Area
cbind(frequency = table(PA), percentage = prop.table(table(PA))*100)
# Belfast City has the highest percentage of accidents at 12.90%
# Rest all have single digit percentage accidents with less than or equal to 4% accidents at each place.
# Binning them according to Counties would be a better option than binning by frequencies.

WD = train$Weekday.of.Collision
cbind(frequency = table(WD), percentage = prop.table(table(WD))*100)
# The accidents too seem to be evenly divided amongst the days of the week
# Highest % of accidents happen on Friday @ 16.80%
# Second is Thu @ 15.65%, third is Wed @ 15.35%
# Lowest is on Sunday @ 10.55%

#Day of Month - DM
DM = train$Day.of.Collision
cbind(frequency = table(DM), percentage = prop.table(table(DM))*100)
# The % of accidents seem to be evenly distributed with each day having either 2% or 3% of the total 

#Hour of the day - HD
HD = train$Hour.of.Collision..24.hour.
cbind(frequency = table(HD), percentage = prop.table(table(HD))*100)
# 8AM to 7PM seem to have the highest % of accidents with greater than 4% of accidents 
# -happening during this time. Rest all times see less than 4% of accidents at each hour.

M = train$Month.of.Collision
cbind(frequency = table(M), percentage = prop.table(table(M))*100)
# % collisions seem to be evenly distributed all around the year

CT = train$Carriageway.Type
cbind(frequency = table(CT), percentage = prop.table(table(CT))*100)
# 85.77% accidents seem to happen on a Single Carriageway

SL = train$Speed.Limit
cbind(frequency = table(SL), percentage = prop.table(table(SL))*100)
# Speeds 30 and 60 have the higest accidents at 53.24% and 31.36% respectively

JD = train$Junction.Detail
cbind(frequency = table(JD), percentage = prop.table(table(JD))*100)
# 41.81% of all accidents seem to happen within 20m of a junction
# 33.90% of all accidents happen at a T or staggered junction

# A method to dynamically change lane allocation at an interchange is called Junction Control. 
# It can be used at freeway off-ramps or on-ramps; particularly for high volume ramps often associated with major freeway-to-freeway interchanges. 
JC = train$Junction.Control
cbind(frequency = table(JC), percentage = prop.table(table(JC))*100)
# 42% of all accidents happen at Not a Junction
# 48.83% of all accidents happen at a Give way

PCHC = train$Pedestrian.Crossing...Human.Control
cbind(frequency = table(PCHC), percentage = prop.table(table(PCHC))*100)
# 99.48% of all accidents happen when there is no human control within 50m

PCPC = train$Pedestrian.Crossing...Physical.Control
cbind(frequency = table(PCPC), percentage = prop.table(table(PCPC))*100)
# 86.7% of all accidents happen when there is no physical control within 50m

LC = train$Light.Conditions
cbind(frequency = table(LC), percentage = prop.table(table(LC))*100)
# 25.57% + 23.27% + 13.86% during day light, ie, 62.7%

WC = train$Weather.Conditions
cbind(frequency = table(WC), percentage = prop.table(table(WC))*100)
Weather = train[train$Weather.Conditions %in% c('1','2','3','9','10'),]
Percentage_of_train_in_selected = (nrow(Weather)/nrow(train))*100
Percentage_of_train_in_selected
# 25% + 19% + 18% ie, 62% of the accidents happen when there are no high winds
# 37% of the accidents happen due to other or unknown factors
# The levels 1,2,3,9,10 form 99.24% of the data. Probably bin the rest into one category?

RC = train$Road.Surface.Conditions
cbind(frequency = table(RC), percentage = prop.table(table(RC))*100)
Road_Con_in_selected = train[train$Road.Surface.Conditions %in% c('1','2'),]
Percentage_Road_Con_in_selected = (nrow(Road_Con_in_selected)/nrow(train))*100
Percentage_Road_Con_in_selected
# 61.68% of the accidents happen on Dry roads
# 32.10% of the accidents happen on Wet/damp roads
# 93.95% of accidents on either Dry or Damp/Wet roads

SCon = train$Special.Conditions.at.Site
cbind(frequency = table(SCon), percentage = prop.table(table(SCon))*100)
# 98.3% of accidents happen when there are no special conditions

########################## Data Summary ###########################################

summary(train)
str(train)

# load packages
library(e1071)
# calculate skewness for each variable
skew <- apply(train[,-c(1,2,4)], 2, skewness)
# display skewness, larger/smaller deviations from 0 show more skew
print(skew)

# Correlation
train_analysis = train
str(train_analysis)
train_analysis[sapply(train_analysis,is.factor)] = lapply(train_analysis[sapply(train_analysis,is.factor)],as.numeric)
# calculate a correlation matrix for numeric variables
correlations <- cor(train_analysis[,2:17])
# display the correlation matrix
print(correlations)

#################################### Visualisations ######################################################

table(train$Collision.Severity)

library(ggplot2)

#Bar Charts

qplot(x = train$Collision.Severity,  geom = "bar", main = "Distribution of Collision Severity")
#89.12% of the data belongs to class 3, 1.28% to class 1 and 9.59% to class 2

qplot(train$Month.of.Collision,main = "Distribution of Month of the Collision")
#Seems that there isn't much of a relation between Collisions and Months of the year

plot(train$Weekday.of.Collision,main = "Distribution of weekdays for Collisions")
#Seems that there isn't much of a  relation between Collisions and weekdays

plot(train$Day.of.Collision,main = "Distribution of Day of the Collision")
#Seems that there isn't much of  relation between Collisions and Days

qplot(train$Hour.of.Collision..24.hour., main = "Distribution of Hour of the Collision", horiz = FALSE)
###The most no of accidents seem to take place at the 17th hour of the day ####

plot(train$Policing.Area,main = "Distribution of Policing areas where Collisions occur")
#BELC seems to have the most no of collisions

barplot(table(train$Road.Surface.Conditions),xlab = "Road Surface Condition")
# Most accidents seem to  happen on Dry or Wet roads

barplot(table(train$Light.Conditions),xlab = "Street Light Condition")
# Most of the accident happen during the day time.

barplot(table(train$Junction.Detail),xlab = "Junction Details")
# Most accidents occur at "Not at or within 20m of junction" and at "T point junction"

barplot(table(train$Pedestrian.Crossing...Human.Control),xlab = "PC-Human Control")
# When there is no Human control witin 50m, more accidents occur

barplot(table(train$Pedestrian.Crossing...Physical.Control),xlab = "PC-Physical Control")
#From this also we can see that when there is no Physical control within 50m, then most accidents occur

barplot(table(train$Speed.Limit),xlab = "Speed Limit")
# Speeds 30 and 60 have the higest accidents 

barplot(table(train$Special.Conditions.at.Site),xlab = "Special Condition at Site")
# Almost all accidents occur when there is no special condition at Site

##Stacked bar plots

collSeverity_vs_Carriageway <- table(train$Collision.Severity,train$Carriageway.Type)
collSeverity_vs_Carriageway
barplot(collSeverity_vs_Carriageway, main="#Collision Severity Levels in diff. Carrigeways",
        col=rainbow(7),width = 0.1,
        xlab="Carrigeways", ylab = "No of Collisions ",
        legend.text = TRUE, 
        args.legend = list(x = "topright", cex = 0.3, ncol=2))
# This stacked bar plot tells us that for carrigeway 13 the no of accidents is the maximum 

collSeverity_vs_JunctionDetail <- table(train$Collision.Severity,train$Junction.Detail)
collSeverity_vs_JunctionDetail
barplot(collSeverity_vs_JunctionDetail, main="#Collision Severity Levels at different junction types",
        col=rainbow(7),width = 0.1,
        xlab="Junction types", ylab = "No of Collisions ",
        legend.text = TRUE, 
        args.legend = list(x = "topright", cex = 0.3, ncol=2))
# 12 and 1 have the highest no of accidents  

collSeverity_vs_JunctionControl <- table(train$Collision.Severity,train$Junction.Control)
collSeverity_vs_JunctionDetail
barplot(collSeverity_vs_JunctionDetail, main="#Collision Severity Levels for diff. junction controls",
        col=rainbow(7),width = 0.1,
        xlab="Junction Control", ylab = "No of Collisions ",
        legend.text = TRUE, 
        args.legend = list(x = "topright", cex = 0.3, ncol=2))
# Junction control : 1 and 7 have the highest no of accidents  

collSeverity_vs_JunctionControl <- table(train$Collision.Severity,train$Weather.Conditions)
collSeverity_vs_JunctionControl 
barplot(collSeverity_vs_JunctionControl , main="#Collision Severity Levels for diff. Weather types",
        col=rainbow(7),width = 0.1,
        xlab="Weather types", ylab = "No of Collisions ",
        legend.text = TRUE, 
        args.legend = list(x = "topright", cex = 0.3, ncol=2))
## From this stacked bar plot we can see that for weather type 1,2,3,9,10 the no of accidents is the high

#Plot between Policing Area and Severity of collisions
ggplot(train,aes(x=train$Policing.Area,fill=train$Collision.Severity)) +
  geom_bar(width = .3) +
  xlab("Policing Area") + ylab("Collision Severity frequency")  +
  ggtitle("Policing Area with collision Severity frequency at each area")+
  theme_bw()
# Area BELC ("Belfast City") has the most no. of collisions






#################################### Data Preprocessing #############################################

#################################### Binning #########################################################
# Binning according to Counties. 
# There are 6 counties in Nothern Ireland.
library(car)
train$Policing.Area =  Recode(train$Policing.Area, 
                              "c('ANTR', 'BELE', 'BELS','BMEN', 'CFER','NABB','BMON','LARN','MOYL','ANTN','BELC','CCGL','LISC','MEAN')='ANTRIM';
                              c('ARMA', 'CRAI','ARBC') = 'ARMAGH';
                              c('FERM', 'FERO') = 'FERMANAUGH';
                              c('CREA', 'DOWN','ARDS','BANB','LISB','NDOW','ARND','NEWM','BELN','BELW','NEMD') = 'DOWN';
                              c('LIMA', 'COLE','MFEL','DCST') = 'LONDONDERRY';
                              c('STRB', 'COOK','DAST','OMAG','FOYL','MIDU') = 'TYRONE'")

validation$Policing.Area =  Recode(validation$Policing.Area, 
                                   "c('ANTR', 'BELE', 'BELS','BMEN', 'CFER','NABB','BMON','LARN','MOYL','ANTN','BELC','CCGL','LISC','MEAN')='ANTRIM';
                                   c('ARMA', 'CRAI','ARBC') = 'ARMAGH';
                                   c('FERM', 'FERO') = 'FERMANAUGH';
                                   c('CREA', 'DOWN','ARDS','BANB','LISB','NDOW','ARND','NEWM','BELN','BELW','NEMD') = 'DOWN';
                                   c('LIMA', 'COLE','MFEL','DCST') = 'LONDONDERRY';
                                   c('STRB', 'COOK','DAST','OMAG','FOYL','MIDU') = 'TYRONE'")

test$Policing.Area =  Recode(test$Policing.Area, 
                             "c('ANTR', 'BELE', 'BELS','BMEN', 'CFER','NABB','BMON','LARN','MOYL','ANTN','BELC','CCGL','LISC','MEAN')='ANTRIM';
                             c('ARMA', 'CRAI','ARBC') = 'ARMAGH';
                             c('FERM', 'FERO') = 'FERMANAUGH';
                             c('CREA', 'DOWN','ARDS','BANB','LISB','NDOW','ARND','NEWM','BELN','BELW','NEMD') = 'DOWN';
                             c('LIMA', 'COLE','MFEL','DCST') = 'LONDONDERRY';
                             c('STRB', 'COOK','DAST','OMAG','FOYL','MIDU') = 'TYRONE'")



############################# Smoting #################################################################
library(DMwR)
str(train)

trainSmote = train
trainSmote$Collision.Severity = as.numeric(trainSmote$Collision.Severity)
smote_train_1_3 = trainSmote[trainSmote$Collision.Severity != 2 ,]
smote_train_2_3 = trainSmote[trainSmote$Collision.Severity != 1 ,]
smote_train_2_3$Collision.Severity = as.factor(as.character(smote_train_2_3$Collision.Severity))
smote_train_1_3$Collision.Severity = as.factor(as.character(smote_train_1_3$Collision.Severity))
table(smote_train_1_3$Collision.Severity)
table(smote_train_2_3$Collision.Severity)

library(DMwR)
balance_1_3 = SMOTE(Collision.Severity~.,data = smote_train_1_3 ,perc.over = 2000,perc.under = 300)
table(balance_1_3$Collision.Severity)
balance_2_3 = SMOTE(Collision.Severity~.,data = smote_train_2_3 ,perc.over = 300,perc.under = 150)
table(balance_2_3$Collision.Severity)

# SMOTing with classes 1,3 and 2,3 for better balancing the classes 
# We get class 1 : 2310, class 2 : 3284, class 3 : 8815
#This is inline with the intial proportion of classes with minority classes oversampled

merge_balanced = merge(x = balance_2_3, y = balance_1_3, all = TRUE)
table(merge_balanced$Collision.Severity)

train1 = merge_balanced
dim(train1)
#14409 x 17 

########################### Model Building ###################################

#################################  SVM  #######################################

train_svm = train1[-1]
validation_svm = validation[-1]
test_svm = test


#converting all variables into numeric 
train_svm_fin <- data.frame(lapply(train_svm, function(x) as.numeric(x)))
str(train_svm_fin)

#converting all variables into numeric 
valid_svm_fin <- data.frame(lapply(validation_svm, function(x) as.numeric(x)))
str(train_svm_fin)
valid_svm_fin$Collision.Severity = as.factor(valid_svm_fin$Collision.Severity)

## Seperating the target var from the rest (train)
x = subset(train_svm_fin,select = -Collision.Severity)
y = as.factor(train_svm_fin$Collision.Severity)
str(y)

## Running SVM on test
library(e1071)
library(caret)
tuned <- tune.svm(Collision.Severity ~., data = train_svm_fin, gamma = 10^(-6:-1), cost = 10^(1:2))
model_linear = svm(x,y,method = 'C-classification', kernel = 'polynomial', cost = 10, gamma = 1, degree = 3)
#summary(model_linear)
predict_train = predict(model_linear)
table(predict_train,train_svm$Collision.Severity)
confusionMatrix(table(predict_train,train_svm$Collision.Severity))

str(x)
predictions = predict(model_linear, valid_svm_fin[-2])
confusionMatrix(table(predictions,valid_svm_fin$Collision.Severity))
?svm

# SVM - after trying radial kernel with different parameters, still couldn't get a good recall
# same with Polynomial Kernal. SVM is not the solution for this problem. 

############################## Linear Discriminant Analysis ###################################

library(MASS)
str(train1)
train_lda = train1[-1]
validation_lda = validation[-1]

# Running the lda model
model_lda <- lda(formula = Collision.Severity ~ ., 
         data = train_lda, 
         prior = c(1,1,1)/3)

#Looking at the components of LDA model
model_lda$counts
prop = model_lda$svd^2/sum(model_lda$svd^2)
prop

#Predicting on train set
plda = predict(object = r, # predictions
               newdata = train_lda)
x = plda$class
table(x)
confusionMatrix(table(x,train_lda$Collision.Severity)) 

#Predicting on validation set
plda_valid = predict(object = r, # predictions
                     newdata = validation_lda)
lda_tab  = plda_valid$class
table(lda_tab)
table(validation_lda$Collision.Severity)

lda_tab = factor(lda_tab,levels(lda_tab)[c(3,1,2)])
confusionMatrix(table(lda_tab ,validation_lda$Collision.Severity)) 
#Accuracy 58.68%, sensitivity = 60%

table(plda_valid)
test_lda = test[-1]
plda_test = predict(object = r, # predictions
                    newdata = test_lda)
result = data.frame(test$Collision.Reference.No. ,as.numeric(plda_test$class))
colnames(result) = c('Collision Reference No.',"Collision Severity")
write.csv(result, file = "C:/Users/dnsh7/Desktop/Analytics/INSOFE/PHD/lda_test_pred.csv", row.names = FALSE)

#################################### Decision Trees #################################

#Decision Trees using C5.0 (For Classification Problem)

train_dt = train1[-1]
str(train_dt)

library(C50) 
dtC50 = C5.0(Collision.Severity ~ ., data = train_dt, rules=TRUE) 
summary(dtC50) 
C5imp(dtC50, metric = 'usage', pct=TRUE)
prd_dt = predict(dtC50, newdata=train_dt, type="class")
table(prd_dt)
prd_dt = factor(prd_dt,levels(prd_dt)[c(3,1,2)])
table(train_dt$Collision.Severity)
train_dt$Collision.Severity = factor(train_dt$Collision.Severity,levels(train_dt$Collision.Severity)[c(3,1,2)])

library(caret)
confusionMatrix(table(prd_dt,train_dt$Collision.Severity))

C5imp(dtC50, metric = "usage", pct = TRUE)

# Sensitivity = 92.60%, Acc = 94.09%

#Validation set

validation_dt = validation[-1]
str(validation_dt)
prd_dt_valid = predict(dtC50, newdata=validation_dt, type="class")
table(prd_dt_valid)
prd_dt_valid = factor(prd_dt_valid,levels(prd_dt)[c(3,1,2)])
table(validation_dt$Collision.Severity)
library(caret)
confusionMatrix(table(prd_dt_valid,validation_dt$Collision.Severity))

# Recall - 0 on class 1, no point of continuing.

########################## XGBoost #############################################

# xgboost for Stacking

library(xgboost)

train_xg_data = train1
train_xg_data[sapply(train_xg_data, is.factor)] = lapply(train_xg_data[sapply(train_xg_data, is.factor)], as.numeric)
str(train_xg_data)
train_xg_data$Collision.Severity = train_xg_data$Collision.Severity-1
# to change the index of Collision.Severity from 1,2,3 to 0,1,2

train_xg = xgboost(data = as.matrix(train_xg_data[-2]), 
                   label = train_xg_data$Collision.Severity,
                   booster = "gbtree",
                   max_depth= 6,
                   num_class = 3,
                   objective = "multi:softmax",
                   nrounds = 50)

xgb_pred = predict(train_xg, newdata = as.matrix(train_xg_data[-2]))
confusionMatrix(table(xgb_pred,train_xg_data$Collision.Severity)) 
train_xg
gb_importance <- xgb.importance(feature_names = colnames(as.matrix(train_xg_data)), model = train_xg)
xgb.plot.importance(importance_matrix = xgb_importance)

#Predicting on Validation Set
validation_xg_data = validation
validation_xg_data[sapply(validation_xg_data,is.factor)] = lapply(validation_xg_data[sapply(validation_xg_data,is.factor)], as.numeric)
str(validation_xg_data)
validation_xg_data = validation_xg_data[-1]
table(validation$Collision.Severity)
validation_xg_data$Collision.Severity = validation_xg_data$Collision.Severity-1
validation_xg_pred = predict(train_xg, newdata = as.matrix(validation_xg_data[-2]))
confusionMatrix(table(validation_xg_pred,validation_xg_data$Collision.Severity))

xgb_importance <- xgb.importance(feature_names = colnames(as.matrix(train_xg_data)), model = train_xg)
xgb.plot.importance(importance_matrix = xgb_importance)
test[sapply(test,is.factor)] = lapply(test[sapply(test,is.factor)], as.numeric)

XG.pred.Test = predict(train_xg, as.matrix(test[-1]), prob = TRUE)
result = data.frame(test$Collision.Reference.No.,as.numeric(XG.pred.Test))
colnames(result) = c('Collision Reference No.',"Collision Severity")
write.csv(result, file = "C:/Users/dnsh7/Desktop/Analytics/INSOFE/PHD/Sub_PHD_5.csv", row.names = FALSE)


# xgboost is giving 8% accuracy.

########################### Trying A Different Approach  #######################################
# Here I am trying to extract the important features of the dataset using Random 
# -Forest's VarImp function and running a glm on that dataset. 

#RandomForest

train_rf = train1[-1]
test_rf = test[-1]
validation_rf = validation[-1]
library(randomForest)
model_rf <- randomForest(Collision.Severity ~ ., data=train_rf, keep.forest=TRUE, ntree=30 , nodesize = 5,proximity=T)
model_rf$predicted
model_rf$importance

varImp(model_rf)
varImpPlot(model_rf,type=2)
confusionMatrix(table(model_rf$predicted,train_rf$Collision.Severity))
str(Train)
str(val)
levels(validation_rf$Speed.Limit) <- levels(train_rf$Speed.Limit)
levels(validation_rf$Weather.Conditions) <- levels(train_rf$Weather.Conditions)
levels(validation_rf$Road.Surface.Conditions) <- levels(train_rf$Road.Surface.Conditions)

pred_val_rf <- predict(model_rf, validation_rf[-2], type="response")
table(pred_val_rf)
table(validation_rf$Collision.Severity)
pred_val_rf = factor(pred_val_rf,levels(pred_val_rf)[c(3,1,2)])
confusionMatrix(table(pred_val_rf,validation_rf$Collision.Severity))

# Recall - 0 on class 1

levels(test$Speed.Limit) <- levels(train_rf$Speed.Limit)
levels(test$Weather.Conditions) <- levels(Train$Weather.Conditions)
levels(test$Road.Surface.Conditions) <- levels(Train$Road.Surface.Conditions)

pred_rf <- predict(train_rf, test[-1], type="response")
table(pred_rf)

# Running GLM on new train with important variables
train_NB_imp = train1[-1]
train_fin = subset(train_NB_imp, select=-c(Special.Conditions.at.Site,Pedestrian.Crossing...Human.Control))
str(train_fin)

require(foreign)
require(nnet)
require(caret)
model_glm <- multinom(train_fin$Collision.Severity ~ . , data = train_fin)
pred_glm <- predict(model_glm, newdata = train_fin)
pred_train_glm <- table(pred_glm,train_fin$Collision.Severity)
confusionMatrix(pred_train_glm)

validation_glm_imp = validation[-1]
valid_fin= subset(validation_glm_imp, select=-c(Special.Conditions.at.Site,Pedestrian.Crossing...Human.Control))
str(valid_fin)
pred_glm_val <- predict(model_glm, newdata = valid_fin)
table(pred_glm_val)
pred_glm_val= factor(pred_glm_val,levels(pred_glm_val)[c(3,1,2)])

pred_valid_glm <- table(pred_glm_val,valid_fin$Collision.Severity)
confusionMatrix(pred_valid_glm)
#Low accuracy still.

########################Ordered Logistic Regression###############################

ordered_probit = polr(train_fin$Collision.Severity ~ . , data = train_fin, 
                      method = c("logistic", "probit", "loglog", "cloglog", "cauchit"))

pred_op <- predict(ordered_probit, newdata = train_fin)


pred_train_op <- table(pred_op,train_fin$Collision.Severity)

confusionMatrix(pred_train_op)

#Low Recall.

########################## Models for Stacking below ###########################
###############################################################################
#Tree Bayes
train_TB = train1[-1]
str(train_TB)
validation_TB = validation[-1]
str(validation_TB)
test_TB = test[-1]
str(test_TB)

set.seed(114)
train_TB[sapply(train_TB,is.numeric)] = lapply(train_TB[sapply(train_TB,is.numeric)],as.factor)
validation_TB[sapply(validation_TB,is.numeric)] = lapply(validation_TB[sapply(validation_TB,is.numeric)],as.factor)
test_TB[sapply(test_TB, is.numeric)] = lapply(test_TB[sapply(test_TB, is.numeric)], as.factor)

library(bnlearn)
TB.fit = tree.bayes(train_TB, 'Collision.Severity')   
TB.pred = predict(TB.fit, train_TB, prob=TRUE)

c = table(TB.pred, train1$Collision.Severity)
confusionMatrix(c)

str(validation_TB)
TB.pred.Val = predict(TB.fit, validation_TB, prob = TRUE)
d = table(TB.pred.Val, validation_TB$Collision.Severity)
confusionMatrix(d, mode = "everything")

test_TB$Collision.Severity = as.factor(as.character(train_TB[1:1833,2]))
str(test_TB)

TB.pred.Test = predict(TB.fit, test_TB, prob = TRUE)
table(TB.pred.Test) 
#result = data.frame(test$Collision.Reference.No. ,as.numeric(TB.pred.Test))

####################################################################################################
require(foreign)
require(nnet)
require(caret)

train_glm = train1[-1]
validation_glm = validation[-1]
test_glm = test[-1]

#table(train$Collision.Severity)
#table(validation$Collision.Severity)
model_glm <- multinom(train_glm$Collision.Severity ~ . , data = train_glm)

summary(train_glm)

pred_glm_train <- predict(model_glm, newdata = train_glm)
confusionMatrix(table(pred_glm_train,train1$Collision.Severity))


pred_glm_valid <- predict(model_glm, newdata = validation_glm)
pred_glm_valid <- relevel(pred_glm_valid, ref = 3)
confusionMatrix(table(pred_glm_valid,validation_glm$Collision.Severity))

pred_glm_test <- predict(model_glm, newdata = test_glm)


############################################################################################################

set.seed(114)
library(bnlearn)


# Prediction    
train_nb = train1[-1]
validation_nb = validation[-1]
test_nb = test[-1]

train_nb[sapply(train_nb,is.numeric)] = lapply(train_nb[sapply(train_nb,is.numeric)],as.factor)
validation_nb[sapply(validation_nb,is.numeric)] = lapply(validation_nb[sapply(validation_nb,is.numeric)],as.factor)
test_nb[sapply(test_nb,is.numeric)] = lapply(test_nb[sapply(test_nb,is.numeric)],as.factor)

str(train_nb)
NB.fit = naive.bayes(train_nb, 'Collision.Severity')   
NB.pred = predict(NB.fit, train_nb, prob=TRUE)
c = table(NB.pred, train1$Collision.Severity)
confusionMatrix(c)
str(train_nb)
str(validation_nb)
NB.pred.Val = predict(NB.fit, validation_nb, prob = TRUE)
d = table(NB.pred.Val, validation$Collision.Severity)
confusionMatrix(d, mode = "everything")

test_nb$Collision.Severity = as.factor(as.character(train[1:1833,3]))
str(test_nb)
NB.pred.Test = predict(NB.fit, test_nb, prob = TRUE)
table(NB.pred.Test)
#####################################################################################################

############################ STACKING on Validation ########################################

valid1 = NB.pred.Val
valid2 = pred_glm_valid
valid3 = TB.pred.Val

pred_val_stack  = cbind(valid1,valid2,valid3)
pred_val_stack = data.frame(pred_val_stack)

setnames(pred_val_stack,c("tb","glm","nb"))

Mode <- function(x) {
  ux<- unique(x)
  ux[which.max(tabulate(match(x, ux)))] }

pred_val_fin <- apply(pred_val_stack,1,function(x){Mode(x)})

confusionMatrix(table(pred_val_fin, validation$Collision.Severity))

# Getting a Recall of 66.67% on class 1, 96.73% on Class 3
# Accuracy of this model : 89.16%


######################### STACKING on Test ###########################################
pred1 = data.frame(NB.pred.Test)
pred2 = data.frame(pred_glm_test )
pred3 = data.frame(TB.pred.Test)
library(data.table)

pred <- cbind(pred1,pred2,pred3)

setnames(pred,c("tb","glm","nb"))

Mode <- function(x) {
  ux<- unique(x)
  ux[which.max(tabulate(match(x, ux)))] }

pred_stack <- data.frame(apply(pred,1,function(x){Mode(x)}))


submission <- cbind(test$Collision.Reference.No.,pred_stack)
#Writing the submissions file 
write.csv(submission, file = "C:/Users/dnsh7/Desktop/Analytics/INSOFE/PHD/submission_stack.csv",row.names = FALSE)

# Returned an Accuracy of 90.185% on submission on Kaggle.
