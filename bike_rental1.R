# Project on bike rental count

# Remove all objects from R
rm(list = ls())

# Set working directory
setwd("C:/Users/Puja/Documents/bike rental")
getwd()

#Read the file
bike = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))
#bike_train=bike
head(credit,1)


# import required libraries

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies')
lapply(x, require, character.only = TRUE)
rm(x)
#install.packages("randomForest")
#install.packages("corrgram")
#install.packages("DataCombine")
#install.packages("fastDummies")

#Exploratory Data Analysis

# Analyze variables  by visualize

# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(bike)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey")+
    geom_density(aes(x=num_x,y=..density..))
  
}


# analyze the distribution of target variable 'cnt'
univariate_numeric(bike$cnt)

# analyse the distrubution of independence variable 'temp'
univariate_numeric(bike$temp)

# analyse the distrubution of  independence variable 'atemp'
univariate_numeric(bike$atemp)

# analyse the distrubution of independence variable 'hum'
univariate_numeric(bike$hum)

# analyse the distrubution of independence variable 'windspeed'
univariate_numeric(bike$windspeed)

# analyse the distrubution of independence variable 'casual'
univariate_numeric(bike$casual)

# analyse the distrubution of independence variable 'casual'
univariate_numeric(bike$registered)


str(bike)

bike$season=as.factor(bike$season)
bike$mnth=as.factor(bike$mnth)
bike$yr=as.factor(bike$yr)
bike$holiday=as.factor(bike$holiday)
bike$weekday=as.factor(bike$weekday)
bike$workingday=as.factor(bike$workingday)
bike$weathersit=as.factor(bike$weathersit)
bike=subset(bike,select = -c(instant,casual,registered))
d1=unique(bike$dteday)
df=data.frame(d1)
bike$dteday=as.Date(df$d1,format="%Y-%m-%d")
df$d1=as.Date(df$d1,format="%Y-%m-%d")
bike$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")
bike$dteday=as.factor(bike$dteday)

str(bike)


# Missing Values Analysis

missing_val = data.frame(apply(bike,2,function(x){sum(is.na(x))}))


# Outlier Analysis

# Outlier Checking using boxplots
numeric_index = sapply(bike,is.numeric) #selecting only numeric

numeric_data = bike[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,ncol=2)

# Feature Selection

# Correlation Plot 
library(corrgram)
corrgram(bike[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# Dimension Reduction

bike = subset(bike,select = -c(atemp))


#Model Development

library(DataCombine)
rmExcept("bike")
train_index = sample(1:nrow(bike), 0.8 * nrow(bike))
train = bike[train_index,]
test = bike[-train_index,]

# Decision tree 
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-12])

# Random Forest Model
library(randomForest)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-12])
plot(RF_model)


#Linear Regression

#converting multilevel categorical variable into binary dummy variable
cnames= c("dteday","season","mnth","weekday","weathersit")
data_lr=bike[,cnames]
cnt=data.frame(bike$cnt)
names(cnt)[1]="cnt"
library(fastDummies)
data_lr <- fastDummies::dummy_cols(data_lr)
data_lr= subset(data_lr,select = -c(dteday,season,mnth,weekday,weathersit))
d3 = cbind(data_lr,bike)
d3= subset(d3,select = -c(dteday,season,mnth,weekday,weathersit,cnt))
data_lr=cbind(d3,cnt)


#dividind data into test and train
train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))
train_lr = data_lr[train_index,]
test_lr = data_lr[-train_index,]

#Linear regression model making
lm_model = lm(cnt ~., data = train_lr)
predictions_LR = predict(lm_model,test_lr[,-64])
plot(lm_model)

summary(lm_model)


#Evaluating MAPE value


MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
MAPE(test[,12], predictions_DT)

MAPE(test[,12], predictions_RF)

MAPE(test_lr[,64],  predictions_LR)

##########extacting predicted values output from Random forest model######################
results <- data.frame(test, pred_cnt = predictions_RF)

write.csv(results, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)

############################################################################################

