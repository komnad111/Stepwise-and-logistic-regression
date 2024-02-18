library(haven)
library(dplyr)
library(ggformula)
library(fastDummies)
library(caret)

#Question 1.
# Import data
data1 <- read_spss("C:\\Users\\x1 Carbon\\Downloads\\Mid data\\Walmart_data.sav")
attach(data1)
# Pre processing of Data
sum(is.na(data1))
duplicated(data1)
# By graphically
gf_histogram(~Store,data = data1)
count(data1,Store)
gf_histogram(~Weekly_Sales,data = data1)
gf_histogram(~Holiday_Flag,data = data1)
count(data1,Holiday_Flag)
gf_histogram(~Temperature,data = data1)
gf_histogram(~Fuel_Price,data = data1)
gf_histogram(~Unemployment,data = data1)

# Remove Name variable 
dataa1<-select(data1, - Date)

g_dataa1 <- mutate(dataa1,Weekly_Sales=ifelse(Weekly_Sales>3e+06,3e+06,Weekly_Sales))
gf_histogram(~Weekly_Sales,data=g_dataa1)

gg_dataa1 <- mutate(g_dataa1,Temperature=ifelse(Temperature < 15,15,Temperature))
gf_histogram(~Temperature,data=gg_dataa1)

d_data <- dummy_cols(gg_dataa1, select_columns = 
                         c("Store","Holiday_Flag"), remove_first_dummy = TRUE,remove_selected_columns = TRUE)

# split data into train and test
data_train<-filter(d_data, partition == "train") %>% select( - partition)
data_test<-filter(d_data, partition == "test") %>% select( - partition)

#Run the stepwise regression
#Create model with no independent variables
nullmodel <- lm(Weekly_Sales ~ 1, data = data_train)
summary(nullmodel)
#Create model with all the independent variables
allmodel <- lm(Weekly_Sales ~., data = data_train)
summary(allmodel)
#Calculate the stepwise equation
Reg_step <- step(nullmodel, scope = formula(allmodel))
summary(Reg_step)

reg_predict_train <- predict(Reg_step,data_train)
RMSE(reg_predict_train,data_train$Weekly_Sales)    # RMSE of train data 

reg_predict_test <- predict(Reg_step,data_test)
RMSE(reg_predict_test,data_test$Weekly_Sales)

        #QUESTION5:
library(haven)
library(dplyr)
library(fastDummies)
library(mosaic)

#DATA PREPROCESSING
heart_data<-read_spss("C:\\Users\\x1 Carbon\\Downloads\\Mid data\\Heart_Disease.sav")
attach(heart_data)

sum(is.na(heart_data))
duplicated(heart_data)

#Create training and test data frames
heart_data_train<-filter(heart_data, partition == "1") %>% select( - partition)
heart_data_test<-filter(heart_data, partition == "0") %>% select( - partition)

#logistic stepwise regression
nullmodel_1 <- glm(Heart_disease ~ 1, data = heart_data_train, family=binomial)
summary(nullmodel_1)
#Create model with all the independent variables
allmodel_1 <- glm(Heart_disease ~., data = heart_data_train, family=binomial)
summary(allmodel_1)
#Calculate the stepwise equation
logregstep <- step(allmodel_1, scope = formula(allmodel_1))
summary(logregstep)

#Get test predictions
logregstep_test_pred <- mutate(heart_data_test, 
                               Predict=predict(logregstep, heart_data_test, type="response") %>% round())
mean(~(Heart_disease == Predict), data = logregstep_test_pred)

#Absolute values
tally(Heart_disease ~ Predict, data = logregstep_test_pred) %>% addmargins()
#Fractions
tally(Heart_disease ~ Predict, data = logregstep_test_pred) %>% prop.table(margin=1)%>%round(3)

# question3
library(rpart) # classification of decision trees
library(rpart.plot)
library(caret)

emp_data<-read_spss("C:\\Users\\x1 Carbon\\Downloads\\Mid data\\Emp_fit_data.sav")
attach(emp_data)

emp_data<-select(emp_data, - Key)

lpartition<-sample(c('train','test'), size = nrow(emp_data), replace = TRUE, prob=c(0.7,0.3))
partition <- sample(c('train','test'), size = nrow(emp_data), replace = TRUE, prob = c(0.7,0.3))
emp_data <- mutate(emp_data, partition)

emp_data_train<-filter(emp_data, partition == "train") %>% select( - partition)
emp_data_test<-filter(emp_data, partition == "test") %>% select( - partition)

#MODEL TRAINING
#Create regression tree model with cp=0.03
empdat_tree03<-rpart(Performance ~ ., data = emp_data_train, method = "anova", cp = 0.03)
#plot the tree
rpart.plot(empdat_tree03, roundint = FALSE, nn= TRUE, extra = 1)

#Create regression tree model with cp = 0.005
empdat_tree005<-rpart(Performance ~ ., data = emp_data_train, method = "anova", cp = 0.005)
#plot the tree
rpart.plot(empdat_tree005, roundint = FALSE, nn= TRUE, extra = 1)

#Get predictions for  p=0.03
pred_tree03_train <-predict(empdat_tree03, emp_data_train)
RMSE(pred_tree03_train, emp_data_train$Performance)

#MODEL TESTING
#Get TEST predictions for  p=0.005
pred_tree005_test <-predict(empdat_tree005, emp_data_test)
#Calculate RMSE by comparing predictions with actuals
RMSE(pred_tree005_test, emp_data_test$Performance)

