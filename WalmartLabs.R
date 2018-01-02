library(sas7bdat)
#walmart <- read.sas7bdat("Desktop/R/datasets/Linear_Reg_walmart.sas7bdat")
walmart <- read.sas7bdat(file.choose())
walmart
dim(walmart)
#which variables have an impact on customer satisfaction; analysing the 
#situation and create a model
#to be able to predict customer satisfaction
#Divide the data into training and validation set.
#Carry out the initial predictive modeling based on
#the training data and then use the hold out set to 
#compare the performance of the predictive model.
#Also please check the result with that of the Adjusted R square criteria.
#Please carry out a validity check for the model using the hold out data set.
str(walmart)
View(walmart)
boxplot(walmart)
plot(walmart$Customer_Satisfaction, walmart$E_Commerce)
cor(walmart$Customer_Satisfaction,walmart$E_Commerce)
cor(walmart)
set.seed(25) # setting the random number seed for splitting the dataset
ranuni <- sample(x=c("Training","Testing"),size=nrow(walmart),
                 replace=T,prob=c(0.7,0.3)) 

# creating a vector with texts "Training" (approx. 70% )& "Testing" (approx. 30%)
cbind(ranuni)
TrainingData <- walmart[ranuni=="Training",] # generating the training data
nrow(TrainingData)
View(TrainingData)
TestingData <- walmart[ranuni=="Testing",] # generating the testing data
View(TestingData)
indVariables <- colnames(walmart[,2:14]) # getting the independent variables

rhsOfModel <- paste(indVariables,collapse ="+")
rhsOfModel

# creating the right hand side of the model expression
model <- paste("Customer_Satisfaction ~ ",rhsOfModel)

# creating the text model
model
class(model)
frml <- as.formula(model) # converting the above text into a formula
frml
class(frml)

TrainingModel <- lm(formula= frml, data= TrainingData)
TrainingModel

class(TrainingModel)
summary(TrainingModel)
install.packages("usdm")
library(usdm)
library(car)
vif(TrainingModel)
TrainingModel <- step(object= TrainingModel, direction="both")
summary(TrainingModel)    #ALL VAR(s) mentioned ARE IMPACTING THE Y VARIABLE

vif(TrainingModel)  #to check for the Multicollinearity   #VIFs less than 5 so no problem

durbinWatsonTest(TrainingModel)     #Null Hyp: rho = 0

qqnorm(TrainingModel$residuals)  #qq plot

qqline(TrainingModel$residuals)   #will draw a straight line

shapiro.test(TrainingModel$residuals)    
#checking normALITY OF RESIDUALS. SAMPLE SIZE IS LESS. 
#Works good when n > 2000.

residuals <- resid(TrainingModel) #getting the residuals
plot(x= TrainingData$Customer_Satisfaction, y = residuals)
plot(TrainingModel)


TestPred<- predict.lm(object= TrainingModel, newdata = TestingData)
TestPred    #estimated values of y 
summary(TestPred)
tbl <- table(TrainingData$Customer_Satisfaction, TestPred)
#check correlation with actual values
cor(x= TestPred, y = TestingData$Customer_Satisfaction)  #corr high means good
plot(x= TestingData$Customer_Satisfaction,y= TestPred, type= "p", col= "red")  
#plot looks linear then good

