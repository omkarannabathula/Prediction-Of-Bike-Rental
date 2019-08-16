#Clear Environment-
rm(list=ls())

library(corrplot)
library(ggplot2)
library(dplyr)
library(rcompanion)
library(mlr)
library(caTools)
library(MASS)
library(Metrics)
library(randomForest)

#Set working directory-
setwd("F:/Edvisor Project/Bike_Rental")

#Check working directory-
getwd()

#load data-
bikedata= read.csv("day.csv")

#------------------------------Exploratory Data Analysis-------------------------------------------#
class(bikedata)
dim(bikedata)
head(bikedata)
names(bikedata)
str(bikedata)
summary(bikedata)

#Remove the instant variable, as it is index in dataset.
bikedata= subset(bikedata,select=-(instant))

#Remove date variable as we have to predict count on seasonal basis not date basis-
bikedata= subset(bikedata,select=-(dteday))

#check the remaining variables-
names(bikedata)

#Rename the variables-
names(bikedata)[1]="Season"
names(bikedata)[2]="Year"
names(bikedata)[3]="Month"
names(bikedata)[4]="Holiday"
names(bikedata)[5]="Weekday"
names(bikedata)[6]="Workingday"
names(bikedata)[7]="Weather"
names(bikedata)[8]="Temperature"
names(bikedata)[9]="Atemperature"
names(bikedata)[10]="Humidity"
names(bikedata)[11]="Windspeed"
names(bikedata)[12]="Casual"
names(bikedata)[13]="Registered"
names(bikedata)[14]="Count"


#Seperate categorical and numeric variables-
names(bikedata)

#numeric variables-
cnames= c("Temperature","Atemperature","Humidity","Windspeed","Count")

#categorical varibles-
cat_cnames= c("Season","Year","Month","Holiday","Weekday","Workingday","Weather")
str(bikedata)

#=================================Data Pre-processing==========================================#

#--------------------------------Missing Vlaue Analysis----------------------------------------#
#Check missing values in dataset-
sum(is.na(bikedata))
#Missing value= 0
#No Missing values in data.

#convering categorical variables into factor

bikedata$Season <- as.factor(bikedata$Season)
levels(bikedata$Season)[levels(bikedata$Season) == 1] <- 'Spring'
levels(bikedata$Season)[levels(bikedata$Season) == 2] <- 'Summer'
levels(bikedata$Season)[levels(bikedata$Season) == 3] <- 'Fall'
levels(bikedata$Season)[levels(bikedata$Season) == 4] <- 'Winter'

bikedata$Month <- as.factor(bikedata$Month)
levels(bikedata$Month)[levels(bikedata$Month) == 1] <- 'Jan'
levels(bikedata$Month)[levels(bikedata$Month) == 2] <- 'Feb'
levels(bikedata$Month)[levels(bikedata$Month) == 3] <- 'Mar'
levels(bikedata$Month)[levels(bikedata$Month) == 4] <- 'Apr'
levels(bikedata$Month)[levels(bikedata$Month) == 5] <- 'May'
levels(bikedata$Month)[levels(bikedata$Month) == 6] <- 'June'
levels(bikedata$Month)[levels(bikedata$Month) == 7] <- 'July'
levels(bikedata$Month)[levels(bikedata$Month) == 8] <- 'Aug'
levels(bikedata$Month)[levels(bikedata$Month) == 9] <- 'Sep'
levels(bikedata$Month)[levels(bikedata$Month) == 10] <- 'Oct'
levels(bikedata$Month)[levels(bikedata$Month) == 11] <- 'Nov'
levels(bikedata$Month)[levels(bikedata$Month) == 12] <- 'Dec'

bikedata$Year <- as.factor(bikedata$Year)
levels(bikedata$Year)[levels(bikedata$Year) == 0] <- '2011'
levels(bikedata$Year)[levels(bikedata$Year) == 1] <- '2012'

bikedata$Holiday <- as.factor(bikedata$Holiday)
bikedata$Weekday <- as.factor(bikedata$Weekday)
bikedata$Workingday <- as.factor(bikedata$Workingday)

bikedata$Weather <- as.factor(bikedata$Weather)
levels(bikedata$Weather)[levels(bikedata$Weather) == 1] <-'Clear'
levels(bikedata$Weather)[levels(bikedata$Weather) == 2] <-'Cloudy'
levels(bikedata$Weather)[levels(bikedata$Weather) == 3] <-'Light Snow'
levels(bikedata$Weather)[levels(bikedata$Weather) == 4] <-' Heavy Rain'


#-----------------------------------Outlier Analysis----------------------------------------------#


#create Box-Plot for outlier analysis-

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var), eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers")
  hist(var_name,
       main = "With outliers",
       xlab = NA,
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main = "Without outliers")
  hist(var_name,
       main = "Without outliers",
       xlab = NA,
       ylab = NA)
  title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name)) *
                                            100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  
}


outlierKD(bikedata, Temperature) #no outliers
outlierKD(bikedata, Atemperature) #no outliers
outlierKD(bikedata, Humidity) # no extreme outlier detected
outlierKD(bikedata, Windspeed) #some extreme values are present but canot be considered as outlier
outlierKD(bikedata, Casual) # no logical outliers
outlierKD(bikedata, Registered)# no ouliers
outlierKD(bikedata, Count)# no ouliers


#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
#                        Correlation Analysis                                                      #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#
par(mfrow = c(1, 1))
numeric_predictors <- unlist(lapply(bikedata, is.numeric))
numVarDataset <- bikedata[, numeric_predictors]
corr <- cor(numVarDataset)
corrplot(
  corr,
  method = "color",
  outline = TRUE,
  cl.pos = 'n',
  rect.col = "black",
  tl.col = "indianred4",
  addCoef.col = "black",
  number.digits = 2,
  number.cex = 0.60,
  tl.cex = 0.70,
  cl.cex = 1,
  col = colorRampPalette(c("green4", "white", "red"))(100)
)

# Findings :
# 1. temp and atemp are highly correlated

# Looking at target variable
ggplot(data = bikedata, aes(Count)) +
  geom_histogram(aes(
    y = ..density..,
    binwidth = .10,
    colour = "black"
  ))
# Target variable looks like normal distribution

#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
#                        Univariate Analysis                                                       #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#
# 1. Continous predictors
univariate_continuous <- function(dataset, variable, variableName) {
  var_name = eval(substitute(variable), eval(dataset))
  print(summary(var_name))
  ggplot(data = dataset, aes(var_name)) +
    geom_histogram(aes(binwidth = .8, colour = "black")) +
    labs(x = variableName) +
    ggtitle(paste("count of", variableName))
}

univariate_continuous(bikedata, Count, "Count")
univariate_continuous(bikedata, Temperature, "Temperature")
univariate_continuous(bikedata, Atemperature, "Atemperature")
univariate_continuous(bikedata, Humidity, "Humidity") # skwed towards left
univariate_continuous(bikedata, Windspeed, "Windspeed") #skewed towards right
univariate_continuous(bikedata, Casual, "Casual") # skwed towards right
univariate_continuous(bikedata, Registered, "Registered")

#2. categorical variables
univariate_categorical  <- function(dataset, variable, variableName) {
  variable <- enquo(variable)
  
  percentage <- dataset %>%
    dplyr::select(!!variable) %>%
    group_by(!!variable) %>%
    summarise(n = n()) %>%
    mutate(percantage = (n / sum(n)) * 100)
  print(percentage)
  
  dataset %>%
    count(!!variable) %>%
    ggplot(mapping = aes_(
      x = rlang::quo_expr(variable),
      y = quote(n),
      fill = rlang::quo_expr(variable)
    )) +
    geom_bar(stat = 'identity',
             colour = 'white') +
    labs(x = variableName, y = "count") +
    ggtitle(paste("count of ", variableName)) +
    theme(legend.position = "bottom") -> p
  plot(p)
}

univariate_categorical(bikedata, Season, 'Season')
univariate_categorical(bikedata, Year, "Year")
univariate_categorical(bikedata, Month, "Month")
univariate_categorical(bikedata, Holiday, "Holiday")
univariate_categorical(bikedata, Weekday, "Weekday")
univariate_categorical(bikedata, Workingday, "Workingday")
univariate_categorical(bikedata, Weather, "Weather")

# ------------------------------------------------------------------------------------------------ #
#
#                                     bivariate Analysis
#
#------------------------------------------------------------------------------------------------- #

# bivariate analysis for categorical variables
bivariate_categorical <-
  function(dataset, variable, targetVariable) {
    variable <- enquo(variable)
    targetVariable <- enquo(targetVariable)
    
    ggplot(
      data = dataset,
      mapping = aes_(
        x = rlang::quo_expr(variable),
        y = rlang::quo_expr(targetVariable),
        fill = rlang::quo_expr(variable)
      )
    ) +
      geom_boxplot() +
      theme(legend.position = "bottom") -> p
    plot(p)
    
  }

bivariate_continous <-
  function(dataset, variable, targetVariable) {
    variable <- enquo(variable)
    targetVariable <- enquo(targetVariable)
    ggplot(data = dataset,
           mapping = aes_(
             x = rlang::quo_expr(variable),
             y = rlang::quo_expr(targetVariable)
           )) +
      geom_point() +
      geom_smooth() -> q
    plot(q)
    
  }

bivariate_categorical(bikedata, Season, Count)
bivariate_categorical(bikedata, Year, Count)
bivariate_categorical(bikedata, Month, Count)
bivariate_categorical(bikedata, Holiday, Count)
bivariate_categorical(bikedata, Weekday, Count)
bivariate_categorical(bikedata, Workingday, Count)
bivariate_categorical(bikedata, Weather, Count)

bivariate_continous(bikedata, Temperature, Count)
bivariate_continous(bikedata, Atemperature, Count)
bivariate_continous(bikedata, Humidity, Count)
bivariate_continous(bikedata, Windspeed, Count)
bivariate_continous(bikedata, Casual, Count)
bivariate_continous(bikedata, Registered, Count)

# removing instant and dteday
bikedata$instant <- NULL
bikedata$Date <- NULL
bikedata$Casual <- NULL
bikedata$Registered <- NULL

# ------------------------------------------------------------------------------------------------ #
#
#                                     Feature scaling or Normalization                             #
#
#------------------------------------------------------------------------------------------------- #

scaledData <- normalizeFeatures(bikedata,'Count')

# Function for calculating Mean Absolute Error
MAE <- function(actual,predicted){
  error = actual - predicted
  mean(abs(error))
}

# ----------------- Model 1 Linear Regression -----------------------------------------------------#


set.seed(654)
split <- sample.split(bikedata$Count, SplitRatio = 0.70)
training_set <- subset(bikedata, split == TRUE)
test_set <- subset(bikedata, split == FALSE)


model1 <- lm(Count ~ ., data = training_set)

# step wise model selection

modelAIC <- stepAIC(model1, direction = "both")
summary(modelAIC)

# Apply prediction on test set
test_prediction <- predict(modelAIC, newdata = test_set)

test_rmse <- rmse(test_set$Count, test_prediction)
print(paste("root-mean-square error for linear regression model is ", test_rmse))
print(paste("Mean Absolute Error for linear regression model is ",MAE(test_set$Count,test_prediction)))
print("summary of predicted count values")
summary(test_prediction)
print("summary of actual Count values")
summary(test_set$Count)

# From the summary we can observe negative prediction values
#We will perform log transformation of trarget variable
model2 <- lm(log(Count)~., data = training_set)

stepwiseLogAICModel <- stepAIC(model2,direction = "both")
test_prediction_log<- predict(stepwiseLogAICModel, newdata = test_set)
predict_test_nonlog <- exp(test_prediction_log)

test_rmse2 <- rmse(test_set$Count, predict_test_nonlog)
print(paste("root-mean-square error between actual and predicted", test_rmse))
print(paste("Mean Absolute Error for linear regression model is ",
            MAE(test_set$Count,predict_test_nonlog)))

summary(predict_test_nonlog)
summary(test_set$Count)



par(mfrow = c(1,1))
plot(stepwiseLogAICModel)

# ----------------- Model 2 Random forest -----------------------------------------------------#

model1 <- randomForest(Count ~.,
                           data = training_set,ntree = 500, mtry = 8, importance = TRUE)
print(model1)
par(mfrow = c(1,1))
plot(model1)


# 300 trees selected from the plot

tumedmodel <- tuneRF(training_set[,1:11], training_set[,12], stepFactor = 0.5, plot = TRUE, 
                     ntreeTry = 250, trace = TRUE, improve = 0.05)

# selected mtry = 6 from the plot

tuned_randomForest <-  randomForest(Count ~. - Atemperature,
                                    data = training_set,ntree = 250, mtry = 6, importance = TRUE)
tuned_randomForest

# predicting using random forest model 1
rf1_prediction <- predict(tuned_randomForest,test_set[,-12])
rmse(rf1_prediction,test_set$Count)
print(paste("Mean Absolute Error for Random forest regressor  is ",
            MAE(test_set$Count,rf1_prediction)))

# Tuned Random Forest

varImpPlot(tuned_randomForest)

# Random forest is performing better than linear regression.

# Model input and output for linear regression and Random forest
write.csv(test_set, file = "InputLinearRegressionR.csv")
write.csv(test_set, file = "InputRandomForestR.csv")
write.csv(predict_test_nonlog, file="outputLogisticRegressionR.csv")
