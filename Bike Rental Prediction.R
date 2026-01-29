----------------------------------
# Step 1: Load Dataset & Libraries
----------------------------------
# Load required libraries
install.packages("readxl")
library(readxl)
  
install.packages("tidyverse")
library(tidyverse)

install.packages("lubridate")
library(lubridate)

install.packages("randomForest")
library(randomForest)

install.packages("caret")
library(caret)

# Load dataset
bike <- read_excel("day.xlsx")

# View dataset
head(bike)
str(bike)
summary(bike)

------------------------------
# Step 2: Data Type Conversion
------------------------------
bike$season      <- as.factor(bike$season)
bike$yr          <- as.factor(bike$yr)
bike$mnth        <- as.factor(bike$mnth)
bike$holiday     <- as.factor(bike$holiday)
bike$weekday     <- as.factor(bike$weekday)
bike$workingday  <- as.factor(bike$workingday)
bike$weathersit  <- as.factor(bike$weathersit)

--------------------------------
# Step 3: Missing Value Analysis
--------------------------------
# Check missing values
colSums(is.na(bike))

-----------------------------------
# Step 4: Monthly Distribution Plot
-----------------------------------
ggplot(bike, aes(x = mnth, y = cnt)) +
geom_boxplot() +
labs(title = "Monthly Distribution of Bike Rentals",
      x = "Month",
      y = "Total Rentals (cnt)")

----------------------------------
# Step 5: Yearly Distribution Plot
----------------------------------
ggplot(bike, aes(x = yr, y = cnt)) +
geom_boxplot() +
labs(title = "Yearly Distribution of Bike Rentals",
      x = "Year (0=2011, 1=2012)",
      y = "Total Rentals")

----------------------------------
# Step 6: Outlier Analysis Boxplot
----------------------------------
ggplot(bike, aes(y = cnt)) +
geom_boxplot() +
labs(title = "Outlier Analysis for Bike Rentals",
      y = "Rental Count")

--------------------------
# Step 7: Train-Test Split
--------------------------
set.seed(123)

trainIndex <- createDataPartition(bike$cnt, p = 0.8, list = FALSE)

train <- bike[trainIndex, ]
test  <- bike[-trainIndex, ]

-----------------------------------
# Step 8: Build Random Forest Model
-----------------------------------
rf_model <- randomForest(
    cnt ~ season + yr + mnth + holiday + weekday +
      workingday + weathersit + temp + atemp + hum + windspeed,
    data = train,
    ntree = 500,
    importance = TRUE
  )

print(rf_model)

----------------------------------------
# Step 9: Model Prediction & Performance
----------------------------------------
predictions <- predict(rf_model, test)

# RMSE
rmse <- RMSE(predictions, test$cnt)

# R² Score
r2 <- R2(predictions, test$cnt)

rmse
r2

----------------------------------
# Step 10: Feature Importance Plot
----------------------------------
varImpPlot(rf_model, main = "Feature Importance in Bike Rental Prediction")
