library(mice)
Admit <- read_excel("Data/Thao_AdmitFTF_FA1624_v1.xlsx")
Admit <- Admit %>% select(-EFC)
Admit$HIGH_SCHOOL_GPA <- as.numeric(Admit$HIGH_SCHOOL_GPA)

#I.DATA CLEANING

#1.1. Check Data Type and Missing Before Cleaning 
library(skimr)
summary(Admit)
skim(Admit)

#1.2. Impute Missing Data 
  #1.2.1. Check Missing Data
md.pattern(Admit)

  #1.2.2. Use Predictive Mean Method with Numeric Data
impute_admit <- mice(Admit, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(impute_admit)

  #1.2.3. Graph to compare historgram of before imputation and after imputation 

# Initialize an empty vector to store the means
means <- numeric(5)
# Loop through the first 5 sets of imputed values and calculate the mean for each
for (i in 1:5) {
  means[i] <- mean(impute_admit$imp$A05[[i]], na.rm = TRUE)
}

  #1.2.3. Complete A05 imputation 
Admit_complete <- complete(impute_admit, 1) #choose set 1 because it has the nearest mean to the before-imputation

#1.3. Load Function and Clean Data
source("function_admitFTF_v1.R")
df <- clean(Admit_complete)

#1.4. Check Data Type and Missing After Cleaning 
summary(df)
skim(df)

#II. Splitting data into training and testing
set.seed(1314)
train <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob=c(0.75, 0.25))
train_df <- df[train, ]
test_df <- df[!train, ]

#III. Rescale Data

#3.1. Identify numeric columns
numeric_cols <- sapply(df, is.numeric)

#3.2. Fit the scaler on the training data
library(caret)
scaler <- preProcess(train_df[, numeric_cols], method = c("center", "scale"))

#3.3. Transform the training data
train_scaled <- predict(scaler, train_df[, numeric_cols])

#3.4. Transform the testing data using the same scaling parameters
test_scaled <- predict(scaler, test_df[, numeric_cols])

#3.5. Combine scaled numeric data with factor in train 
train_df_scaled <- cbind.data.frame(train_scaled, train_df[, !numeric_cols])

#3.6. Combine scaled numeric data with factor in train 
test_df_scaled <- cbind.data.frame(test_scaled, test_df[, !numeric_cols])
y.test <- test_df_scaled$Enroll

#IV. BUILD BINARY LOGISTIC MODEL

#4.1. Build and Summary Model
glm.fit <- glm(Enroll ~ ., data = train_df_scaled, family = "binomial")
summary(glm.fit) #Summary model

#4.2. Make prediction on test data
glm.prob <- predict(glm.fit, newdata = test_df_scaled, type = "response")

#4.3. Convert Probabilities to Class Labels
glm.class <- rep('0', nrow(test_df_scaled))
glm.class[glm.prob >.5]='1' #Assigns '1' to those entries in glm.class where the predicted probability > 0.5.

#4.4. Confusion Matrix to evaluate Model's Performance
table(glm.class, y.test)

#4.5. Calculate Accuracy (=CorrectPredict/Total)
mean(glm.class == y.test)


#4.6. Check Multicolinearlity 
library(glmtoolbox)
gvif(glm.fit)

#4.7. PseudoR2
library(DescTools)
PseudoR2(glm.fit)

#4.8. Percentage change of enrollment for every one-unit increase
exp(glm.fit$coefficients)-1
