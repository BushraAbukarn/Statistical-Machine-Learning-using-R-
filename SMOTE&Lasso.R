```{r}
heart <- heart %>% mutate( 
  HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0,1), labels = c("No", "Yes")), 
  Smoker = factor(Smoker, levels=c(0,1), labels=c("No", "Yes")), 
  Sex = factor(Sex, levels=c(0,1), labels=c("Male", "Female"))
  
#1.Split the features 
input <- heart[, c("BMI", "Smoker", "MentHlth", "Age", "Education", "Income")]
outcome <- heart$HeartDiseaseorAttack
#2.Split the data into training and testing 
heart_split <- initial_split(heart, prop = 0.8, strata = HeartDiseaseorAttack)
train_data <- training(heart_split)
test_data <- testing(heart_split)
#3.1Naive Bayes Model
nbayes <- naiveBayes(HeartDiseaseorAttack ~ ., data = train_data)
nbayes
#3.2Predict using the test data to extract the confusion matrix
predicted <- predict(nbayes, newdata = test_data)
confusionMatrix(data = as.factor(predicted), reference = as.factor(test_data$HeartDiseaseorAttack), 
                mode = "prec_recall")
#4.1Logistic Regression Model 
LR <- glm(HeartDiseaseorAttack ~ ., data=train_data, family="binomial")
summary(LR)
#4.2Predict using the test data to extract the confusion matrix
predicted <- predict(LR,test_data, type="response")
pred_class <- as.factor(ifelse(predicted < 0.5, "Yes", "No"))
confusionMatrix(data = as.factor(predicted), reference = as.factor(test_data$HeartDiseaseorAttack), 
                mode = "prec_recall")
#5.1SVM Model 
svmModel <- svm(HeartDiseaseorAttack ~ ., data=train_data, kernel = "linear")
print(svmModel)
#5.2Predict using the test data to extract the confusion matrix
predicted <- predict(svmfit,test_data)
confusionMatrix(data = as.factor(predicted), reference = as.factor(test_data$HeartDiseaseorAttack), 
                mode = "prec_recall")

##SMOTE 
#3.1Naive Bayes Model
nbayes <- naiveBayes(HeartDiseaseorAttack ~ ., data = train_data)
nbayes
#3.2SMOTE 
smote_result <- SMOTE(train_data[,-7],train_data[,7])
train_data <- smote_result$data
#3.3Predict using the test data to extract the confusion matrix
predicted <- predict(nbayes, newdata = test_data)
confusionMatrix(data = as.factor(predicted), reference = as.factor(test_data$HeartDiseaseorAttack), 
                mode = "prec_recall")
#4.1Logistic Regression Model 
LR <- glm(HeartDiseaseorAttack ~ ., data=train_data, family="binomial")
summary(LR)
#4.2SMOTE 
smote_result <- SMOTE(train_data[,-7],train_data[,7])
train_data <- smote_result$data
#4.3Predict using the test data to extract the confusion matrix
predicted <- predict(LR,test_data, type="response")
pred_class <- as.factor(ifelse(predicted < 0.5, "Yes", "No"))
confusionMatrix(data = as.factor(predicted), reference = as.factor(test_data$HeartDiseaseorAttack), 
                mode = "prec_recall")
#5.1SVM Model 
svmModel <- svm(HeartDiseaseorAttack ~ ., data=train_data, kernel = "linear")
print(svmModel)
#5.2SMOTE 
smote_result <- SMOTE(train_data[,-7],train_data[,7])
train_data <- smote_result$data
#5.3Predict using the test data to extract the confusion matrix
predicted <- predict(svmModel,test_data)
confusionMatrix(data = as.factor(predicted), reference = as.factor(test_data$HeartDiseaseorAttack), 
                mode = "prec_recall")

#6.Logistic Lasso Regression Model 
LR <- logistic_reg(penalty = 0.001, mixture = 1) %>%  
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  #6.1Fitting the model   
  fit(HeartDiseaseorAttack ~ ., data=train_data)
#6.2Predict using the test data to extract the confusion matrix
predicted <- predict(LR, new_data = test_data, type="class")
confusionMatrix(predicted$.pred_class, test_data$HeartDiseaseorAttack, mode = "prec_recall")
#6.3Extract the fitted engine and generate a variable importance plot
LR %>%
  extract_fit_engine() %>%
  vip(num_features = 8)
```
