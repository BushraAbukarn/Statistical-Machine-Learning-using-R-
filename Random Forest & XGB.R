#1.Factor the variables
heart <- heart %>% mutate( 
  HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0,1), labels = c("No", "Yes")), 
  Smoker = factor(Smoker, levels=c(0,1), labels=c("No", "Yes")), 
  Sex = factor(Sex, levels=c(0,1), labels=c("Male", "Female"))
)

#2.Split the features 
input <- heart[, c("BMI", "Smoker", "MentHlth", "Age", "Education", "Income")]
outcome <- heart$HeartDiseaseorAttack

#3.Split the data into training and testing 
heart_split <- initial_split(heart, prop = 0.8, strata = HeartDiseaseorAttack)
train_data <- heart_split %>% 
  training()
test_data <- heart_split %>% 
  testing()

#4.1Random Forest Model 
RF <- randomForest(HeartDiseaseorAttack ~ BMI+Smoker+MentHlth+Age+Education+Income, 
                   data=train_data, importance = TRUE, ntree = 500)
round(importance(RF), 2)

#4.2XGBoost Model 
XGB <- boost_tree(
  mode = "classification",
  engine = "xgboost",
  mtry = NULL,
  trees = 200,
  min_n = NULL,
  tree_depth = 3,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL
)
BS <- fit(XGB, HeartDiseaseorAttack ~., data=train_data)


#5.Confusion Matrix 
predicted <- predict(BS,test_data)$.pred_class
confusionMatrix(predicted, test_data$HeartDiseaseorAttack, mode = "prec_recall", positive = "Yes")
