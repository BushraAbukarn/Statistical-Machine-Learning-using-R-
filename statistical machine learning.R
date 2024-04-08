title: "STAT 473"
output: html_document
date: "2024-01-29"

```{r Lec 1}
#Skim a data frame, getting useful summary statistics
bank %>%
  group_by(Personal_Loan) %>%
  skimr::skim() 

#Getting the proportion 
bank %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))

#Visualizing, make a matrix of plots with a given data set  
bank %>% 
  select(Personal_Loan, Income:CCAvg) %>% 
  ggpairs(coulmns = 2:3, mapping = aes(color = Personal_Loan, alpha = 0.5))
```

```{r Lec 2}
#1.1 Change feature (variable) name
UniversalBank_tb <- UniversalBank %>% 
  rename(c('Personal_Loan' = 'Personal.Loan', 
           'CD_Account' = 'CD.Account', 
           'Securities_Account' = 'Securities.Account'))
str(UniversalBank)

#1.2 Categorize ----> factor 
UniversalBank_tb %>% 
  mutate(
    Personal_Loan = factor(Personal_Loan, 
                           levels = c(1,0), 
                           labels = c("Yes", "No")), 
    Securities_Account = factor(Securities_Account, 
                                levels = c(0,1), 
                                labels = c("No", "Yes")), 
    CD_Account = factor(CD_Account, 
                        levels = c(0,1), 
                        labels = c("No", "Yes")), 
    Online = factor(Online, 
                    levels = c(0,1), 
                    labels = c("No", "Yes")), 
    CreditCard = factor(CreditCard,
                        levels = c(0,1), 
                        labels = c("No", "Yes")), 
    Education = factor(Education, 
                       levels = c(1:3), 
                       labels = c("Undergrad", 
                                  "Graduate", 
                                  "Professional"))
  )
str(UniversalBank_tb)

#1.3 Delete insignificant features 
UniversalBank_tb %>%
  select(-c(ID, `ZIP.Code`))

#2 Base accuracy 
UniversalBank_tb %>% 
  select(Personal_Loan, Education:CreditCard) %>% 
  pivot_longer(Education:CreditCard) %>% 
  ggplot(mapping = aes(y = value, 
                       fill = Personal_Loan)) + 
  geom_bar(position = "fill") + 
  facet_wrap(vars(name), 
             scales = "free", 
             ncol = 2) + 
  labs(x = NULL, y = NULL, fill = NULL)
set.seed(123)

#3 Data Partition
UniversalBank_split <- UniversalBank_tb %>% 
  initial_split(prop = 0.7, strata = Personal_Loan)
UniversalBank_split

train_data <- UniversalBank_split %>% 
  training()
test_data <- UniversalBank_split %>% 
  testing()

train_data %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n / sum(n))

test_data %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n / sum(n))
```

```{r}
#4.1.1 Model - Decision Tree
ctrl <- trainControl(classProbs = TRUE, savePredictions = TRUE, summaryFunction = twoClassSummary)
tree = train(Personal_Loan ~ ., data = train_data, method = "rpart", trControl = ctrl, metric="ROC")

#4.2.1 Model - Naive Bayes  
nbayes = naiveBayes(Personal_Loan ~ ., data=train_data, laplace = 1)
nbayes %>% 
  predict(test_data)
#4.2.2 Prediction - Naive Bayes
confusionMatrix(predicted, test_data$Personal_Loan, mode = "prec_recall")
predicted <- predict(nbayes, newdata = test_data)
lift_result <- data.frame(truth = test_data$Personal_Loan)
lift_result$yes <- predict(nbayes, test_data, type = "raw") [,"Yes"]
lift_result$no <- predict(nbayes, test_data, type = "raw")[,"No"]
lift_result$predicted <- predict(nbayes, test_data)
#4.2.3 Visualization - Naive Bayes
nbayes$tables$Education
gain_curve(lift_result, truth, yes) %>% autoplot()
roc_curve(lift_result, truth, yes) %>% autoplot() 
lift_curve(lift_result, truth, yes) %>% autoplot()

#4.3.1 Model - Logistic Regression 
LR <- glm(Personal_Loan ~ ., data=train_data, family="binomial")
summary(LR)
#4.3.2 Prediction - Logistic Regression
predicted <- predict(LR, test_data, type = "response")
pred_class <- as.factor(ifelse(predicted < 0.5, "Yes", "No"))
confusionMatrix(pred_class, test_data$Personal_Loan, mode = "prec_recall")
lift_result <- data.frame(truth = test_data$Personal_Loan)
lift_result$yes <- 1 - predicted
lift_result$no <- predicted
lift_result$predicted <- pred_class
#4.3.3 Visualization - Logistic Regression
gain_curve(lift_result, truth, yes) %>% autoplot()
roc_curve(lift_result, truth, yes) %>% autoplot() 
lift_curve(lift_result, truth, yes) %>% autoplot()

#4.4.1 Model - Decision Tree
tree <- rpart(Personal_Loan ~., 
              data=train_data, parms = list(split="information"), 
              method = "class",
              control = rpart.control(minsplit = 20, maxdepth = 3, cp = 0.01))
#4.4.2 Prediction - Decision Tree
predicted <- predict(tree,test_data,type = "class")
confusionMatrix(predicted, test_data$Personal_Loan, mode = "prec_recall")
lift_result <- data.frame(truth = test_data$Personal_Loan)
lift_result$yes <- predict(tree, test_data, type = "prob")[,1]
lift_result$no <- predict(tree, test_data, type = "prob")[,2]
lift_result$predicted <- predict(tree, test_data,type="class")
#4.4.3 Visualization
gain_curve(lift_result, truth, yes) %>% autoplot()
roc_curve(lift_result, truth, yes) %>% autoplot()
lift_curve(lift_result, truth, yes) %>% autoplot()
#4.4.4 Decision Tree
vip(tree)
rpart.plot(tree)

#4.5.1 Model - Support Vector Machine 
svmfit <- svm(Personal_Loan ~ ., data = train_data, kernel = "linear")
print(svmfit)
#4.5.2 Prediction - Support Vector Machine 
predicted <- predict(svmfit,test_data)
confusionMatrix(predicted, test_data$Personal_Loan, mode = "prec_recall")
prob <- predict(svmfit, test_data, probability=TRUE)
prob <- data.frame(attr(prob, "probabilities"))
lift_result <- data.frame(truth = test_data$Personal_Loan)
lift_result$yes <- prob$Yes
lift_result$no <- prob$No
lift_result$predicted <- predict(svmfit, test_data)
#4.5.3 Visualization - Support Vector Machine 
gain_curve(lift_result, truth, yes) %>% autoplot()
roc_curve(lift_result, truth, yes) %>% autoplot()
lift_curve(lift_result, truth, yes) %>% autoplot()
```
