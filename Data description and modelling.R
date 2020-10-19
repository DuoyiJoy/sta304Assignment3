# gss
cbind(gss$caseid, gss$age, gss$place_birth_province,
      gss$region, gss$sex, gss$education, gss$income_respondent) %>%
  head() %>%
  knitr::kable(caption = "Figure:Partial Raw Social Identity Responses of General Social Survey 2017",
               col.names = c('Sample', 'Age', 'Birth Province', 'Region','gender', 'Education',
                             "Income"),
  )

library(ggthemes)
p1 <- gss0 %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(fill='#8CBD8C',color = 'grey') +
  ggtitle(label= "Figure 1: Age Distribution of the GSS Respondents") +
  xlab('Age of Respondents (in year)')+
  ylab("Number of Respondents")
p1

library(popbio)
logi.hist.plot(gss0$age, 
               gss0$children_intention,
               boxp=FALSE,type="hist",
               col="#8CBD8C",
               xlabel = "Age of Respondents",
               ylabel = "Whether Entering Parenthood in Life",
               mainlabel = "Figure 2: Distribution of Fertile Intention")

gss0 %>%
  count(education) %>%
  knitr::kable(caption = "Figure 3: Educational Background of the Sample Respondents")

gss0 %>%
  count(region) %>%
  knitr::kable(caption = "Figure 4: Resident Region of the Sample Respondents")

p3 <- gss0 %>%
  ggplot(aes(region)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  geom_text(stat='count', aes(label=..count..), vjust=0.3) +
  ggtitle(label= 'Figure 5: Distribution of Region') +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab('Region of Residence')+
  ylab("Number of Respondents")

p3

gss0 %>%
  count(income_respondent) %>%
  knitr::kable(caption = "Figure 6: Personal Income of the Respondents")

p4 <- gss0 %>%
  ggplot(aes(income_respondent)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  ggtitle(label= 'Figure 7: Distribution of Personal Income') +
  geom_text(stat='count', aes(label=..count..), vjust=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab('Income of Respondents')+
  ylab("Number of Respondents")
p4


gss0 %>%
  count(place_birth_province) %>%
  knitr::kable(caption = "Figure 8: Birth Place Orgins of the Respondents")

p5 <- gss0 %>%
  ggplot(aes(place_birth_province)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  ggtitle(label= 'Figure 9: Distribution of Birthplace') +
  geom_text(stat='count', aes(label=..count..), vjust=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab('province of Birth')+
  ylab("Number of Respondents")
p5


gss0 %>%
  count(is_male) %>%
  knitr::kable(caption = "Figure 10: Sex of the Respondents")





{r GLM, include=FALSE}
# ====================== GLM ==========================
# first, we could conduct a basic logistic regression model
# conduct regression linear model without survey design
logit1 <- glm(children_intention ~ place_birth_province + region + ageC + education + 
                income_respondent + is_male,
              data = gss0,
              family = "binomial"
)

# then apply the stratification to the model to obtain a survey generalized linear model 
strat_design <- svydesign(id=~1, strata=~province, data=gss0, fpc=~fpc)
svylogit1 <- svyglm(children_intention ~ place_birth_province + region + ageC + education + 
                      income_respondent + is_male,
                    strat_design, 
                    family="binomial")
summary(svylogit1)


# we could create tables of our estimated coefficients of the regression model,
knitr::kable(summary(svylogit1)$coef,
             digits = 2,
             caption = "Figure 11:Coefficient Table of Regression Linear Model")


knitr::kable(confint(svylogit1), 
             digits = 2,
             caption = "Figure 12:95% Confidence Interval of Logistic Regression Model Coefficients")


# =========== Cross Validation ============
# In order to access the ability of our model in predicting outcome variable
# (i.e. odds of having fertile plans) in different subsets of the target population,
# we want to apply a k-fold cross-validation
# The mechancism is to partition our sample dataset into k equally sized subset
# here we call it k folds. One fold is held out for validation while the other k-1 
# folds are used to train the model and then used to predict the target variable in
# testing data. We ought to repeat the process by k times., with the performance of
# each model in predicting the hold-out set being tracked using a performance metric 
# such as accuracy. 

# K = 5 fold CV, we assign our cross-validatio to be five-fold
K <- 5

# demonstrate using the first split
k <- 1
fold.size <- nrow(gss0) / K
testing.index <- (k-1)*fold.size + 1:fold.size

training <- gss0[-testing.index,]
testing <- gss0[testing.index,]

# glm's repeated cross validation
train_control = trainControl(method="repeatedcv", number=5, repeats=5)
modelLog = train(children_intention ~ place_birth_province + region + ageC + education + 
                   income_respondent + is_male,
                 data=training, 
                 method="glm", 
                 family=binomial,
                 trControl=train_control)
# we created a classification table, which is also called confusion matrix
PredTrain = predict(modelLog, newdata=training, type="raw")

knitr::kable(table(training$children_intention, PredTrain > 0.5),
             caption = 'Figure 13:Confusion Matrix of Repeated Cross Validation')

# Load the data
# Predict the probability (p) of diabete positivity
probabilities <- predict(svylogit1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Select only numeric predictors
linearity_data <- data.frame(gss0$age)
predictors <- colnames(linearity_data)
# Bind the logit and tidying the data for plot
linearity_data <- linearity_data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity_data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")


car::vif(svylogit1)