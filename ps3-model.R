# Modeling 
# ============= Graph ==========
# plot the age spread to get a general sense of the sample data
p1 <- gss0 %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(fill='#8CBD8C',color = 'grey') +
  ggtitle(label= "Age Distribution of the GSS Respondents") +
  xlab('Age of the Respondents (in year)')
p1
# plot the distribution of the region of the respondents' residency
p3 <- gss0 %>%
  ggplot(aes(region)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  ggtitle(label= 'Distribution of Region') +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
p3
# plot the income respondent situation using bar plot again
gss0 %>%
  ggplot(aes(income_respondent)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  ggtitle(label= 'Distribution of Personal Income') +
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.5)
# bar graph of the gender proportion of the respondents
gss0 %>%
  ggplot(aes(is_male)) +
  geom_bar(fill='#8CBD8C',col='grey') +
  ggtitle(label= 'Distribution of Gender') +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 
# We can plot the data of the fertile intention of of the respondents
# with different ages
logi.hist.plot(gss0$age, 
               gss0$children_intention,
               boxp=FALSE,type="hist",
               col="#FC9676",
               xlabel = "Age of Respondents",
               ylabel = "Proability of Entering Parenthood in Life",
               mainlabel = "Distribution of Fertile Intention")
gss0 %>%
  ggplot(aes(age, children_intention)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Logistic Regression Model", 
       x = "Age of Respodnets",
       y = "Probability of Reproduction")
# ============= Stratification ===============
# We assume that data were collected using stratified Sampling, devided by Census Metropolitan Areas (CMAs)
# Assuming the population size of citizens aged 15+ per area is shown below:
#Alberta	3544753
#British Columbia	4375354
#Manitoba	1110595
#New Brunswick	665083
#Newfoundland and Labrador	452206
#Nova Scotia	833904
#Ontario	12274182
#Prince Edward Island	132874
#Quebec	7158157
#Saskatchewan	943092

province_list <- unique(gss$province)
# We assigned each measurement to its belonging population
gss0 <- gss0 %>%
  mutate(fpc = case_when(province == "Quebec" ~ 7158157, 
                         province == "Alberta" ~ 3544753,
                         province == "Saskatchewan" ~ 943092,
                         province == "Newfoundland and Labrador" ~ 452206,
                         province == "Manitoba" ~ 1110595,
                         province == "Nova Scotia" ~ 833904,
                         province == "Prince Edward Island" ~ 132874,
                         province == "Ontario" ~ 12274182,
                         province == "British Columbia" ~ 4375354,
                         province == "New Brunswick"  ~ 665083))

# Set the stratum as province, according to the userguide, each respondent
# was assigned stratum within its province
# and we record it in stratification fpc
# for later use of the model conducting
strat_design <- svydesign(id=~1, strata=~province, data=gss0, fpc=~fpc)
# ============= GLM ================
# first, we could conduct a basic logistic regression model
# conduct a regular regression linear model without survey design
logit1 <- glm(children_intention ~ place_birth_province + region + ageC + education + 
                income_respondent + is_male,
              data = gss0,
              family = "binomial"
)

# then apply the stratification design to the model to obtain a survey 
# generalized linear model 
strat_design <- svydesign(id=~1, strata=~province, data=gss0, fpc=~fpc)
svylogit1 <- svyglm(children_intention ~ place_birth_province + region + ageC + education + 
                      income_respondent + is_male,
                    strat_design, 
                    family="binomial")
summary(svylogit1)

# ============= Interpret the Result ===========
# we constructed tables for the model coefficients
knitr::kable(summary(svylogit1)$coef,
             digits = 2,
             caption = "Coefficient Table of Regression Linear Model")
knitr::kable(confint(svylogit1), 
             digits = 2,
             caption = "95% Confidence Interval of Logistic Regression Model Coefficients")

# ============= Logistic Regression Cross Validation ===========
# ============= Cross Validation ============
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
# in this table, the numbers of prediction were plotted in comparsion with the acutal
# outcomes, so that we could assess our model's predicting accuracy
PredTrain = predict(modelLog, newdata=training, type="raw")
table(training$children_intention, PredTrain > 0.5,
      caption = 'Confusion Matrix of Repeated Cross Validation')

# ======================== multicollinearity check ============
car::vif(svylogit1)
# we put in our built model to variance inflation factors,
# then we compare the VIF values to 10 according to the rule of thumb
# variable with value higher than 10 indicates its correlation with other 
# variables in the model

# ========= linearity of the decimal and log-transformed outcome ===========
# record the prediction generated by our model
probabilities <- predict(svylogit1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

# Select only numeric predictor, in our case, age of the respondents
linearity_data <- data.frame(gss0$age)
predictors <- colnames(linearity_data)
# Bind the logit and tidying the data for plot
linearity_data <- linearity_data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(linearity_data, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  ggtitle("Scatterplot of the Respondents' Age and\n Odds of Fertile Intetnion (log-transformed)")

