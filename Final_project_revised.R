library(tidyverse)
library(dplyr)
library(MASS)
library(caTools)
library(lmtest)
library(caret)
library(ROCR)
library(interactions)
library(ggplot2)
library(ResourceSelection)

setwd("~/Langara/Sem 3/DANA-4820/project")
df <- read.csv('bank_data.csv')
View(df)

#as.factor categorical variable and response variable
df$y <- as.factor(df$y)
df$marital <- as.factor(df$marital)
df$default <- as.factor(df$default)
df$housing <- as.factor(df$housing)
df$loan <- as.factor(df$loan)
df$poutcome <- as.factor(df$poutcome)
df$education <- as.factor(df$education)
df$job <- as.factor(df$job)


#summary
summary(df)

#4.1 t test on age, balance, campaign, pdays, previous

#1. check variance
###var.test(group1, group2) #if p-value less then H0 rejected - variance of 2 groups not equal
###H(0) = Variances of group A is equal to variance of group B
###H(a) = Variance of group A is not equal to variance of group B
#2. t_test
###H(0) = The difference in group means is zero
###H(a) = The difference in group means is different from zero


group1_bal <- df$balance[df$y == 'yes']
group2_bal <- df$balance[df$y == 'no']
var.test(group1_bal, group2_bal) ###p value very less hence H0 rejected - variance of 2groups not equal
t.test(group1_bal, group2_bal, var.equal=F) ###p value very less hence H0 rejected - means of 2groups not equal


group1_age <- df$age[df$y == 'yes']
group2_age <- df$age[df$y == 'no']
var.test(group1_age, group2_age) ###p value very less hence H0 rejected - variance of 2groups not equal
t.test(group1_age, group2_age, var.equal=F) ###p value less hence H0 rejected - means of 2 groups not equal


group1_camp <- df$campaign[df$y == 'yes']
group2_camp <- df$campaign[df$y == 'no']
var.test(group1_camp, group2_camp) ###p value very less hence H0 rejected - variance of 2groups not equal
t.test(group1_camp, group2_camp, var.equal=F) ###p value very less hence H0 rejected - means of 2groups not equal


group1_pdays <- df$pdays[df$y == 'yes']
group2_pdays <- df$pdays[df$y == 'no']
var.test(group1_pdays, group2_pdays) ###p value very less hence H0 rejected - variance of 2groups not equal
t.test(group1_pdays, group2_pdays, var.equal=F) ###p value very less hence H0 rejected - means of 2groups not equal


group1_prev <- df$previous[df$y == 'yes']
group2_prev <- df$previous[df$y == 'no']
var.test(group1_prev, group2_prev) ###p value very less hence H0 rejected - variance of 2groups not equal
t.test(group1_prev, group2_prev, var.equal=F) ###p value very less hence H0 rejected - means of 2groups not equal


#4.2 chi-square test- Categorical variable: marital, default, housing, loan, poutcome, education, job
# It is generally to check association between explanatory and response variable
# hence lets check between housing and loan, housing and y, loan and y
# Hyposthesis
#H(0) = The variables are independent of each other
#H(a) = The variables are not independent of each other - associated

#marital
chisq.test(df$marital,df$y) ###p value less then 0.05 hence H0 rejected marital and y are not independent

#default
chisq.test(df$default,df$y) ###p value greater then 0.05 hence H0 accepted default and y are independent

#housing_loan
chisq.test(df$housing,df$y) ###p value less then 0.05 hence H0 rejected housing and y are not independent

#personal_loan
chisq.test(df$loan,df$y) ###p value less then 0.05 hence H0 rejected loan and y are not independent

#poutcome
chisq.test(df$poutcome,df$y) ###p value less then 0.05 hence H0 rejected loan and y are not independent

#education
chisq.test(df$education,df$y) ###p value less then 0.05 hence H0 rejected loan and y are not independent

#job
chisq.test(df$job,df$y) ###p value less then 0.05 hence H0 rejected loan and y are not independent

#month
chisq.test(df$month,df$y) ###p value less then 0.05 hence H0 rejected loan and y are not independent

#day
chisq.test(df$day,df$y) ###p value less then 0.05 hence H0 rejected loan and y are not independent

#######Now based on chisq test and t.test only "default" variable is insignificant####### 

#5.1 correlation check
nums <- sapply(df, is.numeric)
numvar <- names(nums[nums == T])
cor(df[,numvar])
corrplot::corrplot(cor(df[,numvar]))
### we can see slight correlation between pdays and previous but not greater than 0.8


#5.2 Multicollinearity check
model <- glm(y~.,family=binomial(link='logit'),data = df)
car::vif(model) 
### no Multicollinearity issue nothing greater than 10

###So the only insignificant variable to drop is only "default"
#dropping insignificant variable
df$default <- NULL

#6. Interaction plot

#interaction.plot(df$education,df$y,df$balance,median)
#check if deposit and education have interaction

interaction.plot(x.factor = df$education, #x-axis variable
                 trace.factor = df$y, #variable for lines
                 response = df$balance, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Account_balance",
                 xlab = "Education_level",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Balance")

str(df)

interaction.plot(x.factor = df$loan, #x-axis variable
                 trace.factor = df$y, #variable for lines
                 response = df$age, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Age",
                 xlab = "loan",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "age")

interaction.plot(x.factor = df$housing, #x-axis variable
                 trace.factor = df$y, #variable for lines
                 response = df$age, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Age",
                 xlab = "housingloan",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "age")

df$balance= as.numeric(df$balance)

fit <- glm(y ~ marital * loan, family ="binomial", data = df)
summary(fit)
cat_plot(fit, pred = loan, modx = marital,geom = "line", interval = TRUE) ###clear interaction

fit2 <- glm(y ~ education * job, family ="binomial", data = df)
cat_plot(fit2, pred = job, modx = education, geom = "line", interval = TRUE) ###high interaction

fit3 <- glm(y ~ education * marital, family ="binomial", data = df)
cat_plot(fit3, pred = marital, modx = education, geom = "line", interval = TRUE) ###high interaction

#7. data split
set.seed(1234) # is used so that each time we get the same data set after splitting
sample_size<- sample.split(df$y, SplitRatio = 7/10) #Splitting the dataset into 70/30 ratio 
train <- subset(df, sample_size==T) 
test <- subset(df, sample_size==F) 
nrow(train) #3165
nrow(test) #1356
  
##8. STEPWISE REGRESSION STARTS HERE...

# Full model ref-https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4842399/
fullmodel <- glm(y ~., data = train, family = binomial)
summary(fullmodel)

step <- stepAIC(fullmodel,trace = F)

step$anova

#final selected variables are: Final Model-> y ~ age + marital + loan + month + campaign + poutcome

#9.comparing two models final model and final model + potential interaction
#comparison using Likelihood ratio test ref-https://api.rpubs.com/tomanderson_34/lrt
#H(0) <- reduced model is better -model1
#H(a) <- full model is better -model2
model1 <- glm(y ~ age + marital + loan + month + campaign + poutcome, data = train, family = binomial)
summary(model1)

model2 <- glm(y ~ age + marital + loan + month + campaign + poutcome + loan*marital, data = train, family = binomial)
summary(model2)

A <- logLik(model1)
B <- logLik(model2)

teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val <- pchisq(teststat, df = 2, lower.tail = FALSE) #0.5160002

##or simply 
lrtest(model1,model2) #pvalue 0.516 cant reject null hypothesis. Hence reduced model is better

#------------------comparison using Wald test----------------------------------------------------------
#comparison using Likelihood ratio test 
#H(0) <- reduced model is better -model1
#H(a) <- full model is better -model2
waldtest(model1,model2) #pvalue 0.5454 cant reject null hypothesis. Hence reduced model is better
#Wald test

#Model 1: y ~ age + marital + loan + month + campaign + poutcome
#Model 2: y ~ age + marital + loan + month + campaign + poutcome + loan * marital
#Res.Df Df      F Pr(>F)
#1   3145                 
#2   3143  2 0.6063 0.5454

###10.classification report for both models ref- https://daviddalpiaz.github.io/r4sl/logistic-regression.html

#Predictions and classification report of model1
predictions_test1 <- predict(model1, train)
summary(predictions_test1)

pred1<- ifelse(predictions_test1>0.5, "yes","no")
Conf.mat_test1<- confusionMatrix(table(pred1, train$y), positive = "yes")
c(Conf.mat_test1$overall["Accuracy"], 
  Conf.mat_test1$byClass["Sensitivity"], 
  Conf.mat_test1$byClass["Specificity"])

#Accuracy Sensitivity Specificity 
#0.88941548  0.09863014  0.99250000 

#Predictions and classification report of model2
predictions_test2 <- predict(model2, train)
predictions_test2

pred2<- ifelse(predictions_test2>0.5, "yes","no")
Conf.mat_test2<- confusionMatrix(table(pred2, train$y), positive = "yes")
Conf.mat_test2
c(Conf.mat_test2$overall["Accuracy"], 
  Conf.mat_test2$byClass["Sensitivity"], 
  Conf.mat_test2$byClass["Specificity"])

#Accuracy Sensitivity Specificity 
#0.8900474   0.1013699   0.9928571

###11.ROC Curve for both models
#model1

library(ROCit)
library(dlstats)    # for package download stats
library(pkgsearch) 


rocpred1<- prediction(predictions_test1, train$y)
perf <- performance(rocpred1,"tpr","fpr")
plot(perf,colorize=TRUE)

#model2
rocpred2<- prediction(predictions_test2, train$y)
perf2 <- performance(rocpred2,"tpr","fpr")
plot(perf2,colorize=TRUE)

###12.Lack of fit test (Hosmer-Lemshow test- ungrouped data) and Interpret the coefficient.

#The Hosmer-Lemeshow test involves a few steps.
# The steps are:
#1. Estimate the probability of yi = 1 for each observation.
#2. Sort the observations into groups (usually 10) with increasing probability.
#3. For each group compute the number of observations multiplied by the average probability. This gives the expected number of yi = 1'sin this group.
#4. Compute the expected number of yi = 0's too
#5. Compute a ??2 test statistic
#6. This test statistic is ??2 10???2 distributed

hl1 <- hoslem.test(model1$y, fitted(model1),g=10)
hl1

#data:  model1$y, fitted(model1)
#X-squared = 10.296, df = 8, p-value = 0.2448

#This gives p=0.25, indicating no evidence of poor fit. 
#This is good, since here we know the model is indeed correctly specified. 
#We can also obtain a table of observed vs expected, from our hl object:

cbind(hl1$observed,hl1$expected)
#                  y0  y1  yhat0      yhat1
#[0.00934,0.0401] 304  13 307.5053   9.494663
#(0.0401,0.0497]  304  12 301.5608  14.439235
#(0.0497,0.0616]  304  14 300.4608  17.539207
#(0.0616,0.0726]  292  23 293.8767  21.123277
#(0.0726,0.0847]  294  24 292.9424  25.057567
#(0.0847,0.0971]  295  20 286.5135  28.486500
#(0.0971,0.112]   278  38 283.2184  32.781596
#(0.112,0.136]    286  31 277.8747  39.125322
#(0.136,0.185]    259  57 267.1269  48.873063
#(0.185,0.943]    184 133 188.9204 128.079573


# Similarly for MODEL 2
hl2 <- hoslem.test(model2$y, fitted(model2),g=10)
#data:  model2$y, fitted(model2)
#X-squared = 9.493, df = 8, p-value = 0.3024

#This gives p=0.3, indicating no evidence of poor fit. 
#This is good, since here we know the model is indeed correctly specified. 
#We can also obtain a table of observed vs expected, from our hl object:

cbind(hl2$observed,hl2$expected)
#               y0  y1    yhat0      yhat1
#[0.0101,0.0414] 303  15 308.0516   9.948367
#(0.0414,0.0488] 306   9 300.6434  14.356605
#(0.0488,0.0599] 305  13 300.8052  17.194772
#(0.0599,0.0718] 292  23 294.2112  20.788829
#(0.0718,0.084]  293  25 293.1938  24.806241
#(0.084,0.0961]  290  25 286.7295  28.270503
#(0.0961,0.111]  283  33 283.3865  32.613459
#(0.111,0.137]   285  32 277.7679  39.232053
#(0.137,0.187]   260  56 266.6855  49.314541
#(0.187,0.946]   183 134 188.5254 128.474632


#final logistic regression (conclusion)
model <- glm(y ~ age + marital + loan + month + campaign + poutcome, data = train, family = binomial)
summary(model)


