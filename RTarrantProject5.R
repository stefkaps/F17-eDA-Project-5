library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)
library(glmnet)
require(tree)
require(randomForest)
require(gbm)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OnRhcnJhbnRybCIsImlzcyI6ImFnZW50OnRhcnJhbnRybDo6MDE1OTQxYzQtNTUyZC00YjI3LWIxNGEtYzllN2ExMjYxN2FiIiwiaWF0IjoxNTA1MzEzMjAyLCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.vWrAbNkyU0mhgsdXXL-bxESWzppmpm8wguw9uI7pJ64ZsDtovi8kbWbPYS5pPcX8DDnVMuYxJJWHhqdxv--R_w"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/kellyjennings/disease"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM GlobalBurdenofDisease_Europe"),
  dataset = project
)

summary(df)
attach(df)
#head(df)

dim(df)
# 74595 rows, 45 attributes
sample=sample(1:nrow(df),7000)

df_pairs <- df %>% dplyr::select(., -cause_name, -cause_medium, -cause_short, -region_name, -age_name_unit, -sex_name)
dim(df_pairs)
pairs(df_pairs[sample,])

df_death <- df %>% dplyr::select(death_abs, death_abs_ui_from, death_abs_ui_upto, death_pct, death_pct_ui_from, death_pct_ui_upto, death_rate, death_rate_ui_from, death_rate_ui_upto)
pairs(df_death[sample,])

df_yll <- df %>% dplyr::select(yll_abs, yll_abs_ui_from, yll_abs_ui_upto, yll_pct, yll_pct_ui_from, yll_pct_ui_upto, yll_rate, yll_rate_ui_from, yll_rate_ui_upto, yld_abs)
pairs(df_yll[sample,])

df_yld <- df %>% dplyr::select(yld_abs, yld_abs_ui_from, yld_abs_ui_upto, yld_pct, yld_pct_ui_from, yld_pct_ui_upto, yld_rate, yld_rate_ui_from, yld_rate_ui_upto)
pairs(df_yld[sample,])

df_daly <- df %>% dplyr::select(daly_abs_ui_from, daly_abs_ui_upto, daly_pct, daly_pct_ui_from, daly_pct_ui_upto, daly_rate, daly_rate_ui_from, daly_rate_ui_upto)
pairs(df_daly[sample,])


x = df %>% dplyr::select(cause_name, yll_abs, death_abs)
dim(x)
x = x[sample,]
dim(x)
x_x = x %>% dplyr::select(yll_abs, death_abs)
plot(x_x, col=as.factor(x$cause_name),cex=2,main="Actual", pch=1,lwd=2)

project <- "https://data.world/kellyjennings/disease"
df10c <- data.world::query(
  data.world::qry_sql("SELECT * FROM disease2010cancer"),
  dataset = project
)

attach(df10c)
df10c2 = df10c %>% dplyr::select(.,-cause_medium,-cause_short,-region_name,-year,-age_name_unit) %>% dplyr::mutate(sex_name = as.factor(sex_name))
x = df10c %>% dplyr::select(yll_abs, death_abs) 
plot(x, col=as.factor(df10c$cause_name),cex=2,pch=1,lwd=2)
head(df10c)
dim(df10c2)
train = sample(1:nrow(df10c2),7000)
boost.cancer=gbm(cause_name~.,data=df10c2[train,],distribution="multinomial", n.trees=1000,shrinkage=0.01,interaction.depth = 4)
summary(boost.cancer)

df10c2test = df10c2[-train,]
test = sample(1:nrow(df10c2test),7000)

n.trees=seq(from=100,to=1000,by=100)
# use number of trees (going by hundreds) to make predictions
predmat=predict(boost.cancer,newdata=df10c2test[test,],n.trees=n.trees)
dim(predmat)
# boosting error
berr=with(df10c2test[test,],apply( (predmat-cause_name)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error"); abline(h=min(berr),col="red")

project <- "https://data.world/kellyjennings/disease"
dfE10 <- data.world::query(
  data.world::qry_sql("SELECT * FROM diseaseEurope2010"),
  dataset = project
)

head(dfE10)
dfE10c = dfE10 %>% dplyr::select(.,-cause_name,-cause_medium,-cause_short,-year,-age_name_unit,-age_name_from, -age_name_upto) %>% dplyr::mutate(sex_name = as.factor(sex_name)) %>% dplyr::mutate(region_name = as.factor(region_name))
dfE10b = dfE10 %>% dplyr::select(.,-cause_name,-cause_medium,-cause_short,-year,-age_name_unit,-age_name_from, -age_name_upto) %>% dplyr::mutate(sex_name = as.factor(sex_name)) %>% dplyr::mutate(region_binary = ifelse(region_name == "Eastern Europe", 1, 0)) %>% dplyr::select(.,-region_name)
trainE10 = sample(1:nrow(dfE10b), 10000)

boost.europe=gbm(region_name~.,data=dfE10c[trainE10,],distribution="gaussian", n.trees=10000,shrinkage=0.01,interaction.depth = 4)
summary(boost.europe)

# Logistic Regression
trainE10 = sample(1:nrow(dfE10b), 10000)
glm.fit.region=glm(region_binary~.,data=dfE10b,family=binomial,subset=trainE10)
summary(glm.fit.region)

glm1.fit=glm(sex2 ~ age + rpde + ppe + total_updrs,
             data=df, family=binomial,
             subset=train)
summary(glm1.fit)

glm1.probs=predict(glm1.fit,newdata=test,type="response")
glm1.pred=ifelse(glm1.probs>0.5,"1","0")
sex2.test = test$sex2
table(glm1.pred,sex2.test) #confusion matrix
mean(glm1.pred==sex2.test) #bad mean

# LDA
#1 age+rpde+ppe+total_updrs predicting sex2
lda1.fit=lda(sex2 ~ age + rpde + ppe + total_updrs,
             data=df, subset=train)
lda1.fit
lda1.pred=predict(lda1.fit, test)
lda1_df = data.frame(lda1.pred)
table(lda1.pred$class,test$sex2) #confusion matrix
mean(lda1.pred$class==test$sex2) #bad mean

# QDA
#1 age+rpde+ppe+total_updrs predicting sex2
qda1.fit = qda(sex2 ~ age + rpde + ppe + total_updrs,
               data=df, subset=train)
qda1.fit
qda1.pred = predict(qda1.fit, test)
table(qda1.pred$class,test$sex2)
mean(qda1.pred$class==test$sex2)


# KNN
predictorsKNN=cbind(age, motor_updrs, total_updrs, jitter, jitter_abs, jitter_ppq5, rpde, dfa, ppe)
knn7.pred=class::knn(predictorsKNN7[train, ],predictorsKNN7[test_knn,],sex2[train],k=1)
table(knn7.pred,sex2[test_knn])
mean(knn7.pred==sex2[test_knn])