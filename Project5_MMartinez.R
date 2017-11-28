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

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/kellyjennings/disease"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM GlobalBurdenofDisease_Europe"),
  dataset = project
)

summary(df)

attach(df)

##Best Subset Selection Section##
df_subset <- df %>% dplyr::select(., -cause_medium, -cause_short, -region_name, -year, -age_name_unit, -sex_name)
attach(df_subset)

regfit.full=regsubsets(as.factor(cause_name)~.,data=df_subset, nvmax=44)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
which.max(reg.summary$adjr2)

summary(regfit.full)

plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
coef(regfit.full,10)

#deathabs section#
df_numeric=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_numeric=dplyr::sample_n(df_numeric, 10000)

attach(df_numeric)

regfit.full=regsubsets(death_abs~.,data=df_numeric, nvmax=37)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
which.max(reg.summary$adjr2)

summary(regfit.full)

plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
coef(regfit.full,10)

#region_name section# too big???
regfit.full=regsubsets(as.factor(region_name)~.,data=df, nvmax=37)
reg.summary=summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
which.max(reg.summary$adjr2)

summary(regfit.full)

plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")
coef(regfit.full,10)

##Forwards and Backwards Selection Section##
df_numeric=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_numeric=dplyr::sample_n(df_numeric, 10000)

attach(df_numeric)

#Forward#
regfit.fwd=regsubsets(death_abs~.,data=df_numeric,nvmax=37,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.max(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

#Backward#
regfit.bwd=regsubsets(death_abs~.,data=df_numeric,nvmax=37,method="backward")
summary(regfit.bwd)

plot(regfit.bwd,scale="Cp")
plot(regfit.bwd,scale="adjr2")

regbwd.summary=summary(regfit.bwd)

which.min(regbwd.summary$cp)
which.max(regbwd.summary$adjr2)

plot(regbwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regbwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

##KNN Section##

#KNN Test Data#
df_nona <- na.omit(df_subset)
attach(df_nona)

set.seed(1)
train = sample(nrow(df_nona), 33346)
test = df_nona[-train,]

test_knn = sample(nrow(test), 33346)

#KNN Analysis Build#
predictorsKNN=cbind(age_name_from, death_abs, death_abs_ui_upto, death_pct_ui_from, death_pct_ui_upto, death_rate, death_rate_ui_from, yll_abs, yll_abs_ui_from, yll_abs_ui_upto, yll_rate, yll_rate_ui_from, yld_abs, yld_abs_ui_from, yld_pct, yld_pct_ui_from, yld_pct_ui_upto, yld_rate, yld_rate_ui_upto, daly_abs, daly_abs_ui_from, daly_abs_ui_upto, daly_pct, daly_rate, daly_rate_ui_from)

knn.pred=class::knn(predictorsKNN[train, ],predictorsKNN[test_knn,],cause_name[train],k=1)
table(knn.pred,cause_name[test_knn])
mean(knn.pred==cause_name[test_knn])

##Linear Regression Section##
df_numeric=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_numeric=dplyr::sample_n(df_numeric, 10000)

#attach(df_numeric)

df_numeric_nona = na.omit(df_numeric)
attach(df_numeric_nona)

#Original Predictors#
fitDeathAbs = lm(death_abs~yll_abs_ui_from + yll_abs + yll_abs_ui_upto,data=df_numeric)
fitDeathAbs
summary(fitDeathAbs)

#Updated Predictors#
fitDeathAbs = lm(death_abs~yll_abs_ui_from + yll_abs,data=df_numeric)
fitDeathAbs
summary(fitDeathAbs)

#yll_abs#
df_yll_abs=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit)
df_yll_abs=dplyr::sample_n(df_yll_abs, 10000)
attach(df_yll_abs)
df_nona_yll = na.omit(df_yll_abs)
attach(df_nona_yll)

fitYLLAbs = lm(yll_abs~death_abs_ui_upto + daly_abs + daly_abs_ui_upto + daly_abs_ui_from,data=df_yll_abs)
fitYLLAbs
summary(fitYLLAbs)

##Lasso Section##
df_numeric=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_numeric=dplyr::sample_n(df_numeric, 10000)

df_numeric_nona <- na.omit(df_numeric)

attach(df_numeric_nona)

x=model.matrix(death_abs~.-1,data=df_numeric_nona) 

y=df_numeric_nona$death_abs

fit.lasso=glmnet(x,y,family="gaussian")

plot(fit.lasso,xvar="lambda",label=TRUE) 

cv.lasso=cv.glmnet(x,y,family="gaussian")

plot(cv.lasso)

coef(cv.lasso)






