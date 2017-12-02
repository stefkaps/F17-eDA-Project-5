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


### Boosting 

#Boosting for yll_abs
dfb=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -yll_abs_ui_upto, -yll_abs_ui_from)
dfb=dplyr::sample_n(dfb, 10000)
boost.df=gbm(yll_abs~.,data=dfb,distribution="gaussian",n.trees=1000,shrinkage=0.01,interaction.depth=4)
summary(boost.df)

#Boosting  for yld_abs
dfb2=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -yld_abs_ui_upto, -yld_abs_ui_from)
dfb2=dplyr::sample_n(dfb2, 10000)
boost.df=gbm(yld_abs~.,data=dfb2,distribution="gaussian",n.trees=1000,shrinkage=0.01,interaction.depth=4)
summary(boost.df)




### Lasso

#Lasso for yll_abs
df1=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -yll_abs_ui_upto, -yll_abs_ui_from)
df1=dplyr::sample_n(df1, 10000)

df1_nona <- na.omit(df1)

attach(df1_nona)

# for this you have to pass in a matrix of x. You have to construct the x's
x=model.matrix(yll_abs~.-1,data=df1_nona) 
# and construct the y's
y=df1_nona$yll_abs

fit.lasso=glmnet(x,y,family="gaussian")
# plot how lambda is changing against the model fit
plot(fit.lasso,xvar="lambda",label=TRUE) # the one that disappears last is our best predictor
# do cross validation for each lambda (default is kfold of 10 across 100 lambdas)
cv.lasso=cv.glmnet(x,y,family="gaussian")
# plot the cross validation mean squared errors of all 100 models
plot(cv.lasso)
# get the coefficients for what it thinks is the best model
coef(cv.lasso)


#Lasso for yld_abs
dfb2=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -yld_abs_ui_upto, -yld_abs_ui_from)
dfb2=dplyr::sample_n(dfb2, 10000)

dfb2_nona <- na.omit(dfb2)

attach(dfb2_nona)

# for this you have to pass in a matrix of x. You have to construct the x's
x=model.matrix(yld_abs~.-1,data=dfb2_nona) 
# and construct the y's
y=dfb2_nona$yld_abs

fit.lasso=glmnet(x,y,family="gaussian")
# plot how lambda is changing against the model fit
plot(fit.lasso,xvar="lambda",label=TRUE) # the one that disappears last is our best predictor
# do cross validation for each lambda (default is kfold of 10 across 100 lambdas)
cv.lasso=cv.glmnet(x,y,family="gaussian")
# plot the cross validation mean squared errors of all 100 models
plot(cv.lasso)
# get the coefficients for what it thinks is the best model
coef(cv.lasso)



### Linear Regression
#yld_abs w/ top 5 boosting predictors
fit_yld_abs = lm(yld_abs~daly_abs + daly_abs_ui_upto + age_name_from + yll_abs_ui_upto + yld_rate_ui_from,data=dfb2_nona)
fit_yld_abs
summary(fit_yld_abs)

#yld_abs w/ Lasso predictors
fit_yld_abs = lm(yld_abs~death_abs + death_rate_ui_from + yll_abs + yll_abs_ui_from + yll_abs_ui_upto + yld_pct_ui_upto + yld_rate_ui_from + daly_abs + daly_abs_ui_upto,data=dfb2_nona)
fit_yld_abs
summary(fit_yld_abs)



### Best Subset Selection

#Best Subset Selection for yll_abs
df2=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -yll_abs_ui_upto, -yll_abs_ui_from, -yll_abs)
df2=dplyr::sample_n(df2, 10000)

attach(df2)

regfit.full=regsubsets(yll_abs~.,data=df2, nvmax=37)
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



###Forwards and Backwards Selection Section

#Forward#
regfit.fwd=regsubsets(yll_abs~.,data=df2,nvmax=37,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.max(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

#Backward#
regfit.bwd=regsubsets(yll_abs~.,data=df2,nvmax=37,method="backward")
summary(regfit.bwd)

plot(regfit.bwd,scale="Cp")
plot(regfit.bwd,scale="adjr2")

regbwd.summary=summary(regfit.bwd)

which.min(regbwd.summary$cp)
which.max(regbwd.summary$adjr2)

plot(regbwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regbwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")



### Random Forest yll_abs
df3=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -yll_abs_ui_upto, -yll_abs_ui_from)
df3=dplyr::sample_n(df3, 10000)

df3_nona = na.omit(df3)

attach(df3_nona)

set.seed(101)
dim(df3_nona)
train=sample(1:nrow(df3_nona),4457)

rf.yll_abs=randomForest(yll_abs~.,data=df3_nona,subset=train)
rf.yll_abs

oob.err=double(37)
test.err=double(37)
for(mtry in 1:37){
  fit=randomForest(yll_abs~.,data=df3_nona,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df3_nona[-train,])
  test.err[mtry]=with(df3_nona[-train,],mean((yll_abs-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))



### 

project <- "https://data.world/kellyjennings/disease"
df10c <- data.world::query(
  data.world::qry_sql("SELECT * FROM disease2010cancer"),
  dataset = project
)

attach(df10c)
df10c = df10c %>% dplyr::select(.,-cause_medium,-cause_short,-region_name,-year,-age_name_unit) %>% dplyr::mutate(sex_name = as.factor(sex_name))
df10c_nona = na.omit(df10c)

View(df10c)

# for this you have to pass in a matrix of x. You have to construct the x's
x=model.matrix(cause_name~.-1,data=df10c_nona) 
# and construct the y's
y=df10c_nona$cause_name

fit.lasso=glmnet(x,y,family="multinomial")
# plot how lambda is changing against the model fit
plot(fit.lasso,xvar="lambda",label=TRUE) # the one that disappears last is our best predictor
# do cross validation for each lambda (default is kfold of 10 across 100 lambdas)
cv.lasso=cv.glmnet(x,y,family="multinomial")
# plot the cross validation mean squared errors of all 100 models
plot(cv.lasso)
# get the coefficients for what it thinks is the best model
coef(cv.lasso)
