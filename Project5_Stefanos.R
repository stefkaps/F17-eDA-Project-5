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



### Lasso
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

