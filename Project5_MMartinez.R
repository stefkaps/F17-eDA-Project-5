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

##KNN Section##

#KNN Test Data#
set.seed(1)
train = sample(nrow(df_subset), 2937)
test = df[-train,]

test_knn = sample(nrow(test), 2937)

#KNN Analysis Build#
predictorsKNN=cbind(jitter, nhr)
knn.pred=class::knn(predictorsKNN[train, ],predictorsKNN[test_knn,],motor_updrs[train],k=1)
table(knn.pred,motor_updrs[test_knn])
mean(knn.pred==motor_updrs[test_knn])
