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
df_deathabs=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_deathabs=dplyr::sample_n(df_deathabs, 10000)

attach(df_deathabs)

regfit.full=regsubsets(death_abs~.,data=df_deathabs, nvmax=37)
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
df_deathabs=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_deathabs=dplyr::sample_n(df_deathabs, 10000)

attach(df_deathabs)

#Forward#
regfit.fwd=regsubsets(death_abs~.,data=df_deathabs,nvmax=37,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.max(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")

#forward region_name with LRI# Not using for now.
LRI <- c('Oth LRI', 'LRI-RSV', 'LRI-HiHB', 'LRI-Pneum', 'LRI-Flu')

df_LRI <- df %>% dplyr::filter(., cause_short %in% LRI)

attach(df_LRI)

regfit.fwd=regsubsets(as.factor(region_name)~.,data=df_LRI,nvmax=37,method="forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")
plot(regfit.fwd,scale="adjr2")

regfwd.summary=summary(regfit.fwd)

which.min(regfwd.summary$cp)
which.max(regfwd.summary$adjr2)

plot(regfwd.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfwd.summary$adjr2,xlab="Number of Variables",ylab="adjr2")


#Backward#
regfit.bwd=regsubsets(death_abs~.,data=df_deathabs,nvmax=37,method="backward")
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
df_deathabs=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_deathabs=dplyr::sample_n(df_deathabs, 10000)

#attach(df_deathabs)

df_deathabs_nona = na.omit(df_deathabs)
attach(df_deathabs_nona)

#Original Predictors#
fitDeathAbs = lm(death_abs~yll_abs_ui_from + yll_abs + yll_abs_ui_upto,data=df_deathabs)
fitDeathAbs
summary(fitDeathAbs)

#Updated Predictors#
fitDeathAbs = lm(death_abs~yll_abs_ui_from + yll_abs,data=df_deathabs)
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
df_deathabs=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
df_deathabs=dplyr::sample_n(df_deathabs, 10000)

df_deathabs_nona <- na.omit(df_deathabs)

attach(df_deathabs_nona)

x=model.matrix(death_abs~.-1,data=df_deathabs_nona) 

y=df_deathabs_nona$death_abs

fit.lasso=glmnet(x,y,family="gaussian")

plot(fit.lasso,xvar="lambda",label=TRUE) 

cv.lasso=cv.glmnet(x,y,family="gaussian")

plot(cv.lasso)

coef(cv.lasso)

#region_name section#
LRI <- c('Oth LRI', 'LRI-RSV', 'LRI-HiHB', 'LRI-Pneum', 'LRI-Flu')

df_LRI <- df %>% dplyr::filter(., cause_short %in% LRI)

df_LRI <- na.omit(df_LRI)

dfLRI_subset <- df_LRI %>% dplyr::select(., -cause_name, -cause_medium, -cause_name, -year, -age_name_unit, -sex_name)

attach(dfLRI_subset)

x=model.matrix(region_name~.-1,data=dfLRI_subset) 

y=dfLRI_subset$region_name

fit.lasso=glmnet(x,y,family="binomial")

plot(fit.lasso,xvar="lambda",label=TRUE) 

cv.lasso=cv.glmnet(x,y,family="binomial")

plot(cv.lasso)

coef(cv.lasso)

##Principal Components Section##
df_subset2 <- df %>% dplyr::select(., -cause_name, -cause_medium, -cause_short, -region_name, -year, -age_name_unit, -sex_name)
df_nona2 <- na.omit(df_subset2)
attach(df_nona2)

dimnames(df_nona2)
apply(df_nona2,2,mean)
apply(df_nona2,2, var)

pca.out=prcomp(df_nona2, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)

##Logistic Regression Section## Keep getting 0 for mean correct. 
df_subset <- df %>% dplyr::select(., -cause_medium, -cause_short, -region_name, -year, -age_name_unit, -sex_name)
df_nona <- na.omit(df_subset)
attach(df_nona)

set.seed(1)
train = sample(nrow(df_nona), 33346)
test = df_nona[-train,]

glm1.fit=glm(as.factor(cause_name) ~ age_name_from + death_abs + death_abs_ui_upto + death_pct_ui_from + death_pct_ui_upto + death_rate + death_rate_ui_from + yll_abs + yll_abs_ui_from + yll_abs_ui_upto + yll_rate + yll_rate_ui_from + yld_abs + yld_abs_ui_from + yld_pct + yld_pct_ui_from + yld_pct_ui_upto + yld_rate + yld_rate_ui_upto + daly_abs + daly_abs_ui_from + daly_abs_ui_upto + daly_pct + daly_rate + daly_rate_ui_from,
             data=df_nona, family=binomial,
             subset=train)
summary(glm1.fit)

glm1.probs=predict(glm1.fit,newdata=test,type="response")
glm1.pred=ifelse(glm1.probs>0.5,"1","0")
cause_name.test = test$cause_name
table(glm1.pred,cause_name.test)
mean(glm1.pred==cause_name.test)

##LDA Section## Not working 
lda1.fit=lda(cause_name ~ age_name_from + death_abs + death_abs_ui_upto + death_pct_ui_from + death_pct_ui_upto + death_rate + death_rate_ui_from + yll_abs + yll_abs_ui_from + yll_abs_ui_upto + yll_rate + yll_rate_ui_from + yld_abs + yld_abs_ui_from + yld_pct + yld_pct_ui_from + yld_pct_ui_upto + yld_rate + yld_rate_ui_upto + daly_abs + daly_abs_ui_from + daly_abs_ui_upto + daly_pct + daly_rate + daly_rate_ui_from,
             data=df_subset, subset=train)
lda1.fit
lda1.pred=predict(lda1.fit, test)
lda1_df = data.frame(lda1.pred)
table(lda1.pred$class,test$cause_name)
mean(lda1.pred$class==test$cause_name)

##QDA Section## Not working
qda1.fit = qda(cause_name ~ age_name_from + death_abs + death_abs_ui_upto + death_pct_ui_from + death_pct_ui_upto + death_rate + death_rate_ui_from + yll_abs + yll_abs_ui_from + yll_abs_ui_upto + yll_rate + yll_rate_ui_from + yld_abs + yld_abs_ui_from + yld_pct + yld_pct_ui_from + yld_pct_ui_upto + yld_rate + yld_rate_ui_upto + daly_abs + daly_abs_ui_from + daly_abs_ui_upto + daly_pct + daly_rate + daly_rate_ui_from,
               data=df_subset, subset=train)
qda1.fit
qda1.pred = predict(qda1.fit, test)
table(qda1.pred$class,test$cause_name)
mean(qda1.pred$class==test$cause_name)

##Random Forests Section##
df_deathabs=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
set.seed(11)
df_deathabs=dplyr::sample_n(df_deathabs, 10000)
df_deathabs_nona = na.omit(df_deathabs)
attach(df_deathabs_nona)

set.seed(101)
dim(df_deathabs_nona)
train=sample(1:nrow(df_deathabs_nona),4457)

rf.death=randomForest(death_abs~.,data=df_deathabs_nona,subset=train)
rf.death

oob.err=double(37)
test.err=double(37)
for(mtry in 1:37){
  fit=randomForest(death_abs~.,data=df_deathabs_nona,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df_deathabs_nona[-train,])
  test.err[mtry]=with(df_deathabs_nona[-train,],mean((death_abs-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))




