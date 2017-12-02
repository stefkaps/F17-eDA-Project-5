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
#### good variables to exclude ####
dfE10c = dfE10 %>% dplyr::select(.,-cause_name,-cause_medium,-cause_short,-year,-age_name_unit,-age_name_from, -age_name_upto) %>% dplyr::mutate(sex_name = as.factor(sex_name)) %>% dplyr::mutate(region_name = as.factor(region_name))
dfE10b = dfE10 %>% dplyr::select(.,-cause_name,-cause_medium,-cause_short,-year,-age_name_unit,-age_name_from, -age_name_upto, -sex_name) %>% dplyr::mutate(region_binary = ifelse(region_name == "Eastern Europe", 1, 0)) %>% dplyr::select(.,-region_name) 
#%>% dplyr::mutate(region_binary = as.factor(region_binary))
trainE10 = sample(1:nrow(dfE10b), 10000)
testE10 = dfE10b[-trainE10,]


## Boosting 
boost.europe=gbm(region_binary~.,data=dfE10b[trainE10,],distribution="gaussian", n.trees=1000,shrinkage=0.01,interaction.depth = 4)
summary(boost.europe)

n.trees=seq(from=100,to=1000,by=100)
# use number of trees (going by hundreds) to make predictions
predmat=predict(boost.europe,newdata=dfE10b[-trainE10,],n.trees=n.trees)
dim(predmat)
# boosting error
berr=with(dfE10b[-trainE10,],apply( (predmat-region_binary)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error"); abline(h=min(berr),col="red")

# Logistic Regression
glm.fit.region=glm(region_binary~yll_rate_ui_upto+daly_rate_ui_upto+yld_abs_ui_from+daly_abs_ui_from+yld_rate_ui_upto,data=dfE10b,family=binomial,subset=trainE10)
summary(glm.fit.region)
glm.probs=predict(glm.fit.region,newdata=testE10,type="response")
glm.pred=ifelse(glm.probs>0.5,"1","0")
region.test = testE10$region_binary
table(glm.pred,region.test)
mean(glm.pred==region.test)


# LDA

lda.fit.region=lda(region_binary~yll_rate_ui_upto+daly_rate_ui_upto+yld_abs_ui_from+daly_abs_ui_from+yld_rate_ui_upto,data=dfE10b,subset=trainE10)
lda.fit.region
lda.pred=predict(lda.fit.region, testE10)
lda_df = data.frame(lda.pred)
table(lda.pred$class,testE10$region_binary) #confusion matrix
mean(lda.pred$class==testE10$region_binary) #bad mean

predroc2 <- prediction(lda_df$posterior.1,testE10$region_binary)
roc.perf = performance(predroc2, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)
auc1 = performance(predroc2, measure = "auc")
auc1@y.values


# QDA

qda.fit.region=qda(region_binary~yll_rate_ui_upto+daly_rate_ui_upto+yld_abs_ui_from+daly_abs_ui_from+yld_rate_ui_upto,data=dfE10b,subset=trainE10)

qda.fit.region
qda.pred = predict(qda.fit.region, testE10)
qda_df=data.frame(qda.pred)
table(qda.pred$class,testE10$region_binary)
mean(qda.pred$class==testE10$region_binary)

predroc3 <- prediction(qda_df$posterior.1,testE10$region_binary)
roc.perf2 = performance(predroc3, measure = "tpr", x.measure = "fpr")
plot(roc.perf2)
abline(a=0, b= 1)
auc2 = performance(predroc3, measure = "auc")
auc2@y.values

# KNN
dfE10b = dfE10 %>% dplyr::select(.,-cause_name,-cause_medium,-cause_short,-year,-age_name_unit,-age_name_from, -age_name_upto) %>% dplyr::mutate(sex_name = as.factor(sex_name)) %>% dplyr::mutate(region_binary = ifelse(region_name == "Eastern Europe", 1, 0)) %>% dplyr::select(.,-region_name)

dfE10bna = na.omit(dfE10b)
attach(dfE10bna)
trainE10 = sample(1:nrow(dfE10bna), 7000)
testE10 = dfE10bna[-trainE10,]

testE10knn = sample(1:nrow(testE10),7000)

predictorsKNN=cbind(yll_rate_ui_upto,daly_rate_ui_upto,yld_abs_ui_from,daly_abs_ui_from,yld_rate_ui_upto)
knn.pred=class::knn(predictorsKNN[trainE10,],predictorsKNN[testE10knn,],region_binary[trainE10],k=1)

table(knn.pred,region_binary[testE10knn])
mean(knn.pred==region_binary[testE10knn])

## K means
km_region_x = dfE10c%>% dplyr::select(yll_rate_ui_upto,daly_rate_ui_upto)
km.out=kmeans(km_region_x,2)
km.out
par(mfrow = c(1, 2))
plot(km_region_x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(km_region_x,col=as.factor(dfE10c$region_name),cex=2,main = "Actual",pch=1,lwd=2)

## Things to do:
"get KNN working
do K means to see if region clusters
Do Boosting with binary region to get error"


### Interpretation of predictors
"age range can be used to predict the quantitative variables because age should affect mortality. However,it might be difficult to use because it has so many categories.
Age range should be excluded when predicting the categorical variables because it appears the same number of times for every category by design. Likewise, each categorical variable is actually not designed to predict other categorical variables."