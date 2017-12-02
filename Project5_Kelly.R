require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)
library(glmnet)
require(tree)
require(randomForest)
require(gbm)
require(e1071)
require(ROCR)
library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/kellyjennings/disease"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM GlobalBurdenofDisease_Europe"),
  dataset = project
)

### boosting death_abs
dfb=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
dfb=dplyr::sample_n(dfb, 3000)
boost.df=gbm(death_abs~.,data=dfb,distribution="gaussian",n.trees=500,shrinkage=0.01,interaction.depth=4)
summary(boost.df)

### boosting sex
dfb2=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -age_name_unit, -age_name_from, -age_name_upto) 
dfb2 = dfb2 %>% dplyr::filter(sex_name %in% c("Male", "Female"))
dfb2=dplyr::sample_n(dfb2, 3000)
boost.df=gbm(as.factor(sex_name)~.,data=dfb2,distribution="gaussian",n.trees=500,shrinkage=0.01,interaction.depth=4)
summary(boost.df)

# Logistic Regression
set.seed(1)
df_binsex = df %>% dplyr::filter(sex_name %in% c("Male", "Female")) %>% dplyr::mutate(sexbin = ifelse(sex_name == "Male", 1, 0))
train = sample(nrow(df_binsex), 25000)
test = df_binsex[-train,]
lda.fit=lda(sexbin~yll_rate_ui_upto+death_pct_ui_upto+yll_pct_ui_upto+yll_abs_ui_upto+yld_abs_ui_upto+yll_rate+daly_rate_ui_upto+yll_rate_ui_from+death_abs_ui_upto+yll_abs+daly_pct_ui_upto+yll_abs_ui_from+yll_pct+death_abs+death_rate_ui_from+daly_rate_ui_from,data=df_binsex, subset=train)
lda.fit
plot(lda.fit)
lda.predsex=predict(lda.fit,test)
df_lda = data.frame(lda.predsex)
ggplot(df_lda) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class)
ggplot(df_lda) + geom_boxplot(mapping = aes(x=class, y=LD1))
table(lda.predsex$class,test$sexbin)
mean(lda.predsex$class==test$sexbin)
# change threshold
df_conf = dplyr::bind_cols(test, df_lda)
df_ldaclass = df_conf %>% dplyr::mutate(newClass = ifelse(posterior.1 > .49, 1, 0))
table(df_ldaclass$newClass,test$sexbin)
mean(df_ldaclass$newClass==test$sexbin)
#roc curve
predroc <- prediction(df_conf$posterior.1,df_conf$sexbin)
roc.perf1 = performance(predroc, measure = "tpr", x.measure = "fpr")
plot(roc.perf1)
abline(a=0, b= 1)
auc = performance(predroc, measure = "auc")
auc@y.values

# k means clustering

dfe_heart = df %>% dplyr::filter(region_name %in% c("Eastern Europe", "Western Europe") & cause_name %in% c("Ischemic heart disease", "Ischemic stroke") & age_name_unit == "years")
x_km = dfe_heart%>%dplyr::select(yld_rate, yll_rate)
km.out=kmeans(x_km,2)
km.out
par(mfrow = c(1, 2))
plot(x_km,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x_km,col=as.factor(dfe_heart$region_name),cex=2,main = "Actual",pch=1,lwd=2)
# ggplot alternative:
km.out$cluster
dfcluster <- data.frame(x_km, km.out$cluster)
names(dfcluster)
dfcluster2 <- dplyr::bind_cols(dfcluster, data.frame(dfe_heart$region_name))

kmeansubject <- dfcluster2 %>% ggplot(aes(x=yld_rate, y=yll_rate, colour = as.factor(km.out.cluster))) + geom_point()
actualsubject <- dfcluster2 %>% ggplot(aes(x=yld_rate, y=yll_rate, colour = as.factor(dfe_heart.region_name))) + geom_point()

df1 <- data.frame(km.out$centers, km.out$size)
#names(df1)
dfcluster2 %>% ggplot() + geom_point(mapping = aes(x=yld_rate, y=yll_rate, colour = as.factor(km.out.cluster))) + geom_point(data=df1, mapping=aes(yld_rate, yll_rate, size=km.out.size))

dfe_heart %>% ggplot(aes(x=yld_rate, y = yll_rate, colour = region_name)) + geom_point()

# hierchical 
hc.complete=hclust(dist(x_km),method="complete")
hc.single=hclust(dist(x_km),method="single")
hc.average=hclust(dist(x_km),method="average")
par(mfrow = c(3, 1))
plot(hc.complete)
plot(hc.single)
plot(hc.average)

hc.cut1=cutree(hc.complete,2)
hc.cut2=cutree(hc.single,2)
hc.cut3=cutree(hc.average,2)

table(hc.cut1,dfe_heart$region_name)
table(hc.cut1,km.out$cluster)

table(hc.cut2,dfe_heart$region_name)
table(hc.cut2,km.out$cluster)

table(hc.cut3,dfe_heart$region_name)
table(hc.cut3,km.out$cluster)

par(mfrow = c(1, 3))
plot(x_km,col=hc.cut1,cex=2,main = "Hierarchical: Complete", pch=1,lwd=2)
plot(x_km,col=hc.cut2,cex=2,main = "Hierarchical: Single", pch=1,lwd=2)
plot(x_km,col=hc.cut3,cex=2,main = "Hierarchical: Average", pch=1,lwd=2)

##### svm
dfe_ms = df %>% dplyr::filter(region_name %in% c("Eastern Europe", "Western Europe") & sex_name %in% c("Male", "Female") & cause_name %in% c("Multiple sclerosis") & age_name_unit == "years")

x_try=subset(dfe_ms, select = c(death_rate, yld_rate))
x_svm=matrix(unlist(x_try), ncol = 2)
y_svm=dfe_ms$sex_name
dat=data.frame(x_svm,y_svm)
svmfit=svm(y_svm~.,data=dat,type="C-classification",kernel="linear",cost=10,scale=FALSE)
print(svmfit)
plot(svmfit,dat)
make.grid=function(x,n=100){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
}
xgrid=make.grid(x_svm)
ygrid=predict(svmfit,xgrid)

beta=drop(t(svmfit$coefs)%*%x_svm[svmfit$index,])
beta0=svmfit$rho
plot(xgrid,col=c("yellow","blue")[as.numeric(ygrid)],pch=20,cex=.2)
tonum=function(ysvm){
  tonumberdf = dfe_ms %>% dplyr::mutate(sex_name2 = ifelse(sex_name == "Male", 1, 0))
  tonumberdf$sex_name2
}
points(x_svm,col=tonum(df)+2,pch=19)
points(x_svm[svmfit$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2])
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)

x_try2=subset(dfe_ms, select = c(yld_rate, death_rate))
x_svm2=matrix(unlist(x_try2), ncol = 2)
dat2=data.frame(x_svm2,y_svm)
nlsvmfit=svm(y_svm~.,data=dat2,type="nu-classification",scale=TRUE,kernel="radial",cost=10)
plot(nlsvmfit, dat2)




