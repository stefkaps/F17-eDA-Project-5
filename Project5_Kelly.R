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


dfb=dplyr::select(df, -cause_name, -cause_medium, -cause_short, -region_name, -sex_name, -age_name_unit, -death_abs_ui_upto, -death_abs_ui_from)
dfb=dplyr::sample_n(dfb, 3000)
boost.df=gbm(death_abs~.,data=dfb,distribution="gaussian",n.trees=500,shrinkage=0.01,interaction.depth=4)
summary(boost.df)

# k means clustering
dfe <- data.world::query(
  data.world::qry_sql("SELECT * FROM GlobalBurdenofDisease_Europe"),
  dataset = project
)

dfe_heart = dfe %>% dplyr::filter(region_name %in% c("Eastern Europe", "Western Europe") & cause_name %in% c("Ischemic heart disease", "Ischemic stroke") & age_name_unit == "years")

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
names(df1)
dfcluster2 %>% ggplot() + geom_point(mapping = aes(x=yld_rate, y=yll_rate, colour = as.factor(km.out.cluster))) + geom_point(data=df1, mapping=aes(yld_rate, yll_rate, size=km.out.size))

dfe_heart %>% ggplot(aes(x=yld_rate, y = yll_rate, colour = region_name)) + geom_point()

