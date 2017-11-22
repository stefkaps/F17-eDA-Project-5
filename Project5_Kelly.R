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
dfb=dplyr::sample_n(dfb, 10000)
boost.df=gbm(death_abs~.,data=dfb,distribution="gaussian",n.trees=1000,shrinkage=0.01,interaction.depth=4)
summary(boost.df)
