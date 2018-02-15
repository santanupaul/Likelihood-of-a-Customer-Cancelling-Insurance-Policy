library(xgboost)
library(readr)
library(stringr)
library(mlr)
library(mlrMBO)


all_fin_now <- all_fin %>% subset(flag == 'dset') %>% mutate(rnum = row_number())
all_fin_now <- all_fin_now[, c(all_vars, 'cancel', 'rnum')]

seed = 101
set.seed(seed)

train <- sample_n(all_fin_now, 0.7*nrow(all_fin_now))
test <- anti_join(all_fin_now,  train, by = "rnum")

train$rnum <- NULL
test$rnum <- NULL

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
xg_set$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 250
)

#define parameters for tuning
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=100,upper=500),
  makeIntegerParam("max_depth",lower=3,upper=15),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.3),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 100L) #do 100 iterations

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

# Train Task
# xgboost mlr doesn't support factors. so need to convert to integer'
for (i in (1:length(fact_vars)))
{
  train[, fact_vars[i]] <- as.integer(train[, fact_vars[i]])
}

trainTask <- makeClassifTask(data = train,target = "cancel", positive = "1")

#tune parameters
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = auc,par.set = xg_ps, control = rancontrol)

# Results not coming good with this. There's overfitting happening'

xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#selecting top 6 important features
top_task <- filterFeatures(trainTask, method = "randomForestSRC.rfsrc", abs = 10)

#tune parameters
xg_tune_new <- tuneParams(learner = xg_set, task = top_task, resampling = set_cv,measures = auc,par.set = xg_ps, control = rancontrol)

xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune_new$x)

xgmodel <- train(xg_new, top_task)

testTask <- makeClassifTask(data = test,target = "cancel", positive = "1")

predict.xg <- predict(xgmodel, testTask)

pred_input <- ROCR::prediction(predict.xg$data$prob.1,test$cancel)
AUC_test <- ROCR::performance(pred_input,"auc")
AUC_test 

