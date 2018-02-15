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

#load GBM
getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "prob")

#specify tuning method
rancontrol <- makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

# paramteres
gbm_par<- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.001, upper = .5)
)

trainTask <- makeClassifTask(data = train,target = "cancel", positive = "1")

#tune parameters
tune_gbm <- tuneParams(learner = g.gbm, task = trainTask,resampling = set_cv,measures = auc,par.set = gbm_par,control = rancontrol)

#check CV accuracy
tune_gbm$y

# set parameters
final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

to.gbm <- train(final_gbm, trainTask)

testTask <- makeClassifTask(data = test,target = "cancel", positive = "1")

# test 
pr.gbm <- predict(to.gbm, testTask)

pred_input <- prediction(pr.gbm$data$prob.1,test$cancel)
AUC_test <- ROCR::performance(pred_input,"auc")
AUC_test 


# train 
trn.gbm <- predict(to.gbm, trainTask)

pred_input <- prediction(trn.gbm$data$prob.1,train$cancel)
AUC_trn <- ROCR::performance(pred_input,"auc")
AUC_trn 
