#####################################GBM###########################################
#Split
set.seed(61)
dt = subset(all_fin,all_fin$flag=="train")
split =sample.split(dt,SplitRatio = 0.7)
train = subset(dt,split==TRUE)
test = subset(dt,split==FALSE)

#Define X and Y from training Data
X.trn <- data.matrix(train[, c(all_vars,"syn","syn2","syn3","syn4","ppp3")])
Y.trn <- data.matrix(train[, "cancel"])

#Define X and Y for test data
X.test <- data.matrix(test[, c(all_vars,"syn","syn2","syn3","syn4","ppp3")])
Y.test <- data.matrix(test[, "cancel"])

######GBM##############
ntrees= 741
model= gbm.fit(x=X.trn, y=Y.trn,
               distribution = "bernoulli",
               n.trees = ntrees, interaction.depth = 4, shrinkage = 0.00668,n.minobsinnode = 62,
               #               bag.fraction = .678,
               
               verbose = T)

# train2 <- train[, c('cancel', all_vars,"syn","syn2","syn3","syn4","ppp3")]
# ntrees= 741
# model= gbm(cancel ~ ., data = train2,
#                distribution = "bernoulli",
#                n.trees = ntrees, interaction.depth = 4, shrinkage = 0.00668,n.minobsinnode = 62,
# #               bag.fraction = .678,
#                 cv.folds = 5,
#                verbose = T)

#Training
trn_pred <-  predict(model, X.trn,n.trees = ntrees,type="response")

pred_input <- prediction(trn_pred,Y.trn)
TAUC <- performance(pred_input,"auc")
TAUC #74.05% (With all vars). 

#Test
test_pred <-  predict(model, X.test,n.trees = ntrees,type="response")

pred_input <- prediction(test_pred,Y.test)
TeAUC <- performance(pred_input,"auc")
TeAUC #73.90% (With all vars).