#################################XG-Boost Algorithm#################################
all_vars <- c(factor.col, cont_vars_all)
#cstat_all <- as.data.frame(NULL)

seed = 61
set.seed(seed)

dt = subset(all_fin,all_fin$flag=="train")
split =sample.split(dt,SplitRatio = 0.7)
#split
train = subset(dt,split==TRUE) 
test = subset(dt,split==FALSE)
X.trn <- data.matrix(train[, c(all_vars,"syn","syn2","syn3","syn4","ppp3")])
Y.trn <- data.matrix(train[, "cancel"])

X.test <- data.matrix(test[, c(all_vars,"syn","syn2","syn3","syn4","ppp3")])
Y.test <- data.matrix(test[, "cancel"])

xgb <- xgboost(data = X.trn, 
               label = Y.trn , 
               max.depth = 4, 
               nround=45, 
               eta = 0.1, 
               nthread = 3, 
               objective = "binary:logistic")
# xgb <- xgb.cv(data = X.trn, 
#                label = Y.trn, 
#                max.depth = 15, 
#                nround=1,
#                 nfold = 5,
#                seed = 1,
#                eta = 0.11, 
#                nthread = 3, 
#                objective = "binary:logistic")

trn_pred <-  predict(xgb, X.trn)

pred_input <- prediction(trn_pred,Y.trn)
AUC <- performance(pred_input,"auc")
AUC #77.9% (With all vars). 

test_pred <-  predict(xgb, X.test)

pred_input <- prediction(test_pred,Y.test)
AUC <- performance(pred_input,"auc")
AUC #73.37% (With all vars). 


# model <- xgb.dump(xgb, with_stats = T)
# model[1:10] #This statement prints top 10 nodes of the model
# 
# # Get the feature real names
# names <- dimnames(data.matrix(X.trn))[[2]]
# 
# # Compute feature importance matrix
# importance_matrix <- xgb.importance(names, model = xgb)
# # Nice graph
# xgb.plot.importance(importance_matrix[1:10,])
# 
# test <- chisq.test(train$Age, output_vector)