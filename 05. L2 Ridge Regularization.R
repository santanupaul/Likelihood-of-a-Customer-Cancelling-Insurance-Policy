library('glmnet')
# 
all_fin_now <- all_fin %>% subset(flag == 'dset') %>% mutate(rnum = row_number())

cstat_all <- as.data.frame(NULL)

seed = 10
set.seed(seed)
train_val <- sample_n(all_fin_now, 0.8*nrow(all_fin_now))
test <- anti_join(all_fin_now,  train_val, by = "rnum")
train <- sample_n(train_val, 0.8*nrow(train_val))
val <- anti_join(train_val,  train, by = "rnum")

X.trn <- data.matrix(train[, all_vars])
Y.trn <- data.matrix(train[, "cancel"])

X.val <- data.matrix(val[, all_vars])
Y.val <- data.matrix(val[, "cancel"])

X.test <- data.matrix(test[, all_vars])
Y.test <- data.matrix(test[, "cancel"])

make.plots     <- TRUE     # plot graphs?

fsd <- function(x){
  # Return various file system, operating system specifics.
  if (x=="datasetFp"){
    return(paste0(getwd(), "/datasets/"))
  } else if(x=="figwin"){
    if(.Platform$OS.type=="unix"){
      if(capabilities('X11') == TRUE) {
        x11()
      }
    } else if (.Platform$OS.type=="windows"){
      windows()	
    }
  } 
  else {
    stop('Argument x not recognised.')
  }
}

#---------------------------------------------- APPLY LOG REG W. REGULARISATION


# select elastic net parameter alpha.
# alpha controls the mixture of l1 and l2 regularisation term
# alpha=0 => pure l2 regularisation a.k.a ridge 
# alpha=1 => pure l1 regularisation a.k.a lasso
alpha <- 0

# do logistic regression over lambda regularisation path.
lr.reg.path <- glmnet(
  X.trn, 
  Y.trn, 
  family="binomial", 
  alpha=alpha,
  lambda.min.ratio=1e-7,
  intercept=TRUE)

# plot the regularisation path
if (make.plots){
  fsd("figwin")
  plot(lr.reg.path, label=TRUE)
  title(paste("Training regularisation path, alpha=", alpha))
}

# Calculate the models predictions for each value of lambda
Y.val.regpath.pred <- predict(lr.reg.path, X.val, type="class")
acc.val.regpath <- apply(Y.val.regpath.pred, 2, function(x){mean(x==Y.val)})

# plot the validation set error over the regularisation path
if (make.plots){
  fsd("figwin")
  plot(log(lr.reg.path$lambda),
       acc.val.regpath,
       xlab="log lambda",
       ylab="Classification accuracy",
       main="Buyer Non-Buyer Validation classification accuracy")
}


if (pause){ browser()}
#------------------------------------------------------------- PICK A BEST MODEL

# Pick a value of lambda
lambda.try <- exp(-2.8)
# lr.reg.path$lambda[which.max(acc.val.regpath)]

# Plot the weight vector of the regularised model for a specific value of lambda
if (make.plots){
  w.to.plot <- as.vector(as.matrix(coef(lr.reg.path, s=lambda.try)))
  names(w.to.plot) <- row.names(as.matrix(coef(lr.reg.path, s=lambda.try)))
  
  fsd("figwin")
  par(oma=c(2,0.5,0.5,0.5))
  barplot(w.to.plot, las=3, col="red",
          main=paste0("Regularised weights w. lambda=", signif(lambda.try,2)))
}

cat("\n\n", "Regularised logistic regression with :")
cat("\n", "alpha = ", signif(alpha, 2), " and lambda = ", signif(lambda.try))

lr.reg.try.trn.acc <- mean(
  predict(lr.reg.path, X.trn, s=lambda.try, type="class")==Y.trn)
cat("\n", "\t",
    "Train set prediction accuracy       = ",
    (lr.reg.try.trn.acc*100), "%")

# Calculate and print validation set predictions
lr.reg.try.val.acc <- mean(
  predict(lr.reg.path, X.val, s=lambda.try, type="class")==Y.val)
cat("\n", "\t",
    "Validation set prediction accuracy  = ",
    (lr.reg.try.val.acc*100), "%", "\n")


#------------------------------------------------- MAKE TEST SET PREDICTIONS

if (FALSE){
  # pick a best value of lambda
  lambda.star <- exp(-4)
  
  Y.test.pred.lamp <- predict(
    lr.reg.path, newx=X.test, s=lambda.star, type="class")
  # print results.
  cat("\n", "For lambda=", lambda.star, 
      "Test set prediction acc. = ", (100*mean(Y.test.pred.lamp!=Y.test)), "%")
}

reg_coeff <- as.data.frame(w.to.plot)
reg_coeff$Variable <- names(w.to.plot)

head(reg_coeff[with(reg_coeff, order(-w.to.plot)), ], 10)

# fwrite(reg_coeff, "Lasso Reg Fin Vars.csv")
# fwrite(dset2, "Fin Data.csv")
# fwrite(as.data.frame(xvars_red2), "Fin Vars After Removing Multicollinearity.csv")

########### Train

# Confusion Matrix
train$pred <- predict(lr.reg.path, X.trn, s=lambda.try, type="response")

conf_test <- table(ActualValue = train$cancel, PredictedValue = train$pred > 0.5)
(conf_test[1,1]+conf_test[2,2])/sum(conf_test)

#ROC Curve
ROCRPred <-  prediction(train$pred, train$cancel)
ROCRPref <-  performance(ROCRPred, "tpr", "fpr")
# plot(ROCRPref, colorsize = TRUE, print.cutoffs.at = seq(0,1, by=.1))
plot(ROCRPref, colorsize = TRUE)

ROC_train <- as.data.frame(NULL)
ROC_train <- as.data.frame(ROCRPref@x.values[[1]])
ROC_train[, "tpr"] <- as.data.frame(ROCRPref@y.values[[1]])
colnames(ROC_train)[1] <- "fpr"

# fwrite(ROC_train, "Buyer_Lasso Training ROC.csv")

pred_input <- prediction(train$pred,train$cancel)

AUC <- performance(pred_input,"auc")
AUC #73.37% (With all vars). 

cstat <- as.data.frame(AUC@y.values)
colnames(cstat)[1] <- 'AUC'
cstat$seed <- seed
cstat$dataset <- 'Training'
cstat_all <- bind_rows(cstat_all, cstat)

# Gain Chart

# perf.obj <- prediction(predictions=train$pred,
#                        labels=train$cancel)
# lift.obj <- performance(perf.obj, measure="lift", x.measure="rpp")
# 
# plot(lift.obj,
#      main="Lift Chart",
#      xlab="% Populations",
#      ylab="Lift",
#      col="blue")
# abline(1,0,col="grey")
# 
# Lift_train <- as.data.frame(NULL)
# Lift_train <- as.data.frame(lift.obj@x.values[[1]])
# Lift_train[, "lift"] <- as.data.frame(lift.obj@y.values[[1]])
# colnames(Lift_train)[1] <- "% Populations"
# 
# fwrite(Lift_train, "Buyer_Lasso Training Lift.csv")


# Cumulatove Lift Chart
# lift <- train[, c("cancel", "pred")]
# fwrite(lift, "Buyer_Lasso Training Cumulative Lift.csv")
# 
# auc(train$cancel, train$pred)

############  Validation

# Confusion Matrix
val$pred <- predict(lr.reg.path, X.val, s=lambda.try, type="response")

conf <- table(ActualValue = val$cancel, PredictedValue = val$pred > 0.5)
(conf[1,1]+conf[2,2])/sum(conf)

#ROC Curve
ROCRPred <-  prediction(val$pred, val$cancel)
ROCRPref <-  performance(ROCRPred, "tpr", "fpr")
# plot(ROCRPref, colorsize = TRUE, print.cutoffs.at = seq(0,1, by=.1))
plot(ROCRPref, colorsize = TRUE)

ROC_val <- as.data.frame(NULL)
ROC_val <- as.data.frame(ROCRPref@x.values[[1]])
ROC_val[, "tpr"] <- as.data.frame(ROCRPref@y.values[[1]])
colnames(ROC_val)[1] <- "fpr"

# fwrite(ROC_val, "Buyer_Lasso valing ROC.csv")

pred_input <- prediction(val$pred,val$cancel)

AUC <- performance(pred_input,"auc")
AUC #67.96% (with all vars). 

cstat <- as.data.frame(AUC@y.values)
colnames(cstat)[1] <- 'AUC'
cstat$seed <- seed
cstat$dataset <- 'Validation'
cstat_all <- bind_rows(cstat_all, cstat)

# Gain Chart

# perf.obj <- prediction(predictions=val$pred,
#                        labels=val$cancel)
# lift.obj <- performance(perf.obj, measure="lift", x.measure="rpp")
# 
# plot(lift.obj,
#      main="Lift Chart",
#      xlab="% Populations",
#      ylab="Lift",
#      col="blue")
# abline(1,0,col="grey")
# 
# Lift_val <- as.data.frame(NULL)
# Lift_val <- as.data.frame(lift.obj@x.values[[1]])
# Lift_val[, "lift"] <- as.data.frame(lift.obj@y.values[[1]])
# colnames(Lift_val)[1] <- "% Populations"
# 
# fwrite(Lift_val, "Buyer_Lasso valing Lift.csv")


# Cumulatove Lift Chart
# lift <- val[, c("cancel", "pred")]
# fwrite(lift, "Buyer_Lasso valing Cumulative Lift.csv")
# 
# auc(val$cancel, val$pred)


# Confusion Matrix
test$pred <- predict(lr.reg.path, X.test, s=lambda.try, type="response")

conf <- table(ActualValue = test$cancel, PredictedValue = test$pred > 0.5)
(conf[1,1]+conf[2,2])/sum(conf)

#ROC Curve
ROCRPred <-  prediction(test$pred, test$cancel)
ROCRPref <-  performance(ROCRPred, "tpr", "fpr")
# plot(ROCRPref, colorsize = TRUE, print.cutoffs.at = seq(0,1, by=.1))
plot(ROCRPref, colorsize = TRUE)

ROC_test <- as.data.frame(NULL)
ROC_test <- as.data.frame(ROCRPref@x.values[[1]])
ROC_test[, "tpr"] <- as.data.frame(ROCRPref@y.values[[1]])
colnames(ROC_test)[1] <- "fpr"

# fwrite(ROC_test_data, "Buyer_Lasso Test ROC.csv")

pred_input <- prediction(test$pred,test$cancel)

AUC <- performance(pred_input,"auc")
AUC #72.1% (with all vars).

cstat <- as.data.frame(AUC@y.values)
colnames(cstat)[1] <- 'AUC'
cstat$seed <- seed
cstat$dataset <- 'Test'
cstat_all <- bind_rows(cstat_all, cstat)

# Gain Chart

# perf.obj <- prediction(predictions=test_data$pred,
#                        labels=test_data$cancel)
# lift.obj <- performance(perf.obj, measure="lift", x.measure="rpp")
# 
# plot(lift.obj,
#      main="Lift Chart",
#      xlab="% Populations",
#      ylab="Lift",
#      col="blue")
# abline(1,0,col="grey")
# 
# Lift_test_data <- as.data.frame(NULL)
# Lift_test_data <- as.data.frame(lift.obj@x.values[[1]])
# Lift_test_data[, "lift"] <- as.data.frame(lift.obj@y.values[[1]])
# colnames(Lift_test_data)[1] <- "% Populations"
# 
# fwrite(Lift_test_data, "Buyer_Lasso Test Lift.csv")
# 
# 
# # Cumulatove Lift Chart
# lift <- test_data[, c("cancel", "pred")]
# fwrite(lift, "Buyer_Lasso Test Cumulative Lift.csv")
# 
# auc(test_data$cancel, test_data$pred)

cstat_all

fwrite(cstat_all, 'L2 Regularization AUCs.csv')