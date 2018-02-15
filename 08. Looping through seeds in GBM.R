for (i in 1:10)
{
  set.seed(i)
  # work_fin$ppp3 <- newdata$ppp3
  # work_fin$ppp2 -<- newdata$ppp2
  # work_fin$ppp1 <- newdata$ppp1
    
  dt = subset(all_fin,all_fin$flag=="train")
  split =sample.split(dt,SplitRatio = 0.7)
  #split
  train = subset(dt,split==TRUE)
  test = subset(dt,split==FALSE)
  
  X.trn <- data.matrix(train[, c(all_vars,"syn","syn2","syn3","syn4","ppp3")])
  Y.trn <- data.matrix(train[, "cancel"])
  
  X.test <- data.matrix(test[, c(all_vars,"syn","syn2","syn3","syn4","ppp3")])
  Y.test <- data.matrix(test[, "cancel"])
  
  
  
  #gbm
  # ntrees= 65
  # model= gbm.fit(x=X.trn, y=Y.trn, 
  #                distribution = "bernoulli", 
  #                n.trees = ntrees, interaction.depth = 3, shrinkage = 0.1, 
  #                verbose = T)
  
  ntrees= 741
  model= gbm.fit(x=X.trn, y=Y.trn,
                 distribution = "bernoulli",
                 n.trees = ntrees, interaction.depth = 4, shrinkage = 0.00668,n.minobsinnode = 62,
             #    bag.fraction = .678,
                 verbose = T)
  
  
  trn_pred <-  predict(model, X.trn,n.trees = ntrees, type = "response")
  
  pred_input <- prediction(trn_pred,Y.trn)
  TAUC <- performance(pred_input,"auc")
  TAUC #73.41% (With all vars). 
  
  
  test_pred <-  predict(model, X.test,n.trees = ntrees, type = "response")
  
  pred_input <- prediction(test_pred,Y.test)
  TeAUC <- performance(pred_input,"auc")
  TeAUC #73.41% (With all vars).
  
  if(i==1)
  {
    rm(area3)
    area3<-as.data.frame(NULL) 
    area3<-as.data.frame(i) #73.41% (With all vars). 
    colnames(area3)[i]="seed"
    area3$TAUC = TAUC@y.values
    area3$Teauc = TeAUC@y.values
  }
  else
  {
    b=data.frame(i,TAUC@y.values,TeAUC@y.values)
    names(b)=c("seed","TAUC","Teauc")
    area3=rbind(area3,b)
  }
  # sum1=sum1+as.numeric(TAUC@y.values)
  # sum2=sum2+as.numeric(TeAUC@y.values)
  
  # b=data.frame(124,sum1/10,sum2/10)
  # names(b)=c("Seed","avg1","avg2")
  # 
  # area=rbind(area,b)
}

fwrite(as.data.frame(area3),"area3.csv")
