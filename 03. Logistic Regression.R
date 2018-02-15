##################################################################################
####################### Stepwise Logistic Regression ##############################
##################################################################################

mod_inputs <- all_vars_back2[1]

for (i in (2:length(all_vars_back2)))
{
  mod_inputs <- paste(mod_inputs, "+", all_vars_back2[i])
}

mod_exp <- paste("cancel ~",mod_inputs)
fit <- glm(as.formula(mod_exp), family=binomial(link='logit'),data=train)

step_result <- step(fit,
                    direction="both",
                    test="Chisq",
                    data=train)

##################################################################################
#################################### Sign Change #################################
##################################################################################
# Checked with initial individual model variables and removed any variable where
# the sign has been changed(sign of multicollinearity)

coeff <- as.data.frame(step_result$coefficients)
head(coeff)
colnames(coeff)[1] <- 'Fin_Coeff'
coeff$New_Var_Name <- row.names(coeff)
head(coeff)

model_indiv <- as.tibble(fread('Individual Model Run.csv'))

model_indiv$New_Var_Name <- c(paste0(fact_vars, "1"), cont_vars_all)

mg <- merge(coeff, model_indiv[, c("New_Var_Name", "Estimate", "Variable_Name")], by = 'New_Var_Name', all.x = TRUE)

fin_vars <- mg[mg$Fin_Coeff*mg$Estimate > 0 & mg$New_Var_Name != '(Intercept)', 'Variable_Name']
head(fin_vars)

# Remove Year 2014 and Year2015 and introduce Year Continuous
fin_vars <- fin_vars[!(fin_vars %in% c('year2014', 'year2015'))]
fin_vars <- c(fin_vars, 'year_cont')

mod_inputs <- fin_vars[1]

for (i in (2:length(fin_vars)))
{
  mod_inputs <- paste(mod_inputs, "+", fin_vars[i])
}

mod_exp <- paste("cancel ~",mod_inputs)
fit <- glm(as.formula(mod_exp), family=binomial(link='logit'),data=train)

train$pred <- predict(fit, train, type = "response")

ROCRPred <-  prediction(train$pred, train$cance)
ROCRPref <-  performance(ROCRPred, "tpr", "fpr")
# plot(ROCRPref, colorsize = TRUE, print.cutoffs.at = seq(0,1, by=.1))
plot(ROCRPref, colorsize = TRUE)

pred_input <- prediction(train$pred,train$cancel)

AUC <- performance(pred_input,"auc")
AUC #71.03%

test$pred <- predict(fit, test, type = "response")

ROCRPred <-  prediction(test$pred, test$cancel)
ROCRPref <-  performance(ROCRPred, "tpr", "fpr")
# plot(ROCRPref, colorsize = TRUE, print.cutoffs.at = seq(0,1, by=.1))
plot(ROCRPref, colorsize = TRUE)

pred_input <- prediction(test$pred,test$cancel)

AUC <- performance(pred_input,"auc")
AUC #70.38%
