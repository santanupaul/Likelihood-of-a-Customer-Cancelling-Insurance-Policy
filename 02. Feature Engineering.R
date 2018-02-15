##################################################################################
################################ Feature Engineering ##############################
##################################################################################

# Zip Code, Year, Buckets(discussed in datapreprocessing tab), Transformations

#----------
#Zip Code
#----------

# Create Zip Code Bucket: We have 222 distinct zip codes in the data. There are 
# many zip codes which have low counts. Let's group them together as 'Others'

zip_cnt <- table(data2$zip.code)

# Let's group thoose zip codes which have counts < 40. 40 is an arbitary number 
# below which we believe will be too less information (In Training it will be fewer)

zip_keep <- names(zip_cnt[zip_cnt >= 40])

# Create a new variable in all2 dataset which will have 'Others'
data2 <- data2 %>% mutate(zip_new = ifelse(zip.code %in% zip_keep, zip_keep, 'Others'))

class(data2$zip_new)
#Convert into factor
data2$zip_new <- as.factor(data2$zip_new)
class(data2$zip_new)


factor.col <- c(factor.col, 'zip_new')
factor.col <- factor.col[factor.col != 'zip.code']
factor.col

#--------
#Year
#--------

# Since year 2017 is missing from training dataset. We will create a trend variable
# as discussed earlier which will have continuous values like 0.2, 0.4,.....
# 2013 - 0.2, 2014 - 0.4, ..... , 2017 - 1.0

data2 <- data2 %>% mutate(year_cont = ifelse(year == 2013, 0.2, 
                                             ifelse(year == 2014, 0.4,
                                                    ifelse(year == 2015, 0.6,
                                                           ifelse(year == 2016, 0.8,
                                                                  ifelse(year == 2017, 1.0, 99))))))
table(data2$year_cont)
table(data2$year)

cont_vars <- c(cont_vars, 'year_cont')
cont_vars

#-----------------
#Premium Bucket
#-----------------

data2 <- data2 %>% mutate(prem_buck = 1*(premium > 1090))

table(data2$prem_buck)
table(data2[data2$flag == 'train', 'prem_buck']) 
table(data2[data2$flag == 'train' & data2$prem_buck == 1, 'cancel']) #47% cancellations
table(data2[data2$flag == 'train' & data2$prem_buck == 0, 'cancel']) #32% cancellations

factor.col <- c(factor.col, 'prem_buck')

#----------------------------
#Len at res and Tenure Ratio
#----------------------------

data2 %>% 
  ggplot(aes(x = len.at.res, y = tenure)) +
  geom_point() 

data2 <- data2 %>% mutate(ten_per_res = tenure / len.at.res)

data2 %>%
  ggplot(aes(ten_per_res)) +
  geom_histogram(fill = "red", bins = 30) +
  scale_x_discrete(limits = seq(0, 30, 5))
summary(data2$ten_per_res)

AAA_factor <- cut(x = data2[data2$flag == 'train','ten_per_res'], breaks = seq(min(data2$n.children)-1, max(data2$n.children), 0.5))
AAA_factor <- as.data.frame(AAA_factor)
AAA_factor <- bind_cols(AAA_factor, as.data.frame(data2[data2$flag == 'train','cancel']))
colnames(AAA_factor)[2] <- 'cancel'

AAA_factor %>% 
  group_by(AAA_factor) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = AAA_factor, y = pct_cancel)) + 
  geom_col() +
  labs(x = "ten_per_res Bucket", y = "% Cancel") 

# > 2.5 Ratio seems to jump up. Let's create a flag for this as we did with premium

data2 <- data2 %>% mutate(ten_per_res_flag = ifelse(ten_per_res > 2.5, 1, 0))

data2 %>% subset(flag == 'train') %>%
  group_by(ten_per_res_flag) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = ten_per_res_flag, y = pct_cancel)) + 
  geom_col() +
  labs(x = "ten_per_res_flag Bucket", y = "% Cancel") 

cont_vars <- c(cont_vars, 'ten_per_res')
factor.col <- c(factor.col, 'ten_per_res_flag')


##########################Transformations######################################

#------------------------------------------
#Transformations on Contiinuous Variables
#------------------------------------------
cont_vars_all <- cont_vars
for (i in 1:length(cont_vars))
{
  data2[, paste0("log_",cont_vars[i])] <- log(1 + data2[, cont_vars[i]])
  data2[, paste0("sqr_",cont_vars[i])] <- data2[, cont_vars[i]]**2
  data2[, paste0("sqrt_",cont_vars[i])] <- data2[, cont_vars[i]]**0.5
}
cont_vars_all <- c(cont_vars_all, paste0("log_",cont_vars))
cont_vars_all <- c(cont_vars_all, paste0("sqr_",cont_vars))
cont_vars_all <- c(cont_vars_all, paste0("sqrt_",cont_vars))
cont_vars_all


##################################################################################
################ Create dummy variables for design matrix ##############################
##################################################################################

# Remove year variable from the model since test data has only 2017. So, continuous
# variable of year will be used
factor.col <- factor.col[factor.col != 'year']

baseline <- c('ni.genderM', 'sales.channelOnline', 'coverage.typeB', 'dwelling.typeLandlord', 'dwelling.typeHouse', 'creditmedium', 'house.colorblue', 'year2017', 'year2016', 'zip_newOthers')

dumm_conv <- factor.col[!(factor.col %in% c('claim.ind', 'ni.marital.status', 'prem_buck', 'ten_per_res_flag'))]

for (i in (1:1))
{
  cont_inputs <- paste0('~ -1 +', dumm_conv[i])
  dummies <- as.data.frame(model.matrix(as.formula(cont_inputs), data = data2))
}
for (i in (2:length(dumm_conv)))
{
  cont_inputs <- paste0('~ -1 +', dumm_conv[i])
  dumm_curr <- as.data.frame(model.matrix(as.formula(cont_inputs), data = data2))
  dummies <- bind_cols(dummies, dumm_curr)
}

colnames(dummies)

# Drop the baseline
dummies <- dummies[, !(colnames(dummies) %in% baseline)]

colnames(dummies)

class(dummies$ni.genderF)
dummies[] <- lapply(dummies, factor)
class(dummies$ni.genderF)

all_fin <- bind_cols(data2, dummies)

factor.col <- c(colnames(dummies), c('claim.ind', 'ni.marital.status', 'prem_buck',
                                     'ten_per_res_flag'))

all_fin <- all_fin[, c('cancel', 'flag', cont_vars_all, factor.col)] #Final Data to use

all_fin$cancel <- as.factor(all_fin$cancel)

# Write to disk
fwrite(all_fin, 'Final_Data.csv')

# Split the data
set.seed(100)
train <- all_fin[all_fin$flag == 'train', ]
indexes = sample(1:nrow(train), size=0.3*nrow(train))

test = train[indexes,]
# dim(test) 
train = train[-indexes,]

table(train$cancel) #24.5% events

##################################################################################
#################### Choose the Best Transformation ##############################
##################################################################################


trans <- matrix(data = NA, nrow = 4, ncol = 2)
con_vars_trans <- matrix(data = NA, nrow = length(cont_vars), ncol = 2)
for (i in 1:length(cont_vars))
{
  i = 1
  exp <- paste0("cancel ~ ", cont_vars[i])
  fit <- glm(as.formula(exp), family=binomial(link='logit'),data=train) 
  
  trans[1, 1] = cont_vars[i]
  trans[1, 2] = summary(fit)$coefficients[2, 3] #Z Score
  
  exp <- paste0("cancel ~ log_", cont_vars[i])
  fit <- glm(as.formula(exp), family=binomial(link='logit'),data=train) 
  
  trans[2, 1] = paste0("log_", cont_vars[i])
  trans[2, 2] = summary(fit)$coefficients[2, 3] #Z Score
  
  exp <- paste0("cancel ~ sqr_", cont_vars[i])
  fit <- glm(as.formula(exp), family=binomial(link='logit'),data=train) 
  
  trans[3, 1] = paste0("sqr_", cont_vars[i])
  trans[3, 2] = summary(fit)$coefficients[2, 3] #Z Score
  
  exp <- paste0("cancel ~ sqrt_", cont_vars[i])
  fit <- glm(as.formula(exp), family=binomial(link='logit'),data=train) 
  
  trans[4, 1] = paste0("sqrt_", cont_vars[i])
  trans[4, 2] = summary(fit)$coefficients[2, 3] #Z Score
  
  con_vars_trans[i, 1] <- trans[head(order(abs(as.numeric(trans[, 2])), decreasing = TRUE), 1), 1]
  con_vars_trans[i, 2] <- trans[head(order(abs(as.numeric(trans[, 2])), decreasing = TRUE), 1), 2]
}

cont_vars_back <- con_vars_trans[,1]

all_vars <- c(factor.col, cont_vars_all)

#######Feature Engineering 2nd Part########################################

setwd('V:/Semester Fall/Travellers Modelling Competetion/Final Data')
newdata=as.tibble(fread("Final Data 2nd Feature Creation.csv"))

#More Feature Creation based on our Vizualization
library(splines)

cont_vars_back_new <- cont_vars

# Create Tenure 10 and Tenure 11 as buckets: 

all_fin <- all_fin %>% mutate(ten10 = 1*(tenure == 10),
                              ten11 = 1*(tenure == 11))

factor.col <- c(factor.col, 'ten10', 'ten11')

bs.x <- bs(all_fin$tenure, knots = c(7, 10, 19, 23), degree = 3)

attributes(bs.x) <- attributes(bs.x)["dim"]
bs.x <- as.data.frame(bs.x)

colnames(bs.x)[1] = 'ten1'
colnames(bs.x)[2] = 'ten2'
colnames(bs.x)[3] = 'ten3'
colnames(bs.x)[4] = 'ten4'
colnames(bs.x)[5] = 'ten5'
colnames(bs.x)[6] = 'ten6'
colnames(bs.x)[7] = 'ten7'

# Merge with all_fin
all_fin <- bind_cols(all_fin, bs.x)

# cont_vars_back_new <- c(cont_vars_back_new[cont_vars_back_new != 'log_tenure'], 'ten1', 'ten2',
#                       'ten3', 'ten4', 'ten5', 'ten6', 'ten7')
cont_vars_back_new <- c(cont_vars_back_new, 'ten1', 'ten2',
                        'ten3', 'ten4', 'ten5', 'ten6', 'ten7')

# Child_per_Adult
all_fin <- all_fin %>% mutate(child_per_adult = n.children / n.adults,
                              child_per_adult_6 = 1*(child_per_adult == 6))

factor.col <- c(factor.col, 'child_per_adult_6')

bs.x <- bs(all_fin$child_per_adult, knots = c(6), degree = 2)

attributes(bs.x) <- attributes(bs.x)["dim"]
bs.x <- as.data.frame(bs.x)

colnames(bs.x)[1] = 'cpa1'
colnames(bs.x)[2] = 'cpa2'
colnames(bs.x)[3] = 'cpa3'

# Merge with all_fin
all_fin <- bind_cols(all_fin, bs.x)

cont_vars_back_new <- c(cont_vars_back_new, c('cpa1', 'cpa2', 'cpa3'))


# Premium Per Person
all_fin <- all_fin %>% mutate(prem_per_pers = premium / (n.children + n.adults))

bs.x <- bs(all_fin$prem_per_pers, knots = c(350, 550, 700), degree = 3)

attributes(bs.x) <- attributes(bs.x)["dim"]
bs.x <- as.data.frame(bs.x)

bs.x <- as.data.frame(bs.x)
colnames(bs.x)

colnames(bs.x)[1] = 'ppp1'
colnames(bs.x)[2] = 'ppp2'
colnames(bs.x)[3] = 'ppp3'
colnames(bs.x)[4] = 'ppp4'
colnames(bs.x)[5] = 'ppp5'
colnames(bs.x)[6] = 'ppp6'

# Merge with all_fin
all_fin <- bind_cols(all_fin, bs.x)

cont_vars_back_new <- c(cont_vars_back_new, c('ppp1', 'ppp2', 'ppp3',
                                              'ppp4', 'ppp5', 'ppp6'))

# PRemium

bs.x <- bs(all_fin$premium, knots = c(720), degree = 1)

attributes(bs.x) <- attributes(bs.x)["dim"]
bs.x <- as.data.frame(bs.x)

colnames(bs.x)

colnames(bs.x)[1] = 'prem1'
colnames(bs.x)[2] = 'prem2'

# Merge with all_fin
all_fin <- bind_cols(all_fin, bs.x)

# cont_vars_back_new <- c(cont_vars_back_new[cont_vars_back_new != 'log_premium'],
#                         'prem1', 'prem2')
cont_vars_back_new <- c(cont_vars_back_new,
                        'prem1', 'prem2')

#all_vars <- c(factor.col, cont_vars_back_new)

#Creating the variable combination based interaction effect on the model.We tried different combination of variables to check the synergy effect of these combination on our model 
all_fin$syn <- as.numeric(unlist(all_fin$sales.channelPhone))*as.numeric(unlist(all_fin$creditlow
                                                                                ))

all_fin$syn2 <- as.numeric(unlist(all_fin$sales.channelBroker))*as.numeric(unlist(all_fin$zip_new85065
))
#
all_fin$syn3 <- as.numeric(unlist(all_fin$credithigh))*as.numeric(unlist(all_fin$zip_new80012
))

all_fin$syn4 <- as.numeric(unlist(all_fin$credithigh))*as.numeric(unlist(all_fin$log_ni.age
))
#all_fin$ppp3 <- newdata$ppp3

# all_fin$syn5 <- as.numeric(unlist(all_fin$credithigh))*as.numeric(unlist(all_fin$credithigh
# ))