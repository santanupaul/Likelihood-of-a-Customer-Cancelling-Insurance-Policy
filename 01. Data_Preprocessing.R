#---------------------------------------------------------------------
#Load Libraries
#---------------------------------------------------------------------
#Load required libraries and suppress the generated startup messages.
library('tibble')
#Tibbles are a modern take on data frames. They keep the features that have stood 
# the test of time, and drop the features that used to be convenient but are now 
# frustrating (i.e. converting character vectors to factors).

library('data.table')
#data.table inherits from data.frame. It offers fast and nemory efficient:
#fread is a part of it

library('dplyr')
#bind_rows() function - better than rbind(deprecated) since can append two rows even
# if two rows don't have exact fields

library('Hmisc') 
#Missing Value Imputation Using Preditive Mean Matching


#--------------
#Load the data
#--------------
setwd('V:/Semester Fall/Travellers Modelling Competetion')
rm(list = ls())
train= as.tibble(fread('Train.csv'))
test=as.tibble(fread('Test.csv'))


#------------------------
#Append the two datasets
#------------------------
# Create a flag to denote which dataset represents what. Will be helpful after appending
train <- train %>% mutate(flag = "train")
test <- test %>% mutate(flag = "test")
data <- bind_rows(train,test)
# Remove train and test
rm(train)
rm(test)

table(data[data$flag == 'dset','cancel'], useNA = 'ifany')
# Contains -1. Need to be removed(later)
# 24.4% Events(1s)


#------------------------------------------------------------------
#Missing Values
#------------------------------------------------------------------
# Total Number of Missig Values
sum(is.na(data[data$flag == 'train',!(colnames(data) == 'cancel')]))
sum(is.na(data[data$flag == 'test',!(colnames(data) == 'cancel')]))

sum(is.na(data[,!(colnames(data) == 'cancel')])) #136 (105 dset + 31 score)

all_vars <- colnames(data)[!(colnames(data) %in% c('cancel', 'id', 'flag'))]

# Check which columns have missing values in the appended dataset
#miss <- matrix(data = NA, nrow = length(all_vars), ncol = 2)
#for (i in 1:length(all_vars))
#{
#  miss[i, 1] <- all_vars[i]
#  miss[i, 2] <- sum(is.na(data[, all_vars[i]]))
#}
#miss[order(as.numeric(miss[, 2]), decreasing = TRUE), ]
t(apply(is.na(data), 2, sum))

# Check which columns have missing values in the test dataset(score)
miss_score <- matrix(data = NA, nrow = length(all_vars), ncol = 2)
for (i in 1:length(all_vars))
{
  miss_score[i, 1] <- all_vars[i]
  miss_score[i, 2] <- sum(is.na(data[data$flag == 'test', all_vars[i]]))
}

miss_score[order(as.numeric(miss_score[, 2]), decreasing = TRUE), ]


# year, house.color and n.children don't have missing values. 
#So we can remove records in the train data(dset) for which these 3 columns have missing values. 
#So, for these columns we don't have to impute anything

data <- data[!is.na(data$house.color), ]
data <- data[!is.na(data$n.children), ]
data <- data[!is.na(data$year), ]

#----------------------------------------------------
#Impute Missing Value
#----------------------------------------------------
# Now we can impute the other missing values 
# Check which columns have missing values in the appended dataset
miss <- matrix(data = NA, nrow = length(all_vars), ncol = 2)
for (i in 1:length(all_vars))
{
  miss[i, 1] <- all_vars[i]
  miss[i, 2] <- sum(is.na(data[, all_vars[i]]))
}

miss <- miss[order(as.numeric(miss[, 2]), decreasing = TRUE), ]

#--------------------------------------------------------
#Convert Data Type
#--------------------------------------------------------
str(data)

#Need to convert data type to factor claim.ind,ni.marital.status,year,cancel,zip-code
# Convert Data Types of Categorical Variable
factor.col =c('ni.gender', 'sales.channel', 'coverage.type', 'dwelling.type',
              'credit', 'house.color', 'claim.ind', 'ni.marital.status', 'year','zip.code')
data[factor.col] = lapply(data[factor.col], factor)
str(data)


#-----------------------------------------------------------
#Impute Other Missing Value usinh Hmisc package
#-----------------------------------------------------------
# Let's use Hmisc package for missing value imputation using Predictive Mean Matching
miss_imp <- all_vars[all_vars != 'zip.code'] #Zip Code cannot be predicted using this method
# since there are too many levels

exp <- paste0("~ ", miss_imp[1])
for (i in 2:length(miss_imp))
{
  exp <- paste0(exp,"+",miss_imp[i])
}
impute_arg <- aregImpute(as.formula(exp), data = data, n.impute = 5) #argeImpute
impute_arg

# Please note 'all' dataset is used to impute missing values as test data(score)
# contains information on x variables

imputed <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=data, list.out=TRUE, pr=FALSE, check=FALSE))

data2 <- bind_cols(imputed, data[, c("cancel", "id", "flag", "zip.code")])

sum(is.na(data2[data$flag != 'test', ])) #4 which is for zipcodes
sum(is.na(data2$zip.code)) #7: There are 3 missing values in zipcodes for test dataset(score)

# Let's replace the missign values in zip codes with the modal value of the variable'
a <- table(data2$zip.code)
zip_imp <- names(a[order(a, decreasing = TRUE)][1])

data2$zip.code[is.na(data2$zip.code)] <- zip_imp
rm(a)
rm(zip_imp)

sum(is.na(data2[data2$flag != 'test', ])) #No missing values

# Replace the missing values in flag = score to 0(arbitary)
data2$cancel[data2$flag == 'test'] <- 0

# Now Remove records where cancel = -1 (First Missing Value Imputation. 
# Then this removel)
data2 <- data2[data2$cancel != -1, ]



##################################################################################
################################ Study Each Feature ##############################
##################################################################################

str(data2)

########################## Continuous Variables ##############################

# The continuous variables are of type impute. Let's convert them into numeric'
cont_vars <- c('tenure', 'n.adults', 'n.children', 'premium', 'len.at.res', 'ni.age')

for (i in 1:length(cont_vars))
{
  data2[, cont_vars[i]] <- as.numeric(unlist(data2[, cont_vars[i]]))
}
str(data2)

#--------
# Tenure
#--------
data2 %>% 
  ggplot(aes(tenure)) +
  geom_histogram(fill = "red", bins = 20) +
  scale_x_discrete(limits = 0:31)
# More or less bell shaped, with Mode around 15 
summary(data2$tenure)

AAA_factor <- cut(x = data2[data2$flag == 'train','tenure'], breaks = seq(min(data2$tenure)-1, max(data2$tenure)+5, 5))
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
  labs(x = "Tenure Bucket", y = "% Cancel") 

# Slight indication of a square relationship: log odd = x^2
# If tenure is very low or very high then high chance of cancellation. 
# When tenure is low it means customers are not loyal. 

#----------------------
# Check: Tenure vs Age
#----------------------
data2 %>%
  subset(tenure > 24) %>%
  ggplot(aes(ni.age)) +
  geom_histogram(fill = "red", bins = 20) 
summary(data2$ni.age[data2$tenure > 24])
# Median age is 52 for higher tenure

data2 %>%
  mutate(age_ten_diff = (ni.age - tenure)) %>%
  ggplot(aes(age_ten_diff)) +
  geom_histogram(fill = "red", bins = 20) 

chk <- data2 %>%
  mutate(age_ten_diff = (ni.age - tenure))
summary(chk$age_ten_diff)

# The graph is left skewed because of outliers in Age
# Also, there are very low values indicating the policyholders to be with Kangaroo
# from a very early age, which is absurd. These are also outliers and need to be checked
# Mimum age of 16 would be taken as a cutoff point when one can have a home insurance
# So Age - Tenure difference = 16 will be treated as a cutoff point

chk[chk$age_ten_diff < 17, c("ni.age", "tenure", "age_ten_diff", "flag")]

# Clearly 31 tenure is an outlier. Assuming values for the age is correct, tenure cannot
# be 31

# Unfortunately these outliers are present in Test Dataset(score). We can either
# cap them to make age - tenure difference as 16 or treat them as missing values
# We will treat them as missing values and try to impute using Preditcive Mean Matching

chk[chk$tenure == 31, c("ni.age", "tenure", "age_ten_diff", "flag")]
data2[data2$tenure == 31, "tenure"] <- NA #Need to revisit to treat them
chk[chk$tenure == 30, c("ni.age", "tenure", "age_ten_diff", "flag")] #Seems fine

#----------
# n.adults
#----------
data2 %>%
  ggplot(aes(n.adults)) +
  geom_histogram(fill = "red", bins = 20) +
  scale_x_discrete(limits = 0:20)
# Deceresing Trend
summary(data2$n.adults)

AAA_factor <- cut(x = data2[data2$flag == 'train','n.adults'], breaks = seq(min(data2$n.adults)-1, max(data2$n.adults)+5, 1))
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
  labs(x = "n.adults Bucket", y = "% Cancel") 

# As number of adults increase % cancellations increase. 
#Maybe becasuse of less stability
# in the household due to more number of people

#------------
# n.children
#------------
data2 %>%
  ggplot(aes(n.children)) +
  geom_histogram(fill = "red", bins = 20) +
  scale_x_discrete(limits = 0:20)
# Deceresing Trend
summary(data2$n.children)

AAA_factor <- cut(x = data2[data2$flag == 'train','n.children'], breaks = seq(min(data2$n.children)-1, max(data2$n.children)+5, 1))
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
  labs(x = "n.children Bucket", y = "% Cancel") 

# As number of children increase % cancellations increase. Maybe this has direct 
# correlation with number of adults. Let's check

data2 %>%
  ggplot(aes(x = n.children, y = n.adults)) +
  geom_point() 

# There's no correlation between children and adults
# Lets see with premium
data2 %>%
  ggplot(aes(x = n.children, y = premium)) +
  geom_point() 

#Nothing striking. 

#----------
# premium
#----------
data2 %>%
  ggplot(aes(premium)) +
  geom_histogram(fill = "red", bins = 50) +
  scale_x_discrete(limits = 0:20)
# Normal distribution: Bell Curve
summary(data2$premium)

AAA_factor <- cut(x = data2[data2$flag == 'train','premium'], breaks = seq(min(data2$premium)-1, max(data2$premium)+50, 50))
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
  labs(x = "premium Bucket", y = "% Cancel") 

# The first bucket(645 to 695) has higher chance of cancelling. However, there are
# few data points in that bucket (Run above until pct_cancel = n_cancel / n_total))
# Alos, there's a jump when the premium is high (1090 to 1140). Cancelling when the 
# premium is high makes sense. Let's create a bucket when premium is >= 1090

data2 %>% subset(ni.age < 90) %>% 
  ggplot(aes(x = ni.age, y = premium)) +
  geom_point()

#------------
# len.at.res
#------------
data2 %>%
  ggplot(aes(len.at.res)) +
  geom_histogram(fill = "red", bins = 30) +
  scale_x_discrete(limits = seq(0, 50, 5))
# Normal distribution: Bell Curve. There seems to be some extreme values beyond 25
summary(data2$len.at.res)

AAA_factor <- cut(x = data2[data2$flag == 'train','len.at.res'], breaks = seq(min(data2$len.at.res)-1, max(data2$len.at.res)+5, 5))
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
  labs(x = "len.at.res Bucket", y = "% Cancel") 
# As len at res increases chances of cancellation decreases. If the len at res
# is more it means they are more stable and less likely to cancel

data2 %>% 
  ggplot(aes(x = n.children, y = len.at.res)) +
  geom_point() 
# As number of children increases, length of res decreases, increasing the chance 
# of cancellation
# Here we find a small negative correlation b/w children and len at res. So, as num
# of children increases, len at res decreases and higher chance of cancellation

data2 %>% 
  ggplot(aes(x = n.adults, y = len.at.res)) +
  geom_point() 
# As number of adults increases, length of res decreases, increasing the chance 
# of cancellation

data2 %>% subset(ni.age <= 90) %>%
  ggplot(aes(x = ni.age, y = len.at.res)) +
  geom_point() 

#-------------------------------------------------------------------------------
# Length at res(how long the policy holder is living in the property) is greater 
# than age in some cases. How is this possible? We can cap these values to Age
# (Assuming Age is fine for these policyholders)
#-------------------------------------------------------------------------------

nrow(data2[data2$len.at.res > data2$ni.age, ]) #47 such policies 

# Cap them
data2[data2$len.at.res > data2$ni.age, 'len.at.res'] <- data2[data2$len.at.res > data2$ni.age, 'ni.age']

data2 %>% subset(ni.age <= 90) %>%
  ggplot(aes(x = ni.age, y = len.at.res)) +
  geom_point() 

#--------
# ni.age
#--------
data2 %>%
  ggplot(aes(ni.age)) +
  geom_histogram(fill = "red", bins = 30) +
  scale_x_discrete(limits = seq(0, 300, 50))
# Normal distribution: Bell Curve. 
# Check Outlier: There are clearly outliers present in Age. Need to treat them.
summary(data2$ni.age)
length(data2[data2$ni.age > 90, "ni.age"]) #19 people with Age > 90. Let's treat them
# as outliers. We will replace these with missing values and then treat them 
# using Pred Mean Matching method
#data2[data2$ni.age > 90, "ni.age"] <- NA #Need to revisit to treat them

sum(is.na(data2)) #25 missing (19 Age and 6 Tenure). Will apply Pred Mean Match

AAA_factor <- cut(x = data2[data2$flag == 'train','ni.age'], breaks = seq(min(data2$ni.age)-1, 90, 5))
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
  labs(x = "ni.age Bucket", y = "% Cancel") 

# As Age increases, there is lower chance of cancellation. With age the behaviour 
# gets more stable

data2 %>% subset(ni.age < 90) %>%
  ggplot(aes(x = ni.age, y = tenure)) +
  geom_point() 

# There are two clusters existing in the data. <=38 and > 38. People > 38 years old
# are higher tenured

data2 %>% subset(ni.age < 90) %>%
  ggplot(aes(x = ni.age, y = n.adults)) +
  geom_point() 

# With Age, number of adults living in the family decreases


################# Categorical Variables (Factors)##############################
#-------------
# ni.gender
#-------------
data2 %>% 
  group_by(ni.gender) %>% subset(flag == 'train') %>%
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = ni.gender, y = pct_cancel)) + 
  geom_col() +
  labs(x = "ni.gender Bucket", y = "% Cancel") 

# Neither has any effect. Can make either of them baseline for the model

#---------------
# sales.channel
#---------------
data2 %>% 
  group_by(sales.channel) %>% subset(flag == 'train') %>%
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = sales.channel, y = pct_cancel)) + 
  geom_col() +
  labs(x = "sales.channel Bucket", y = "% Cancel") 

# Broker definitely has some influence on customers since cancellation rate is lower
# comapred to others. This should be a significant variable

# Make Online or Phone as the baseline

#-----------------
# coverage.type
#-----------------
data2 %>% 
  group_by(coverage.type) %>% subset(flag == 'train') %>% subset(flag == 'train') %>%
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = coverage.type, y = pct_cancel)) + 
  geom_col() +
  labs(x = "coverage.type Bucket", y = "% Cancel") 
# This has no effect on the cancellation. Make any one of them baseline

#----------------
# dwelling.type
#----------------
data2 %>% 
  group_by(dwelling.type) %>% subset(flag == 'train') %>%
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = dwelling.type, y = pct_cancel)) + 
  geom_col() +
  labs(x = "dwelling.type Bucket", y = "% Cancel")
# No striking effect

data2 %>% 
  group_by(dwelling.type) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = dwelling.type, y = pct_cancel)) + 
  geom_col() +
  labs(x = "dwelling.type Bucket", y = "% Cancel") 
table(data2$dwelling.type)

# Landlord is only present in Test dataset. We cannot do anything since we cannot
# train our model with landlord. Need to exclude it from the model

#----------
# credit
#----------
data2 %>% subset(flag == 'train') %>%
  group_by(credit) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = credit, y = pct_cancel)) + 
  geom_col() +
  labs(x = "credit Bucket", y = "% Cancel") 

# If the credit level is low then high chance of cancellation. Makes sense.
# Make medium as the baseline.

#-------------
# house.color
#-------------
data2 %>% subset(flag == 'train') %>%
  group_by(house.color) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = house.color, y = pct_cancel)) + 
  geom_col() +
  labs(x = "house.color Bucket", y = "% Cancel") 

# Intuitively house color and policy cancellation doesn't make sense'. There is no
# striking feature about the numbers as well. Make Blue as the baseline.

#-----------
# claim.ind
#-----------
data2 %>% 
  group_by(claim.ind) %>% subset(flag == 'train') %>%
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = claim.ind, y = pct_cancel)) + 
  geom_col() +
  labs(x = "claim.ind Bucket", y = "% Cancel") 

#As claim ind is 1 there is higher cancellation. Probably for these, the premium is
# increased
data2 %>% subset(flag == 'train') %>%
  group_by(claim.ind) %>% 
  summarise(prem = mean(premium)) %>%
  ggplot(aes(x = claim.ind, y = prem)) + 
  geom_col() +
  labs(x = "claim.ind Bucket", y = "Premium") 
# No, the premium is similar for both claim ind

# These people have already claimed and then cancelled. It means insurance company 
# challenged the claim and getting the coverage wasn't easy for them. That's why
# they probably cancelled

#--------------------
# ni.marital.status
#--------------------
data2 %>% subset(flag == 'train') %>%
  group_by(ni.marital.status) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = ni.marital.status, y = pct_cancel)) + 
  geom_col() +
  labs(x = "ni.marital.status Bucket", y = "% Cancel") 

# Marital Status doesn't seem to have any direct effect on cancellations.

#------
# year
#------
data2 %>% subset(flag == 'train') %>%
  group_by(year) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = year, y = pct_cancel)) + 
  geom_col() +
  labs(x = "year Bucket", y = "% Cancel") 

# After 2013 there is a spike in cancellation (from 2014). Then it decreases over 
# years

data2 %>% subset(flag == 'train' & zip.code == 15025) %>%
  group_by(year) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = year, y = pct_cancel)) + 
  geom_col() +
  labs(x = "year Bucket", y = "% Cancel") 

#But if we look at the numbers by different zip codes, the story is different. 
# Zip Codes Might play an important role in the analysis

data2 %>% 
  group_by(year) %>% 
  summarise(n_cancel = sum(cancel),
            n_total = n(),
            pct_cancel = n_cancel / n_total) %>%
  ggplot(aes(x = year, y = pct_cancel)) + 
  geom_col() +
  labs(x = "year Bucket", y = "% Cancel") 

# We have 2017 data only in Test dataset. This is equivalent to forecasting.
# We need to create a trend variable with year. We will treat that as continuous
# variable. Example - 2013 will be 0.2, 2014 - 0.4, 2015 - 0.6, 2016 - 0.8, 2017 -
# 1.0


#-----------------------------------
#Replacing Missing Values(outliers)
#-----------------------------------
# Now let's replace the created missing values(outliers) using PMM

miss_imp <- all_vars[all_vars != 'zip.code'] #Zip Code cannot be predicted using this method
# since there are too many levels

exp <- paste0("~ ", miss_imp[1])
for (i in 2:length(miss_imp))
{
  exp <- paste0(exp,"+",miss_imp[i])
}
impute_arg <- aregImpute(as.formula(exp), data = data2, n.impute = 5)
impute_arg

imputed <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=data2, list.out=TRUE, pr=FALSE, check=FALSE))

data2 <- bind_cols(imputed, data2[, c("cancel", "id", "flag", "zip.code")])

sum(is.na(data2)) #0 Missing Values

data2 %>%
  ggplot(aes(ni.age)) +
  geom_histogram(fill = "red", bins = 30) +
  scale_x_discrete(limits = seq(0, 300, 50))
# No Outliers
summary(data2$ni.age)


```