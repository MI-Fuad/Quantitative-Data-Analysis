# state   |---->  Grouped state of origin
# sex     |---->  Sex of patient.
# diag    |---->  Julian date of diagnosis (the number of days since 1970-01-01).
# death   |---->  Julian date of death or end of observation (the number of days since 1970-01-01).
# status  |---->  "A" (alive) or "D" (dead) at the end of observation.
# T.categ |---->  Reported transmission category.
# age     |---->  Age in years, at diagnosis.
# year    |---->  The year of observation (normal calendar)
# outcome |---->  ‘1’ if the patient died in the year of observation specified in ‘year’, ‘0’ if survived


#PREWORK
#to avoid max print error
options(max.print = 999999999)
#to not show scientific version
options(scipen = 999)

#SETTING WORKING DIRECTORY 
setwd("/Users/MIFuad/Google Drive/1.MSC/QDA/Coursework") 

a2adata <- read.csv('data/Aids2ann.csv', header = T, colClasses = c("NULL", NA,NA,NA,NA,NA,NA,NA,NA,NA))
dim(a2adata)
head(a2adata)





# VARIABLE DECLARATIONS ================================================
X <- a2adata$X
state <- a2adata$state
sex <- a2adata$sex
diag <- a2adata$diag
death <- a2adata$death
status <- a2adata$status
tcat <- a2adata$T.categ
age <- a2adata$age
year <- a2adata$year
outcome <- a2adata$outcome
outcomeaf <- as.factor(outcome)

#check the dimension of dataset
dim(a2adata)
#check if there's any missing data in the dataset
sapply(a2adata, function(x)sum(is.na(x)))
range(death)

#VARIABLE STATUS ================================================
#STATUS
frstatus <- table(status)
frstatus
#relative frequency
rfstatus = frstatus/length(status)
rfstatus
ggplot(a2adata, aes(x = status)) + geom_bar(fill = "steelblue")


# VARIABLE YEAR================================================
#frequency count for state
yr.freq <-table(year)
yr.freq
#relative frequency count for state
yr.relfreq = yr.freq/length(year)
yr.relfreq
#Bar plot
ggplot(a2adata, aes(x = year)) + geom_bar(fill = "steelblue") + theme_minimal()

#variable OUTCOME

levels(outcomeaf)
freq.outcome <- table(outcomeaf)
freq.outcome
rfoutcome <- freq.outcome/length(outcomeaf)
rfoutcome

ggplot(a2adata, aes(x = outcomeaf)) + geom_bar(fill = "steelblue") + theme_minimal()

#VARIABLE - state
#check how many categories
levels(state)
#frequency count for state
state.freq <-table(state)
state.freq
#relative frequency count for state
rel.freq = state.freq/length(state)
barplot(rel.freq)
#Bar plot
ggplot(a2adata, aes(x = state)) + geom_bar(fill = "steelblue") + theme_minimal()


#VARIABLE - sex
#check how many categories
levels(sex)
#frequency count for sex
sex.freq <-table(sex)
sex.freq
#relative frequency count for sex
sex_rf = sex.freq/length(sex)
sex_rf
#Bar plot
ggplot(a2adata, aes(x = sex)) + geom_bar(fill = "steelblue") + theme_minimal()


#VARIABLE - T.categ
#check how many categories
levels(tcat)
#frequency count for tcat
tcat.freq <-table(tcat)
tcat.freq
#relative frequency count for sex
tcat_rf = tcat.freq/length(tcat)
tcat_rf
#Bar plot
ggplot(a2adata, aes(x = T.categ)) + geom_bar(fill = "steelblue") + theme_minimal()


#Function to count the number of outliers for a particular variable
find_outliers <-function(x) {
  lower_threshold = quantile(x,.25) - 1.5*IQR(x)
  upper_threshold = quantile(x,.75) + 1.5*IQR(x)
  result <- which(x<lower_threshold | x>upper_threshold)
  y = length(result)
  return (y)
}


#VARIABLE - age
#Mean, SD, Variance, Range,Minimum,Maximum,Lower quartile,Median,Upper quartile ,IQR
mean(age)
median(age)
sd(age)
#to get minimum and maximum 
range(age)
#range of variable
age_range = max(age) - min(age)
age_range
var(age)
quantile(age)
IQR(age)
#count of outliers
find_outliers(age)

#Histogram 
ggplot(a2adata, aes(x = age)) + geom_histogram(color = "steelblue",fill = "lightblue", bins=20)+ 
  ggtitle("Age at diagnosis") +theme(plot.title = element_text(hjust = 0.5))

#Box-plot
ggplot(a2adata, aes(x = "", y = age)) + 
  geom_boxplot(color = "steelblue",fill = "lightblue") +  ggtitle("Boxplot for Age at diagnosis")+
  coord_cartesian(ylim = c(0, 84)) + theme(plot.title = element_text(hjust = 0.5))


#VARIABLE - diag
#Mean, SD, Variance, Range,Minimum,Maximum,Lower quartile,Median,Upper quartile ,IQR
mean(diag)
median(diag)
sd(diag)
#to get minimum and maximum 
range(diag)
#range of variable
diag_range = max(diag) - min(diag)
diag_range
var(diag)
quantile(diag)
IQR(diag)
#count of outliers
find_outliers(diag)

#Histogram 
ggplot(a2adata, aes(x = diag)) + geom_histogram(color = "steelblue",fill = "lightblue", bins=25)+ 
  ggtitle("Histogram for Julian date at diagnosis") +theme(plot.title = element_text(hjust = 0.5))

#Box-plot
ggplot(a2adata, aes(x = "", y = diag)) + 
  geom_boxplot(color = "steelblue",fill = "lightblue") +  ggtitle("Boxplot for Julian date at diagnosis")+ theme(plot.title = element_text(hjust = 0.5))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

res <- getmode(diag)
print(res)

#VARIABLE - death
#Mean, SD, Variance, Range,Minimum,Maximum,Lower quartile,Median,Upper quartile ,IQR
mean(death)
median(death)
sd(death)
#to get minimum and maximum 
range(death)
#range of variable
death_range = max(death) - min(death)
death_range
var(death)
quantile(death)
IQR(death)

#count of outliers
find_outliers(death)

#Histogram 
ggplot(a2adata, aes(x = death)) + geom_histogram(color = "steelblue",fill = "lightblue", bins=25)+ 
  ggtitle("Histogram of Julian date of death/end of observ.") +theme(plot.title = element_text(hjust = 0.5))

#Box-plot
ggplot(a2adata, aes(x = "", y = death)) + 
  geom_boxplot(color = "steelblue",fill = "lightblue") +  ggtitle("Boxplot of Julian date of death/end of observ.")+ theme(plot.title = element_text(hjust = 0.5))

#Number of DAYS LIVED after diagnosis
days_lived <- death - diag

mean(days_lived)
median(days_lived)
sd(days_lived)
#to get minimum and maximum 
range(days_lived)
#range of variable
dl_range = max(days_lived) - min(days_lived)
dl_range
var(days_lived)
quantile(days_lived)
IQR(days_lived)

hist(days_lived, breaks = 25)

#Histogram 
ggplot(a2adata, aes(x = days_lived)) + geom_histogram(color = "steelblue",fill = "lightblue", bins=25)+ 
  ggtitle("Histogram days lived after observation") +theme(plot.title = element_text(hjust = 0.5))

#boxplot
ggplot(a2adata, aes(x = "", y = days_lived)) + 
  geom_boxplot(color = "steelblue",fill = "lightblue") +  ggtitle("Boxplot of days lived after observation")+ theme(plot.title = element_text(hjust = 0.5))

boxplot(days_lived)
boxplot(age~tcat,col = "green",
        main="Box plot of type of weather by numbers of users",xlab="Type of Weather", ylab ="Frequency")

#par(mfrow=c(2,2))

#======================================================================================================
# CHI-SQUARE Test for Independence
#======================================================================================================

#========================================
# STATUS and Tcat
#========================================
TABLE1 <- table(status,tcat)
TABLE1
#to get row and column sums
addmargins(TABLE1)
#BARPLOT to examine relationship visually
barplot(TABLE1, beside=T, legend= T)
CHI <- chisq.test(TABLE1, correct = T)
CHI
attributes(CHI)

table_expected <- CHI$expected
table_expected
addmargins(table_expected)

qchisq(0.95,df=7)
qchisq(0.99, df = 7)

#========================================
# STATUS and Tcat
#========================================
table2 <- table(sex,state)
barplot(table2, beside=T, legend= T)
CHI2 <- chisq.test(table2, correct = T)
CHI2
qchisq(0.95,df=3)
qchisq(0.99,df=3)
#========================================
# age and Death
#========================================

plot(age, death,col="lightblue")


#======================================================================================================
# LOGISTIC REGRESSION MULTIPLE
#======================================================================================================

#full model
logistic.model0 <-glm(status ~ state+sex+diag+death+tcat+age+year+outcome, data = a2adata, family = "binomial")
#reduced
logistic.model1 <-glm(status ~ year+death+state, data=a2adata, family = "binomial")

#reduced further
logistic.model2 <-glm(status ~ death+state, data=a2adata, family = "binomial")

#======================================================================================================
# Likelihood Ratio Test (LRT)
#======================================================================================================
anova(logistic.model2, logistic.model0)


qchisq(.99, df=6) 

plot(fitted(logistic.model0),residuals(logistic.model0),col="blue")
lines(lowess(predict(logistic.model0),residuals(logistic.model0)),col="black",lwd=2)
res<-rstandard(logistic.model0)
qqnorm(res, col="blue")
