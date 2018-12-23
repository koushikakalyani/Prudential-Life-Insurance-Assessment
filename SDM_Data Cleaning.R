### SDM Project ### 


# Prudential Data

# Exploratory Analysis

library(ggplot2)
library(gridExtra)

train <- read.table("C:/Users/gd_su/Documents/Classes/Prudential_Kaggle/data/train.csv", sep=",", header=TRUE)
test <- read.table("C:/Users/gd_su/Documents/Classes/Prudential_Kaggle/data/test.csv", sep=",", header=TRUE)

#splitting variables into categorical, cont and discrete
var_cat <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))
var_cont <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5")
var_discrete <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                    paste("Medical_Keyword_", 1:48, sep=""))

# categorical set in train
train.cat <- train[, var_cat]
test.cat <- test[, var_cat]

train.cont <- train[, var_cont]
test.cont <- test[, var_cont]

train.disc <- train[, var_discrete]
test.disc <- test[, var_discrete]

# categorical var -> convert to as.factor
train.cat <- as.data.frame(lapply(train.cat, factor))
test.cat <- as.data.frame(lapply(test.cat, factor))

#look at the data and cols
str(train.cont) #59381 obs. of  13 variables
str(train.disc) #59381 obs. of  52 variables
str(test.cont) #19765 obs. of  13 variables
str(test.disc) #19765 obs. of  52 variables

summary(train.cont) 
summary(train.disc)

summary(test.cont)
summary(test.disc)

head(train)
head(test)

str(train.cat) #59381 obs. of  61 variables
str(test.cat) # 19765 obs. of  61 variables

summary(train.cat)
summary(test.cat)

#####################################################################3

# missing data

# percent missing data in train and test
sum(is.na(train)) / (nrow(train) * ncol(train)) #0.05171885
sum(is.na(test)) / (nrow(test) * ncol(test)) #0.05205894

# look for col wise missing val in train and test
apply(train, 2, function(x) { sum(is.na(x)) })
apply(test, 2, function(x) { sum(is.na(x)) })

# find the col that has max NAs and min NAs in train and test with respect to each level in target variable
train.na.per.response <- sapply(sort(unique(train$Response)), function(x) { apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train.na.per.response


# percent of NA in each target level
round(colSums(train.na.per.response) / sum(train.na.per.response), digits=4)

# plot missingness strucutre wrt target var

plotMissingnessStr <- function(data.in, title=NULL) {
  r <- as.data.frame(ifelse(is.na(data.in), 0, 1))
  r <- r[,order(colMeans(is.na(data.in)))]
  dat.tmp <- expand.grid(list(x=1:nrow(r), y=colnames(r)))
  dat.tmp$r <- as.vector(t(r))
  
  ggplot(dat.tmp) + geom_tile(aes(x=x, y=y, fill=factor(r))) + scale_fill_manual(values=c("black", "white"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plotMissingnessStr(data.in=train[,apply(train, 2, function(x) { sum(is.na(x)) > 0 })], title="Train data set")
plotMissingnessStr(data.in=test[,apply(test, 2, function(x) { sum(is.na(x)) > 0 })], title="Test data set")









