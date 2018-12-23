  # Random Forest and GBM using h20

library(dplyr)
library(h2o)
library(caret)
library(rpart)
library(rpart.plot)
library(var)

set.seed(42)

In = read.csv("C:/Users/gd_su/Documents/Classes/Prudential_Kaggle/data/input.csv")

#RPART

#partition the data set 
s <- sample(nrow(In), 0.70*nrow(In))
train_data <- In[s, ]
test_data <- In[-s, ]

rpt_mod <- rpart (as.factor(Response) ~ .-(Id+InsuredInfo_4+Insurance_History_3+Insurance_History_4+Insurance_History_7+Insurance_History_9+Medical_History_36+Medical_Keyword_8+Insurance_History_7+
                                             Insurance_History_9+Medical_Keyword_44+Medical_Keyword_21+Product_Info_1+Medical_Keyword_7+Medical_Keyword_30+
                                             + Medical_Keyword_13+Medical_Keyword_4+Medical_Keyword_47+Medical_Keyword_1+Medical_Keyword_10+Medical_Keyword_17+Medical_Keyword_36+Medical_Keyword_48+Medical_Keyword_43+Medical_Keyword_42
                                           +Medical_Keyword_36+Medical_Keyword_35+Medical_Keyword_24+Medical_Keyword_32+Medical_Keyword_23+Product_Info_5+InsuredInfo_3+Medical_Keyword_5+Medical_Keyword_14+
                                             Employment_Info_5 + Medical_Keyword_40+Medical_Keyword_27+Medical_Keyword_28+Medical_History_16+Medical_History_9+Medical_History_8+Employment_Info_3+ Medical_History_34+Medical_History_26+Medical_History_25), data=train_data) 
plot(rpt_mod) 
text(model1, use.n = T, digits = 3, cex = 0.6)
preds_rpart <- predict(rpt_mod, newdata=test_data, type='class')
confusionMatrix(preds_rpart, test_data$Response) #Accuracy = 0.4725

cat("Variable Importance")
rptImp = varImp(rpt_mod)
head(rptImp, 10)

#removed cols: "Medical_History_10", 

# Convert non-numeric cols to factors.
factorVariables <- c("Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", 
                     "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", 
                     "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", 
                     "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", 
                     "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", 
                     "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", 
                     "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", 
                     "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", 
                     "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", 
                     "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", 
                     "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", 
                     "Medical_History_39", "Medical_History_40", "Medical_History_41")

for (n in colnames(In[factorVariables])){
  In[[n]] <- as.factor(In[[n]])
}

In[["Response"]] <- as.factor(In[["Response"]]) # only for rf and nb

In[["Response"]] <- as.numeric(In[["Response"]]) # run this only for gbm


#split for train and test sets for h20 package
s <- sample(nrow(In), 0.70*nrow(In))
train_local <- In[s, ]
test_local <- In[-s, ]


# define target and features

target = 'Response'

features = colnames(In)[!(colnames(In) %in% 
                               c('Id', 'Response', 'Product_Info_2', 
                                 'InsuredInfo_4','Insurance_History_3',
                                 'Insurance_History_4','Insurance_History_7',
                                 'Insurance_History_9','Medical_History_36',
                                 'Medical_Keyword_8','Insurance_History_7',
                                 'Insurance_History_9','Medical_Keyword_44', 
                                 'Medical_Keyword_21','Product_Info_1',
                                 'Medical_Keyword_7','Medical_Keyword_30',
                                 'Medical_Keyword_13','Medical_Keyword_4',
                                 'Medical_Keyword_47','Medical_Keyword_1',
                                 'Medical_Keyword_10','Medical_Keyword_17',
                                 'Medical_Keyword_36','Medical_Keyword_48',
                                 'Medical_Keyword_43','Medical_Keyword_42',
                                 'Medical_Keyword_36','Medical_Keyword_35',
                                 'Medical_Keyword_24','Medical_Keyword_32',
                                 'Medical_Keyword_23','Product_Info_5',
                                 'InsuredInfo_3','Medical_Keyword_5',
                                 'Medical_Keyword_14','Employment_Info_5',
                                 'Medical_Keyword_40','Medical_Keyword_27','Medical_Keyword_28',
                                 'Medical_History_16','Medical_History_9','Medical_History_8', 'Medical_History_10', 
                                 'Employment_Info_3','Medical_History_34','Medical_History_26',
                                 'Medical_History_25'))]


# Open H2o instance and convert data
h2o.init(nthreads = -1, max_mem_size = '4G')

trainHex <- as.h2o(train_local)
testHex <- as.h2o(test_local)


# Random Forest
cat("Training Random Forest")
h2oRf <- h2o.randomForest(x = features, y = target, ntrees = 150, max_depth = 50,
                       training_frame = trainHex)

preds_rf <- as.data.frame(h2o.predict(h2oRf, testHex))
confusionMatrix(preds_rf$predict, test_local$Response)

cat("Variable Importance")
rfImp = h2o.varimp(h2oRf)
head(rfImp, 10)
h2o.varimp_plot(h2oRf, num_of_features = 10)

#h20 naive bayes
cat("Training Naive Bayes")
h2oNaive <- h2o.naiveBayes(x=features, y= target, training_frame = trainHex, laplace = 3)
preds_nb <- as.data.frame( predict(h2oNaive, testHex))
confusionMatrix(preds_nb$predict, test_local$Response) 

# Gradient Boosting Machine
cat("Training gbm")
h2oGbm <- h2o.gbm(x=features, y=target, training_frame = trainHex, 
                  learn_rate=0.025, ntrees=235, max_depth=22, min_rows=3, distribution="poisson")

preds_gbm <- as.data.frame( predict(h2oGbm, testHex))
predicting_gbm <- as.data.frame(test_local$Id)
predicting_gbm <- cbind(predicting_gbm, round(preds_gbm$predict))
names(predicting_gbm) <- c("Id", "Response")
head(predicting_gbm)

cat("Confusion Matrix")
confusionMatrix(predicting_gbm$Response, test_local$Response) 

cat("Variable Importance")
gbmImp = h2o.varimp(h2oGbm)
head(gbmImp, 10)
h2o.varimp_plot(h2oGbm , num_of_features = 10)

