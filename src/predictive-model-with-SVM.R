#
# Build a model to predict collaboration of 
# each pair of authors in the next year
# using the Support Vector Machine (SVM) algorithm
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

# if (!require("e1071")) {
#   install.packages("e1071") 
# }

# for SVM
library("e1071")

# consider a feature as the number of collaborations of two authors in each year,
# as two aothors with 'more collab.' in the 'recent years' might also have collab. in the next year
res <- sqlQuery(dbhandle, 'SELECT distinct count (*) as numOfPapers, aa1.author_key as auth1, aa2.author_key as auth2, py.year
  	                       FROM author_article as aa1 INNER JOIN pyear as py 
                           on (py.year <= 1990 and py.year >= 1980 and aa1.PID = py.PID)
                           INNER JOIN author_article as aa2
                           on (aa1.author_key != aa2.author_key and aa1.PID = aa2.PID)
                           group by aa1.author_key, aa2.author_key, py.year')
# backUp
rData = res

rData$Concatenation.IDs <- do.call(paste, c(rData[c("auth1", "auth2")], sep = " ")) 
mData = data.frame(unique(rData$Concatenation.IDs))

# change mData object name to Concatenation.IDs
names(mData) ='Concatenation.IDs'

# backUp
fData = mData

# number of fData rows
lenRes = length(rownames(fData))

# y = 0 0 0 0 0 0 ...
y = c(rep(0, lenRes))

# construct fData (real Data), 1980-1989 (training data) + 1990 (test data)
for (i in 0:9){
  yearTable = rData[rData$year == 1980 + i,]
  fData = cbind(fData, y)
  fData[(fData$Concatenation.IDs %in% yearTable$Concatenation.IDs),]$y = yearTable$numOfPapers
  
  # change the column name to desired year
  colnames(fData)[i+2] <- as.character(1980 + i)
}

yearTable = rData[rData$year==1990,]
fData = cbind(fData, y)
fData[(fData$Concatenation.IDs %in% yearTable$Concatenation.IDs),]$y = 1 #1: we just need 1 instead of yearTable$numOfPapers, because these authors belong to class 1.
colnames(fData)[12] <- as.character(1990)

# train 0.9*279308 (90%) of data
train <- svm(fData[1:251377, 2:11], fData[1:251377, 12], type = "C-classification")

# predict rest (10%)
pred <- predict(train , fData[251377:279308, 2:11])

numPred = as.numeric(as.character(pred))  # containing the predictions
label = fData[251377:279308, 12]  # containing the true class labels

# for prediction function
install.packages("ROCR") 
library(ROCR)

# transform the input data into a standardized format.
predLab = prediction(numPred, label)

# precision and Recall
perfMeasure <- performance(predLab, "prec", "rec")
plot(perfMeasure)

# num of False Negatives
fn = numPred - label
sum(abs(fn))

# ROC curve
ROC.perf <- performance(predLab, "tpr", "fpr")
plot(ROC.perf)

# for calculating precision and recall
install.packages("caret")
library(caret)

precision <- posPredValue(factor(numPred), factor(label), positive="1")  # the precision of the model is 1
recall <- sensitivity(factor(numPred), factor(label), positive="1")  # the recall of the model is 0.9946098

# we could increase recall by correctly predicting all the real collab., however, precision
# decreases in this case due to many false positives raised by incorrect predictions of collab.
# which are not real collab.
