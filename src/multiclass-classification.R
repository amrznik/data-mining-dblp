#
# Multiclass Classification
# Build a classifier to predict an interval for 
# number of citations of the papers in the next year
# using the Support Vector Machine (SVM) algorithm
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

# for SVM
library("e1071")

library(ROCR)

paper_year <- sqlQuery(dbhandle, 'SELECT * FROM pyear 
                                  WHERE year <= 1990 and year >= 1980
                                  ORDER BY PID ASC')

# feature: number of authors of a paper
num_of_auth <- sqlQuery(dbhandle, 'SELECT aa.PID, count(*) as numOfAuth
                                   FROM author_article as aa INNER JOIN pyear as py on aa.PID = py.PID and
                                   py.year <= 1990 and py.year >= 1980
                                   group by aa.PID
                                   ORDER BY aa.PID ASC')

# feature: sum of citations for authors of a paper
sum_of_auth_cite <- sqlQuery(dbhandle, 'SELECT aa.pid, COUNT(*) as sumOfAuthCite
                                        FROM author_article as aa INNER JOIN pyear as py on aa.PID = py.PID and 
                                        py.year <= 1990 and py.year >= 1980
                                        INNER JOIN author_reference as ar on aa.author_key = ar.eid_to
                                        group by aa.pid
                                        ORDER BY aa.PID ASC')

# feature: number of references for a paper
num_of_ref <- sqlQuery(dbhandle, 'SELECT re.PID1_from, count(*) as numOfref
                                  FROM reference as re INNER JOIN pyear as py 
                                  on re.PID1_from = py.PID and
                                  py.year <= 1990 and py.year >= 1980
                                  group by re.PID1_from
                                  ORDER BY re.PID1_from ASC')
# feature: length of a paper title
length_of_title <- sqlQuery(dbhandle, 'SELECT DISTINCT ti.PID, LEN(ti.title) as lengthOfTitle
                                       FROM title as ti INNER JOIN pyear as py
                                       on ti.PID = py.PID and py.year <= 1990 and py.year >= 1980
                                       ORDER BY ti.PID ASC')

# feature: number of citations of a paper
num_of_cite <- sqlQuery(dbhandle, 'SELECT re.PID2_to, count(*) as numOfCite
                                   FROM reference as re INNER JOIN pyear as py on re.PID2_to = py.PID and
                                   py.year <= 1990 and py.year >= 1980
                                   group by re.PID2_to
                                   ORDER BY re.PID2_to ASC')


# construct rData, 1980-1989 (training data) + 1990 (test data)
rData = unique(paper_year)
rData = rData[!duplicated(rData$PID),]

# number of rData rows
lenRes = length(rownames(rData))

# y = 0 0 0 0 0 0 ...
y = c(rep(0,lenRes))

# add feature: number of authors of a paper
rData = cbind(rData, y)
rData[(rData$PID %in% num_of_auth$PID),]$y = num_of_auth$numOfAuth
colnames(rData)[3] <- 'numOfPapAuth'


# add feature: sum of citations for authors of a paper
rData = cbind(rData, y)
rData[(rData$PID %in% sum_of_auth_cite$pid),]$y = sum_of_auth_cite$sumOfAuthCite
colnames(rData)[4] <- 'sumOfPapAuthCite'


# add feature: number of references for a paper
rData = cbind(rData, y)
rData[(rData$PID %in% num_of_ref$PID1_from),]$y = num_of_ref$numOfref
colnames(rData)[5] <- 'numOfPapRef'

# add feature: length of a paper title
rData = cbind(rData, y)
rData[(rData$PID %in% length_of_title$PID),]$y = length_of_title$lengthOfTitle
colnames(rData)[6] <- 'lengthOfPapTitle'

# add feature: number of citations of a paper
rData = cbind(rData, y)
rData[(rData$PID %in% num_of_cite$PID2_to),]$y = num_of_cite$numOfCite
colnames(rData)[7] <- 'numOfPapCite'

# rData$numOfPapCite = NULL

# add feature: Average of citations of a paper
rData = cbind(rData, y)
rData$y = rData$numOfPapCite / (2011 - rData$year)
colnames(rData)[8] <- 'avgOfPapCite'

# eliminate zero values of numOfPapCite
rts = rData
rts <- rts[rts$numOfPapCite != 0,]

# five classes: five intervals for the number of citations
rts = cbind(rData, y)
rts[rts$numOfPapCite <= 20 ,]$y = 1  # class 1
rts[rts$numOfPapCite > 20 & rts$numOfPapCite <= 40 ,]$y = 2  # class 2
rts[rts$numOfPapCite > 40 & rts$numOfPapCite <= 100 ,]$y = 3  # class 3
rts[rts$numOfPapCite > 100 & rts$numOfPapCite <= 500 ,]$y = 4  # class 4
rts[rts$numOfPapCite > 500 , ]$y = 5  # class 5
colnames(rts)[9] <- 'label'

# train: 1980-1989
rts$label <- as.integer(rts$label)
train_data <- rts[rts$year >= 1980 & rts$year <= 1989, 3:8]
labels <- rts[rts$year >= 1980 & rts$year <= 1989, 9]
train <- svm(train_data, labels, type = "C-classification")

# predict rest: 1990
test_data <- rts[rts$year == 1990, 3:8]
pred <- predict(train, test_data)
summary(pred)

numPred = as.numeric(as.character(pred))  # containing the predictions

label_year90 <- rts[rts$year == 1990,]$label  # real data

table(numPred, label_year90)  # compare predicted data vs real data
# sum of the numbers in the main diagonal: 5918 predicted classes out of all 6010 classes are correct,
# thus the model gets the accuracy of 98%

# predLab = prediction(as.numeric(numPred), as.numeric(label_year90))
# Precision and Recall
# perfMeasure <- performance(numPred, "prec", "rec")
# plot(perfMeasure)

