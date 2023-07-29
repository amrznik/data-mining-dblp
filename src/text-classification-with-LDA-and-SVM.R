#
# 1. Text Classification on the papers published in [1971, 1975] based on the abstracts
# using Latent Dirichlet Allocation (LDA) for topic modelling
# 2. Build a Classifier on the resulting topics based on the titles
# using the Support Vector Machine (SVM) algorithm
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

query <- c("SELECT ab.PID, ab.abstract FROM abstract as ab 
            INNER JOIN pyear as py
            on ab.PID = py.PID and py.year <= 1975 and py.year >= 1971
            and ab.abstract NOT LIKE 'Without Abstract'
            ORDER BY ab.PID ASC")

pap_abstract <- sqlQuery(dbhandle, query)
pap_abstract <- unique(pap_abstract)

query2 <- c("SELECT ab.PID, py.year, ab.abstract, ti.title 
             FROM abstract as ab INNER JOIN title as ti
             ON ab.PID = ti.PID and ab.abstract NOT LIKE 'Without Abstract'
             INNER JOIN pyear as py
             on ab.PID = py.PID and py.year <= 1975 and py.year >= 1971
             ORDER BY ab.PID ASC")

pap_title <- sqlQuery(dbhandle, query2)
pap_title <- unique(pap_title)

#---------------------------------------------------------------------------#
# for Latent Dirichlet Allocation (LDA)
install.packages('topicmodels')
library(topicmodels)

# for create_matrix function
install.packages("RTextTools")
library(RTextTools)

# for weighting=weightTf (tf-idf)
library(tm)

# for tidy
install.packages('tidytext')
library(tidytext)
#---------------------------------------------------------------------------#

# convert pap_abstract (DataFrame) to TermDocumentMatrix
# to see how many words (Terms) are there in each abstract (Document)
# we remove stopwords, punctuations, etc., for a better topic modelling, otherwise
# the resulting topics won't be good enough, for example, LDA will assign a topic
# to stopwords since they have been repeated frequently
matrix_s <- create_matrix(cbind(as.vector(pap_abstract$abstract)), language="english",
                          removeNumbers=TRUE, stemWords=TRUE, 
                          removeStopwords=TRUE, removePunctuation=TRUE,
                          toLower=TRUE, weighting=weightTf)

# create 10 topics by LDA
ap_lda <- LDA(matrix_s, k = 10)
terms(ap_lda)
topics(ap_lda)

# for tidying model objects
ap_topics <- tidy(ap_lda, matrix = "beta")

library(ggplot2)
library(dplyr)

# showing 10 most used words in each topic
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# View(as.data.frame(head(topics(ap_lda), 10)))
# as.data.frame(topics(ap_lda))

# construct rData, 1971-1974 + 1975
rData = pap_title

# number of rData rows
lenRes = length(rownames(rData))

# y = 0 0 0 0 0 0 ...
y = c(rep(0,lenRes))

# add feature: Length of title
rData = cbind(rData, y)
for (i in 1:lenRes){
  rData$y[i] = nchar(as.character(rData$title[i]))
}
colnames(rData)[5] <- 'lengthOfTitle'

# for Cosine
install.packages('lsa')
library(lsa)

# add feature: similarity between Abstract and Title using Cosine
rData = cbind(rData, y)
for (i in 1:lenRes){
  CSString_vector <- c(as.character(rData$abstract[i]), as.character(rData$title[i]))
  corp <- tm::VCorpus(VectorSource(CSString_vector))
  controlForMatrix <- list(language="english",
                           removeNumbers = TRUE, stemWords = TRUE, 
                           removeStopwords = TRUE, removePunctuation = TRUE,
                           toLower = TRUE, wordLengths = c(1, Inf), weighting = weightTf)
  dtm <- DocumentTermMatrix(corp, control = controlForMatrix)
  matrix_of_vector = as.matrix(dtm)
  rData$y[i] <- cosine(matrix_of_vector[1,], matrix_of_vector[2,])
}
colnames(rData)[6] <- 'similarityOfAbsTitle'

# add feature: Label (Topic of a title that has been obtained by LDA for each abstract)
rData = cbind(rData, y)
for (i in 1:lenRes){
  rData$y[i] = topics(ap_lda)[i]
}
colnames(rData)[7] <- 'label'

# for Support Vector Machine (SVM)
library("e1071")

# backup
rts = rData

# train: 1971-1974
rts$label <- as.integer(rts$label)
train_data <- rts[rts$year >= 1971 & rts$year <= 1974, 5:6]
labels <- rts[rts$year >= 1971 & rts$year <= 1974, 7]
train <- svm(train_data, labels, type = "C-classification")

# predict rest: 1975
test_data <- rts[rts$year == 1975, 5:6]
pred <- predict(train, test_data)
# summary(pred)

label_year75 <- rts[rts$year == 1975,]$label # real data
# table(as.data.frame(label_year75))

numPred = as.numeric(as.character(pred))  # containing the predictions
table(numPred, label_year75)  # compare predicted data vs real data

# split 1971-1974 data (training data) with 1975 data (test data)
rts7174 = rts[rts$year != 1975,]
rts75 = rts[rts$year == 1975,]

# create the document term matrix
dtMatrix <- create_matrix(rts7174$title, language="english",
                          removeNumbers=TRUE, stemWords=TRUE, 
                          removeStopwords=TRUE, removePunctuation=TRUE,
                          toLower=TRUE)

# configure the training data
container <- create_container(dtMatrix, rts7174$label, trainSize=1:length(rownames(rts7174)), 
                              virgin=FALSE)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)

# 1975 data
predictionData <- as.character(rts75$title)

# create a prediction document term matrix
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix, language="english",
                            removeNumbers=TRUE, stemWords=TRUE, 
                            removeStopwords=TRUE, removePunctuation=TRUE,
                            toLower=TRUE)

# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels = rep(0,predSize), 
                                        testSize = 1:predSize, virgin=FALSE)

# predict
results <- classify_model(predictionContainer, model)

# compare predicted results with real data
final <- ifelse(rts75$label == results$SVM_LABEL, 1, 0)
as.data.frame(table(final))
# 859 predicted classes out of all 1046 classes are correct, thus the model gets the accuracy of 82%.


# tdm_s <- TermDocumentMatrix(Corpus(VectorSource(pap_abstract$abstract)))  # convert DataFrame to TermDocumentMatrix
# ap_lda <- LDA(tdm_s, k = 10, control = list(seed = 1234))
