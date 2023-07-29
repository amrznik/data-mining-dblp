#
# Sequence Mining on the titles of papers published in [1980, 1990]
# using cSPADE with minimum support threshold of minsup=3
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

library(tm)
library(Matrix)
library(arules)
library(arulesSequences)

res <- sqlQuery(dbhandle, 'select ti.PID, ti.title
                           from title as ti INNER JOIN pyear as py
                           ON ti.PID = py.PID and py.year <= 1990 and py.year >= 1980')

#---------------------------------------------------------------------------#
# some preprocessing

# convert to lower case
lower_title <- tolower(res$title)

# remove ponctuations
df <- data.frame(lapply(lower_title, as.character), stringsAsFactors=FALSE)
documents = gsub('[[:punct:]]',' ', df)

# remove stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
documents = stringr::str_replace_all(documents, stopwords_regex, '')

# stemming
final_doc <- stemDocument(documents)

#---------------------------------------------------------------------------#
# finding most frequent subsequences of title words

res = res[order(res$PID),]
res_formed = data.frame(sequenceID = c(), eventID = c(), SIZE=c(), final_doc = c())


for(i in 1:nrow(res)){
  res_formed = rbind(res_formed, data.frame(sequenceID = i,
                                            eventID = i,
                                            SIZE = length(final_doc[i]),
                                            final_doc = final_doc[i]))
}

res_items = strsplit(as.character(res_formed$final_doc), " ")
res_trans = as(res_items, "transactions")
res_trans@itemsetInfo = data.frame(sequenceID = res_formed$sequenceID,
                                   eventID = res_formed$eventID,
                                   SIZE = res_formed$SIZE)

res_final = cspade(res_trans, parameter = list(support = 0.00002), control = list(verbose = TRUE)) # support=minsup/nrow(res_formed)

# sapply(res$title, tolower)
# install.packages("NLP", repos="https://cran.r-project.org/web/packages/NLP/index.html")
# install.packages('tm',dependencies = TRUE)
# install.packages("arulesSequences")
