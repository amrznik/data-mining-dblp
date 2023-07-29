#
# Association Rule Mining on papers published in [2001, 2005] and
# the rest of the papers which cited them
# using Apriori Algorithm with minimum support threshold of minsup=5
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

library(arules)
res <- sqlQuery(dbhandle, 'select re.PID1_from, ab.pid
                           from abstract as ab INNER JOIN pyear as py
                           ON ab.PID = py.PID
                           and py.year >= 2001 and py.year <= 2005
                           INNER JOIN reference as re on re.PID2_to = ab.pid ')

# According to res, MyData should have two columns including the paper and an itemset of
# all papers cited that paper
tr <- read.transactions("MyData.txt",format = "basket", sep = ",", cols = NULL)

max_freq_is <- apriori(tr, parameter = list(target = "maximally frequent itemsets", support = 0.00003)) # support=minsup/length(readLines("MyData.txt"))
cls_freq_is <- apriori(tr, parameter = list(target = "closed frequent itemsets", support = 0.00003))

itemFrequencyPlot(tr, topN=10, type="absolute", main="Item Frequency")

inspect(tail(max_freq_is,10))
summary(max_freq_is)

inspect(head(tr,10))

itemsets <- eclat(tr, parameter = list(supp = 0.00003, maxlen=50))
is.maximal(itemsets)
inspect(head(itemsets,1000))

