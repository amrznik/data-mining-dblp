#
# Graph Mining
# 
# Frequent Pattern (FP-Tree) Mining on
# authors considering their joint papers (FP-Growth Algorithm)
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

if (!require("igraph")) {
  install.packages("igraph")
}

library("igraph")

AuthPaperGraph <- function(AuthQ){
  AuthGraph <- AuthQ[order(AuthQ[,2]),,drop = FALSE]
  rownames(AuthGraph) = NULL
  TempGraph = data.frame(AuthGraph[1:21,]$PID, AuthGraph[1:21,]$author_key)
  dataFrame.g <- graph.data.frame(d = TempGraph, directed = TRUE)
  plot(dataFrame.g, vertex.label = V(dataFrame.g)$name, edge.arrow.size = .1,
       vertex.color="green", vertex.size = 18,
       vertex.frame.color="yellow", vertex.label.color="black",
       vertex.label.cex = 0.9 , edge.curved=0.2)
  return(AuthGraph)
}

# pruning the tree by removing papers with less than 'minSup' authors
AuthPaperPrune <- function(AuthArticle, minSup){
  auth <- AuthArticle
  nOccur <- data.frame(table(auth$PID))
  prune <- auth[auth$PID %in% nOccur$Var1[nOccur$Freq >= minSup],]
  AuthPaperGraph(prune)
}

# finding authors whose order is the same in their joint papers
AuthSequential <- function(AuthGraph){
  AuthOrd <- AuthGraph[order(AuthGraph[,2], AuthGraph[,1]),]
  authsort <- AuthGraph[row.names(AuthGraph) == row.names(AuthOrd),]
  return(authsort)
}

auth_paper_graph <- sqlQuery(dbhandle, 'select * from author_article')

AuthPaperGraph(auth_paper_graph)

# AuthPaperPrune(auth_paper_graph, 2)
auth_paper_prune <- AuthPaperPrune(auth_paper_graph, 2)

auth_seq <- AuthSequential(auth_paper_prune)

auth_paper_prune_2 <- AuthPaperPrune(auth_seq, 2)
