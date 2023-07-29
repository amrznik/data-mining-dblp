#
# Graph Mining on the authors of papers published in [1980, 1990]
# 
# Considering authors as the graph nodes and 
# having joint papers as the graph edges
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

auth_paper_q <- sqlQuery(dbhandle, 'SELECT aa1.author_key, aa2.author_key
  	                                FROM author_article as aa1 INNER JOIN pyear as py 
                                    on (py.year <= 1990 and py.year >= 1980 and aa1.PID = py.PID)
                                    INNER JOIN author_article as aa2
                                    on (aa1.author_key != aa2.author_key and aa1.PID = aa2.PID)')  

AuthPaperAuthGraph <- function(AuthQ){
  res <- AuthQ[1:60,]
  dataFrame.g <- graph.data.frame(d = res, directed = FALSE)
  plot(dataFrame.g, vertex.label = V(dataFrame.g)$name, edge.arrow.size= .1,
       vertex.color = "green", vertex.size = 18,
       vertex.frame.color="yellow", vertex.label.color="black",
       vertex.label.cex = 0.9)
  return(dataFrame.g)
}

# for the whole graph
# AuthPaperAuthGraph <- function(AuthQ){
#   dataFrame.g <- graph.data.frame(d = AuthQ, directed = FALSE)
#   return(dataFrame.g)
# }

auth_paper_auth <- AuthPaperAuthGraph(auth_paper_q)

#---------------------------------------------------------------------------#
# finding authors with many joint papers

eb <- edge.betweenness.community(auth_paper_auth)

mod <- sapply(0:ecount(auth_paper_auth), function(i){
  g <- delete.edges(auth_paper_auth, eb$removed.edges[seq(length=i)])
  clst <- clusters(g)$membership
  modularity(auth_paper_auth,clst)
})

g <- delete.edges(auth_paper_auth, eb$removed.edges[seq(length = which.max(mod)-1)])
V(auth_paper_auth)$color = clusters(g)$membership

auth_paper_auth$layout <- layout.fruchterman.reingold

plot(auth_paper_auth)
plot(g)
#---------------------------------------------------------------------------#

mean_dist_node_graph <- mean_distance(auth_paper_auth, directed = FALSE, unconnected = TRUE)

mean_dist_node_comm <- mean_distance(g, directed = FALSE, unconnected = TRUE)

degree_node <- degree(auth_paper_auth, mode = "all")
print(max(degree_node))
