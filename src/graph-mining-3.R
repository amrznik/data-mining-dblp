#
# Graph Mining
# 
# Considering paper ids as the graph nodes and 
# paper cited and citing as the graph edges
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

paper_ref_q <- sqlQuery(dbhandle, 'SELECT * FROM reference')

dataFrame.g <- graph.data.frame(d = paper_ref_q, directed = TRUE)

degree_node <- degree(dataFrame.g, mode = "all")

# paper with maximum number of cited and citing in total
print(max(degree_node))

# number of cited and citing papers on average
mean(degree_node) 

# finding the number of papers that are between two papers and cited each other on avarage
mean_dist_node_graph <- mean_distance(dataFrame.g, directed = TRUE, unconnected = TRUE)

# finding the max. number of papers that are between two papers and cited each other
diameter(dataFrame.g, directed = TRUE, unconnected = TRUE, weights = NULL)

# PaperRefGraph <- function(papref){
#   ref <- papref[0:50,]
#   dataFrame.g <- graph.data.frame(d = ref, directed = TRUE)
#   plot(dataFrame.g, vertex.label = V(dataFrame.g)$name, edge.arrow.size= .1,
#        vertex.color = "green", vertex.size = 18,
#        vertex.frame.color="yellow", vertex.label.color="black",
#        vertex.label.cex = 0.9, edge.curved=0.2)
#   return(dataFrame.g)
# }

# PaperRefGraph(paper_ref_q)