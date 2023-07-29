#
# Dimensionality reduction on the abstracts of 
# papers published in [1971, 1975]
# using Principal Component Analysis (PCA)
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

pap_abstract <- sqlQuery(dbhandle, 'SELECT ab.abstract FROM abstract as ab 
                                    INNER JOIN pyear as py
                                    on ab.PID = py.PID and py.year <= 1975 and py.year >= 1971')

# for weighting=weightTf (tf-idf)
library(tm)

# convert pap_abstract (DataFrame) to TermDocumentMatrix
# to get the number of words (Terms) in each abstract (Document)
# and sparsity
matrix_s <- TermDocumentMatrix(Corpus(VectorSource(pap_abstract$abstract)), 
                               control = list(wordLengths = c(1, Inf)))

# 2D matrix. Each row represents a word (dimension), and each column represents an abstract
matrix_freq <- as.matrix(matrix_s)
# matrix_freq[1:20,1:20]
# there are more than 13000 rows (dimensions) in matrix_freq, 
# the data is too sparse and we have many irrelevant/redundant dimensions.
# We need to reduce the dimensionality.

# PCA
pca_existing <- prcomp(matrix_freq, scale. = TRUE)
# pca_existing$x

plot(pca_existing)

# use the first two PCs to represent all the words
pca_existing_df <- as.data.frame(pca_existing$x)

# show first two PCs for head words
head(pca_existing_df[1:2], 10)

plot(PC1~PC2, data = pca_existing_df, 
     main= "Distribution",
     cex = .1, lty = "solid")
text(PC1~PC2, data = pca_existing_df, 
     labels = rownames(matrix_freq),
     cex=.8)

# set the color associated with the mean value for all the abstracts
library(scales)

ramp <- colorRamp(c("yellow", "blue"))
colors_by_mean <- rgb(ramp(as.vector(rescale(rowMeans(matrix_freq), c(0,1)))), max = 255)

plot(PC1~PC2, data = pca_existing_df, 
     main = "Mean Distribution",
     cex = .1, lty = "solid", col = colors_by_mean)
text(PC1~PC2, data = pca_existing_df, 
     labels = rownames(matrix_freq),
     cex=.8, col = colors_by_mean)


# associate color with total sum
ramp <- colorRamp(c("yellow", "blue"))
colors_by_sum <- rgb(ramp(as.vector(rescale(rowSums(matrix_freq), c(0,1)))), max = 255)

plot(PC1~PC2, data = pca_existing_df, 
     main= "Total Sum Distribution",
     cex = .1, lty = "solid", col = colors_by_sum)
text(PC1~PC2, data = pca_existing_df, 
     labels = rownames(matrix_freq),
     cex=.8, col = colors_by_sum)


# existing_df_change <- matrix_freq[2897] - matrix_freq[1]
# ramp <- colorRamp(c("yellow", "blue"))
# colors_by_change <- rgb( 
#  ramp( as.vector(rescale(existing_df_change,c(0,1)))), 
#  max = 255 )
# plot(PC1~PC2, data=pca_existing_df, 
#     main= "Distribution",
#     cex = .1, lty = "solid", col=colors_by_change)
# text(PC1~PC2, data=pca_existing_df, 
#     labels=rownames(matrix_freq),
#     cex=.8, col=colors_by_change)


# biplot(pca_existing)