# Reverse projection
# create count table where organizations are columns (make y)
tbl <- table(edges[c(1,2)])
# pull out column names -- these are mode 1 (orgs)
orgids <- colnames(tbl)
# take the cross-product to get a co-occur matrix
comat <- crossprod(tbl)
# assign self-co-occurrences 0
diag(comat) <- 0
# This should be symmetrical
isSymmetric(comat)
# make a df and add ids as the column names
comat <- data.frame(comat)
colnames(comat) <- orgids
dim(comat)
# Remove those with no co-occurrence at all
comat2 <- comat[rowSums(comat, na.rm = T) != 0, 
                colSums(comat, na.rm = T) != 0]
dim(comat2)
# make co-occur df into matrix object
comat3 <- as.matrix(comat2)

# create a graph so that it can be converted into a weighted edgelist
g <- igraph::graph_from_adjacency_matrix(comat3, weighted = T, mode = 'undirected')
deg <- data.frame(degree(g))
el_proj <- igraph::get.data.frame(g)

# rename columns and sort
colnames(el_proj) <- c("V1", "V2", "weight")


deg <- data.frame(degree(g))
deg$id = as.numeric(rownames(deg))
colnames(deg)[1] <- 'n_connections'
nodes <- read.csv('data/nodelist_twomode.csv')
library(tidyverse)
df <- left_join(deg, nodes)
df$funds_numeric <- str_remove_all(df$funds, ",|\\$")

write.csv(df, '~/Desktop/dst_projects.csv', row.names = F)
