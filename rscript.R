install.packages("igraph")
library(igraph)
g1 = graph.full(5, directed = FALSE, loops = FALSE)
K5 = g1
K5_al = get.adjacency(K5)

g2 = graph.full(3, directed = FALSE, loops = FALSE)
K5.3 = g1 + g2
K5.3_al = get.adjacency(K5.3)

K5.3e = K5.3 + edges(1,7)
K5.3e_al = get.adjacency(K5.3e)

B2.3 = graph.full.bipartite(2,3,directed = FALSE)
B2.3_al = get.adjacency(B2.3)

S5 = graph.star(n=5,mode="undirected")
S5_al = get.adjacency(S5)

P5 = graph(c(1,2,2,3,3,4,4,5,5,6),directed=FALSE)
P5_al = get.adjacency(P5)



K5_diag = diag(degree(K5))
K5.3_diag = diag(degree(K5.3))
K5.3e_diag = diag(degree(K5.3e))
B2.3_diag = diag(degree(B2.3))
S5_diag = diag(degree(S5))
P5_diag = diag(degree(P5))


K5_laplacian = graph.laplacian(K5)
K5.3_laplacian = graph.laplacian(K5.3)
K5.3e_laplacian = graph.laplacian(K5.3e)
B2.3_laplacian = graph.laplacian(B2.3)
S5_laplacian = graph.laplacian(S5)
P5_laplacian = graph.laplacian(P5)

eigen(K5_al)
eigen(K5.3_al)
eigen(K5.3e_al)
eigen(B2.3_al)
eigen(P5_al)


eigen(K5_laplacian)
eigen(K5.3_laplacian)
eigen(K5.3e_laplacian)
eigen(B2.3_laplacian)
eigen(P5_laplacian)