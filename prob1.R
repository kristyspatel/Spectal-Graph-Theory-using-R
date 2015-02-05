plot(var1,col='red',pch=22,bg='red',xlim=range(0,max(alldata[,1])),ylim =range (0,max(alldata[,2])))
points(var2,col='green',pch=24,bg='green')	
points(var3,col='blue',pch=19,bg='blue')

mat1 = matrix(rep(0),0,3)
mat2 = matrix(rep(0),0,3)
points_in_community1=c()
points_in_community2=c()

for(i in 1:60)
{
	if(Re(eigen(knn_lap_norm)$vectors[,59][i]) < 0.0)
	{
		mat1=rbind(mat1,c(alldata[i,],i))
		points_in_community1 = c(points_in_community1,i)	
	}
	else
	{
		mat2=rbind(mat2,c(alldata[i,],i))
		points_in_community2 = c(points_in_community2,i)	
	}
}

total_edges_in_community1 = 0
for(i in 1:length(points_in_community1))
{
	total_edges_in_community1 = total_edges_in_community1 + sum(knn_adjacency_matrix[points_in_community1[i],])
}

total_edges_in_community2 = 0
for(i in 1:length(points_in_community2))
{
	total_edges_in_community2 = total_edges_in_community2 + sum(knn_adjacency_matrix[points_in_community2[i],])
}


total_edges_leaving_community=0
for(i in 1:length(points_in_community1))
{
	for(j in 1:60)	
	{
		if(knn_adjacency_matrix[points_in_community1[i],j]==1)
		{
			if(j %in% points_in_community2)
			{
				total_edges_leaving_community = total_edges_leaving_community+1
			}
		}
	}
}

conductance1 = total_edges_leaving_community/ (2*total_edges_in_community1 + total_edges_leaving_community)
conductance2 = total_edges_leaving_community/ (2*total_edges_in_community2 + total_edges_leaving_community)

kmeans_clust = kmeans(alldata,3)
plot(alldata, col = kmeans_clust$cluster)


first_smallest = sort(eigen(knn_lap)$values)[1]
second_smallest =  sort(eigen(knn_lap)$values)[2]
third_smallest = sort(eigen(knn_lap)$values)[3]

j=0,k=0,l=0
for(i in 1:60)
{
	if(eigen(knn_lap)$values[i]==first_smallest)
	{
		j=i
		print(j)
	}
	if(eigen(knn_lap)$values[i]==second_smallest)
	{
		k=i
		print(k)
	}
	if(eigen(knn_lap)$values[i]==third_smallest)
	{
		l=i
		print(l)
	}
}

k_matrix = cbind(Re(eigen(knn_lap)$vectors[,i]),Re(eigen(knn_lap)$vectors[,j]),Re(eigen(knn_lap)$vectors[,k]))
spect_clust = kmeans(k_matrix,3)
plot(alldata, col = spect_clust$cluster)
