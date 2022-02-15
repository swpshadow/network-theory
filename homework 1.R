library(igraph)
library(igraphdata)

#Kendall Tau and Spearman/pearson would be good to look in

data(package="igraphdata")

data(UKfaculty)

adj = get.adjacency(karate)
adj

#degree
karate.deg <- degree(karate)
karate.deg <- karate.deg/max(karate.deg) 
karate.deg <-sort(karate.deg, decreasing = T)

#eigen centrality 
karate.eigs <- eigen_centrality(karate)$vector # resolves degree ties
karate.eigs <-sort(karate.eigs,decreasing=T)

#katz
karate.katz <- alpha_centrality(karate,alpha=.2)  
karate.katz<- sort(karate.katz,decreasing=T)


#pagerank
karate.pr <- page_rank(karate,damping = .85)$vector
karate.pr<- sort(karate.pr,decreasing=T)



#airport data

data(USairports)
USairports.adj = get.adjacency(USairports)
USairports.adj

#data details:

length(V(USairports))
length(E(USairports))

hist(degree(USairports), 100)


V(USairports)$size <- 50* degree(USairports)/max(degree(USairports))
plot(USairports, labels="None")

#degree
USairports.deg <- degree(USairports)
USairports.deg <- USairports.deg/max(USairports.deg)
USairports.deg <- sort(USairports.deg, decreasing = T)






#eigen centrality 
USairports.eigs <- eigen_centrality(USairports, directed = TRUE)$vector # resolves degree ties
USairports.eigs<-sort(USairports.eigs,decreasing=T)
#dont need to divide by max since it is already set to max being 1


#pagerank
USairports.pr <- page_rank(USairports,damping = .85)$vector
USairports.pr <- sort(USairports.pr,decreasing=T)
USairports.pr <- USairports.pr/max(USairports.pr)


#katz
USairports.katz <- alpha_centrality(USairports,alpha=0.002)  
USairports.katz <- sort(USairports.katz,decreasing=T)
USairports.katz <- USairports.katz/max(USairports.katz)




#metric analysis
centralities <- list()
centralities[[1]] <- USairports.deg
centralities[[2]] <- USairports.eigs
centralities[[3]] <- USairports.pr
centralities[[4]] <- USairports.katz

m<-rep(0)
for(x in seq(1,4)){
  m[x] <- mean(centralities[[x]])
}
m

v<-rep(0)
for(x in seq(1,4)){
  v[x] <- var(centralities[[x]])
}
v



#difference analysis
rank.difference <- function(data1, data2, keys, early.stopping=NULL){
  result = 0
  # if(is.null(early.stopping)){
  #   early.stopping <- 0.5* length(keys)
  # }
  for(k in seq(length(keys)) ){
    # if(k > early.stopping){
    #   break
    # }
    result = result + abs(mean(which(data2 == data2[keys[k]])) - mean(which(data1==data1[keys[k]])))
  }
  return(result/length(keys))
}

keys <- names(USairports.pr)

rank.difference.results <- diag(nrow=4)
rank.difference.results <- as.data.frame(rank.difference.results, row.names = c("deg", "eig", "pr", "katz"))
colnames(rank.difference.results) <- c("deg", "eig", "pr", "katz")
rank.difference.results

rank.difference.results[1,2] <- rank.difference(USairports.deg, USairports.eigs, keys)
rank.difference.results[2,1] <- rank.difference.results[1,2]

rank.difference.results[1,3]<-rank.difference(USairports.deg, USairports.pr, keys)
rank.difference.results[3,1]<-rank.difference.results[1,3]

rank.difference.results[1,4] <- rank.difference(USairports.deg, USairports.katz, keys)
rank.difference.results[4,1] <- rank.difference.results[1,4]

rank.difference.results[2,3] <- rank.difference(USairports.eigs, USairports.pr, keys)
rank.difference.results[3,2] <-rank.difference.results[2,3]

rank.difference.results[2,4] <- rank.difference(USairports.eigs, USairports.katz, keys)
rank.difference.results[4,2] <- rank.difference.results[2,4]

rank.difference.results[3,4] <- rank.difference(USairports.pr, USairports.katz, keys)
rank.difference.results[4,3] <- rank.difference.results[3,4]

rank.difference.results





