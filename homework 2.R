library(igraph)
library(igraphdata)


data(karate)


eig <- eigen_centrality(karate)$vector # resolves ties
sort(eig,decreasing=T)


#igraph subgraph method
sub_cent <- subgraph.centrality(karate)
avg_sub_cent <- sum(sub_cent)/length(karate)

#betweeness
bet <- betweenness(karate)



#katz
katz.cent <- function(A,alpha=NULL,beta=NULL){
  if (class(A)=="igraph"){
    A <- get.adjacency(A) # convert to matrix if needed
  }
  lam.dom <- eigen(A)$values[1]
  if (is.null(alpha)){
    alpha <- .8/lam.dom # 80% of max threshold
  }
  n <- nrow(A)
  if (is.null(beta)){
    beta <- matrix(rep(1/n,n),nrow=n)
  }
  eye <- diag(n) # identity
  scores <- solve(eye-alpha*A,beta)
  att <- rownames(scores)
  scores.maxscale <- scores/max(scores)
  katz.df <- data.frame(att=att,scores=scores,
                        scores.maxscale=scores.maxscale,
                        row.names = NULL)
  return(katz.df)
}

cat <- katz.cent(get.adjacency(karate))

#results
eig
sub_cent
bet
cat