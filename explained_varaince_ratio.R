#calculate the explained variance of each principal component

explained_variance_ratio <- function(results){
  explained_variance_ratio <- summary(results)[["importance"]]['Proportion of Variance',]
  explainedvarianceratio <- 100 * explained_variance_ratio
  return(explainedvarianceratio)
}
