#Calculate a threshold for the number of principal components that explain, as a default, 99% of the variance.
threshold<-function(data, cutoff = 99){

 a<-pcresults(data)

  t <-length(which(a$pcsum$`Cumulative Proportion` < cutoff))
  return(append(a, list(threshold=t)))
}
