#make the dataframe of the principal component scores and metadata. This will be used in the plots. The pca

pcdf<- function(results, annotation){
  pcdf<-cbind(as.data.frame(results[["x"]]), annotation)
  return(pcdf)
}
