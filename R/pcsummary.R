#Summarise the principal components calculated in a dataframe
pcsummary<-function(results){
    pcsum<-rownames_to_column(as.data.frame(t(summary(results)[["importance"]]))) %>%
            mutate(across(where(is.double), ~.x*100))
  return(pcsum)
}
