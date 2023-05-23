#calculate Principal Components using prcomp

pcresults <- function(data, annotation) {

  results <- prcomp(data, center = TRUE, scale. = TRUE)
  explained_variance_ratio <- 100*(summary(results)[["importance"]]['Proportion of Variance',])
  pcsum <- rownames_to_column(as.data.frame(t(summary(results)[["importance"]]))) %>%
            mutate(across(where(is.double), ~.x*100))
  scores <- results[["x"]]
  loadings <- results[["rotation"]]
  pcdf<- cbind(as.data.frame(results[["x"]]), annotation)

  return(list(results = results,
              scores = scores,
              loadings = loadings,
              explained_variance_ratio = explained_variance_ratio,
              pcsum = pcsum,
              pcdf = pcdf))
}
