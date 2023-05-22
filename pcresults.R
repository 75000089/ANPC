#calculate Principal Components using prcomp

# pcresults <- function(data) {
#   results <- prcomp(data, center = TRUE, scale. = TRUE)
#   return(results)
# }

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

# explained_variance_ratio <- function(results){
#   explained_variance_ratio <- summary(results)[["importance"]]['Proportion of Variance',]
#   explainedvarianceratio <- 100 * explained_variance_ratio
#   return(explainedvarianceratio)
# }
# pcsummary<-function(results){
#   pcsum<-rownames_to_column(as.data.frame(t(summary(results)[["importance"]]))) %>%
#     mutate(across(where(is.double), ~.x*100))
#   return(pcsum)
# }
# overview <- summary(prcomp(data, scale = scaleValue))
#
# ## Extract all PC scores and loadings
# scores <- as.data.frame(overview$x)
# loadings <- as.data.frame(overview$rotation)
#
# return(list(overview = overview,
#             scores = scores,
#             loadings = loadings))
