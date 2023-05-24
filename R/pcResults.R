#calculate Principal Components using prcomp

pcResults <- function(data, annotation) {

  results <- prcomp(data,
                    center = TRUE,
                    scale. = TRUE)

  explainedVarianceRatio <- 100*(summary(results)[["importance"]]['Proportion of Variance',])

  pcSum <- rownames_to_column(as.data.frame(t(summary(results)[["importance"]]))) %>%
            mutate(across(where(is.double), ~.x*100))

  scores <- results[["x"]]

  loadings <- results[["rotation"]]

  pcdf<- cbind(as.data.frame(results[["x"]]), annotation)

  return(list(results = results,
              scores = scores,
              loadings = loadings,
              explainedVarianceRatio = explainedVarianceRatio,
              pcSum = pcSum,
              pcdf = pcdf))
}
