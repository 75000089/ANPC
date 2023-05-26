#Make PCA grid with ggpairs
plotLoadingGrid <- function(screeCumulativeThresholdObject){

output <- screeCumulativeThresholdObject
df <- as.data.frame(output$data$loadings)
thresh <- output$data$threshold

# symmetric_limits <- function (x)
# {
#   max <- max(abs(x))
#   c(-max, max)
# }

title <- list()
for (i in 1:thresh) {
  title[[i]] <- paste0('PC', i, ' (', round(output$data$explainedVarianceRatio[i], 1), '%)')
}
title<-unlist(title)

              plotLoadingGrid <- GGally::ggpairs(df[,1:thresh],
                                        columnLabels = c(title),
                                        diag="blank",
                                        upper="blank",
                                        #upper=list(continuous =my_fn1),
                                        lower=list(continuous =myFn2),
                                        #legend = TRUE, progress = F, switch="both") +
                                        #legend = grab_legend(test),
                                        progress = F, switch="both") +
                                        geom_point(color= "red", size = 0.5) +
                                        geom_text(aes(label = rownames(df)), size = 2, colour = "red", hjust=0, vjust=0)+
                                        theme_bw() +
                                        theme(strip.background = element_rect(fill = "white"),axis.text.x=(element_text(size=rel(0.7), angle=0)),
                                              axis.text.y=(element_text(size=rel(0.7), angle=0)), panel.grid.major = element_blank(),
                                              panel.grid.minor = element_blank(), panel.border = element_rect(fill = NA,colour = "grey35"))

#remove empty grid spaces (lower and diagonal)

final_plot <- gPairsLower(plotLoadingGrid)
return(final_plot)
}
