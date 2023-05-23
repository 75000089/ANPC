#plot of PC1 vs PC2, used for the legend of the PCA grid.
pcagridlegend<-function(screecumulativethresholdobject, x = "PC1", y = "PC2", CO, SH, AL, SZ, COtitle, SHtitle, SZtitle, ALtitle){
  # Get required data for plotting
  output <- plotinput(screecumulativethresholdobject, CO, SH, SZ, AL)

  gu <- if(class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
    guides(alpha = "none", size = "none")
  } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
    guides(alpha = "none", shape = "none")
  } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
    guides(shape = "none", size = "none")
  } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
    guides(alpha = "none" )
  } else if (class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
    guides(size = "none")
  } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
    guides(shape = "none")
  } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
    guides()
  } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
    guides(shape = "none", size = "none", alpha = "none")
  }

  test <- ggplot(data = output$data$pcdf, aes(x = .data[[x]], y = .data[[y]], color = output$CO, shape = output$SH, alpha = output$AL, size = output$SZ)) +
    geom_point() +
    gu +
    labs(color = COtitle, shape = SHtitle, size = SZtitle, alpha = ALtitle ) +
    theme_minimal()
  return(test)
}
# pcagridlegend<-function(pcresultsobject, x = "PC1", y = "PC2", colour, SH, AL, SZ){
#
#   a <- pcresultsobject
#
#   # Get required data for plotting
#
#   #shape
#   SH <- if(class(SH) == "NULL") {
#     SH = SH
#   }else if(class(SH) == "character"){
#     SH = a$pcdf[,SH]
#   }
#   #SH = a$pcdf$SH
#
#   #size
#   SZ <- if(class(SZ) == "numeric") {
#     SZ = SZ
#   }else if (class(SZ) == "character"){
#     SZ = a$pcdf[,SZ]
#   }
#   #SZ = a$pcdf$SZ
#
#   #alpha
#   AL <- if(class(AL) == "numeric") {
#     AL = AL
#   }else if (class(AL) == "character"){
#     AL = a$pcdf[,AL]
#   }
#   #AL = a$pcdf$AL
#
# gu <- if(class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "numeric") {
#     guides(alpha = "none", size = "none")
#   } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "numeric") {
#     guides(alpha = "none", shape = "none")
#   } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "character") {
#     guides(shape = "none", size = "none")
#   } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "numeric") {
#     guides(alpha = "none")
#   } else if (class(SH) == "character" & class(SZ) == "numeric" & class(AL) == "character") {
#     guides(size = "none")
#   } else if (class(SH) == "NULL" & class(SZ) == "character" & class(AL) == "character") {
#     guides(shape = "none")
#   } else if (class(SH) == "character" & class(SZ) == "character" & class(AL) == "character") {
#     guides()
#   } else if (class(SH) == "NULL" & class(SZ) == "numeric" & class(AL) == "numeric") {
#     guides(shape = "none", size = "none", alpha = "none")
#   }
#
# test <- ggplot(data = a$pcdf, aes(x = .data[[x]], y = .data[[y]], color = .data[[colour]], shape = SH, alpha = AL, size = SZ)) +
#         geom_point() +
#         gu +
#         theme_minimal()
# return(test)
# }
# pcagridlegend<-function(data = pcdf, x = "PC1", y = "PC2", colour, SH, AL, SZ){
#
#     test<-ggplot(data = pcdf, aes(x = .data[[x]], y = .data[[y]], color = .data[[colour]], shape = SH, alpha = AL, size = SZ)) +
#         geom_point() +
#         gu +
#         theme_minimal()
#      }
#
# #vimal
# plotLoadingsPCA <- function(pca, pcx = 1, pcy = 2, subtitle = NULL) {
#   ## Extract required data for plotting
#   xValue <- pca$loadings[pcx]
#   yValue <- pca$loadings[pcy]
#   dataFrame <- bind_cols(xValue, yValue)
#   colnames(dataFrame) <- c("pcx", "pcy")
#
#   ## Generate PCA loadings plot
#   pcaLoadingsPlot <- ggplot(dataFrame, aes(x = pcx, y = pcy)) +
#     geom_point(aes(color = row.names(pca$loadings))) +
#     theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#     labs(title = "PCA Loadings Plot",
#          subtitle = subtitle,
#          color = "Variables",
#          x = paste(colnames(xValue), paste0("[",round(pca$overview$importance[2, pcx]*100, 2), "%]")),
#          y = paste(colnames(yValue), paste0("[",round(pca$overview$importance[2, pcy]*100, 2), "%]")))
#
#   return(pcaLoadingsPlot)
# }
#
#












# test<-function(x, y, colour, shape, alpha, size){
#   if(class(colour)=='character' & class(shape)=='NULL' & class(alpha)=='numeric' & class(size)=='numeric'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=shape, alpha=alpha, size=size)) +
#       geom_point() +
#       guides(alpha="none", size="none")+
#       theme_minimal()
#   }else if(class(colour)=='character' & class(shape)=='character' & class(alpha)=='numeric' & class(size)=='numeric'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=.data[[shape]], alpha=alpha, size=size)) +
#       geom_point() +
#       guides(alpha="none", size="none")+
#       theme_minimal()
#   }else if (class(colour)=='character' & class(shape)=='character' & class(alpha)=='character' & class(size)=='numeric'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=.data[[shape]], alpha=.data[[alpha]], size=size)) +
#       geom_point() +
#       guides(size="none")+
#       theme_minimal()
#   }else if (class(colour)=='character' & class(shape)=='character' & class(alpha)=='character' & class(size)=='character'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=.data[[shape]], alpha=.data[[alpha]], size=.data[[size]])) +
#       geom_point() +
#       guides()+
#       theme_minimal()
#   }else if (class(colour)=='character' & class(shape)=='NULL' & class(alpha)=='character' & class(size)=='numeric'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=shape, alpha=.data[[alpha]], size=size)) +
#       geom_point() +
#       guides(size="none")+
#       theme_minimal()
#   }else if (class(colour)=='character' & class(shape)=='NULL' & class(alpha)=='character' & class(size)=='character'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=shape, alpha=.data[[alpha]], size=.data[[size]])) +
#       geom_point() +
#       guides()+
#       theme_minimal()
#   }else if (class(colour)=='character' & class(shape)=='NULL' & class(alpha)=='numeric' & class(size)=='character'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=shape, alpha=alpha, size=.data[[size]])) +
#       geom_point() +
#       guides(alpha="none")+
#       theme_minimal()
#   }else if (class(colour)=='character' & class(shape)=='character' & class(alpha)=='numeric' & class(size)=='character'){
#     test<-ggplot(df, aes(x = .data[[x]], y = .data[[y]], color=.data[[colour]], shape=.data[[shape]], alpha=alpha, size=.data[[size]])) +
#       geom_point() +
#       guides(alpha="none")+
#       theme_minimal()
#   }
#   test
# }
# test<-test(x="PC1", y="PC2",colour = "covid_status", shape=NULL, alpha=.4, size=1)
# test(x="PC1", y="PC2",colour = "covid_status", shape="sex", alpha=1, size="bmi")
