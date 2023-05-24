#plot of PC1 vs PC2, used for the legend of the PCA grid.
pcaGridLegend<-function(screeCumulativeThresholdObject, x = "PC1", y = "PC2", CO, SH, AL, SZ, COtitle, SHtitle, SZtitle, ALtitle){

# Get required data for plotting
  output <- plotInput(screeCumulativeThresholdObject, CO, SH, SZ, AL)

#combinations for which legend guides should appear
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

#plot used only for its legend in ggpairs in pcaGrid function
  test <- ggplot(data = output$data$pcdf, aes(x = .data[[x]], y = .data[[y]], color = output$CO, shape = output$SH, alpha = output$AL, size = output$SZ)) +
    geom_point() +
    gu +
    labs(color = COtitle, shape = SHtitle, size = SZtitle, alpha = ALtitle ) +
    theme_minimal()
  return(test)
}
