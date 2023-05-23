#Make PCA grid with ggpairs using the threshold, explainedvariance ratio,

pcagrid <-function(screecumulativethresholdobject, CO, SH = NULL, SZ = 0.2, AL = 1, COtitle, SHtitle = "NULL", SZtitle = "NULL", ALtitle = "NULL"){

  output <- plotinput(screecumulativethresholdobject, CO, SH, SZ, AL)
  thresh <- output$data$treshold
  test <- pcagridlegend(screecumulativethresholdobject, x = "PC1", y = "PC2", CO, SH, AL, SZ, COtitle, SHtitle, SZtitle, ALtitle)
  title <- list()
  for (i in 1:thresh) {
    title[[i]] <- paste0('PC', i, ' (', round(output$data$explained_variance_ratio[i], 1), '%)')
  }
  title<-unlist(title)

  pcagridplot<-GGally::ggpairs(data = output$data$pcdf[,1:thresh],
                               columnLabels = c(title),
                               diag="blank",
                               upper="blank",
                               #upper=list(continuous = my_fn1),
                               lower=list(continuous = my_fn1),
                               legend = grab_legend(test),
                               progress = F,
                               switch = "both") +
                               geom_point(aes(colour = output$CO, shape = output$SH, size = output$SZ, alpha = output$AL)) +
                               stat_ellipse(aes(group=interaction(output$CO, color=output$CO), color=output$CO))+
                               theme_bw() +
                               theme(strip.background = element_rect(fill = "white"),
                               axis.text.x = (element_text(size=rel(0.7), angle=0)),
                               axis.text.y = (element_text(size=rel(0.7), angle=0)),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(fill = NA,colour = "grey35"))

  final_plot <- gpairs_lower(pcagridplot)
  return(final_plot)
}
