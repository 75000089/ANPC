#Make PCA grid with ggpairs using the threshold, explainedvariance ratio,

pcagrid <-function(screecumulativethresholdobject, colour, SH, SZ, AL){

  output <- plotinput(screecumulativethresholdobject, SH, SZ, AL)
  thresh <- output$data$treshold
  myfn1 <- my_fn1(output, data, mapping, method="stat_ellipse")
  #   my_fn1 <- function(data, mapping, method="stat_ellipse"){
  #   p <- ggplot(data = output$data$pcdf, mapping=mapping) +
  #     geom_point(aes(color = .data[[colour]], shape = output$SH, size=output$SZ, alpha=output$AL)) +
  #     stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
  #     theme_minimal()+
  #     geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #     geom_vline(xintercept=0, linetype="dashed", colour= "black")
  #   p
  # }

  title <- list()
  for (i in 1:thresh) {
    title[[i]] <- paste0('PC', i, ' (', round(output$data$explained_variance_ratio[i], 1), '%)')
  }
  title<-unlist(title)

  pcagridplot<-GGally::ggpairs(output$data$pcdf[,1:thresh],
                               columnLabels = c(title),
                               diag="blank",
                               upper="blank",
                               #upper=list(continuous = my_fn1),
                               lower=list(continuous = myfn1),
                               #legend = grab_legend(test),
                               progress = F,
                               switch = "both") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"),
          axis.text.x = (element_text(size=rel(0.7), angle=0)),
          axis.text.y = (element_text(size=rel(0.7), angle=0)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA,colour = "grey35"))
  pcagridplot

}
# pcagrid <-function(screecumulativethresholdobject, colour, SH, SZ, AL){
#
#   output <- screecumulativethresholdobject
#   #t <- 4
#   thresh <- output$data$treshold
#   #as.numeric(output$data$treshold)
#
#   # Get required data for plotting
#
#   #shape
#   SH <- if(class(SH) == "NULL") {
#     SH = SH
#   }else if(class(SH) == "character"){
#     SH = output$data$pcdf[,SH]
#   }
#   #SH = output$pcdf$SH
#
#   #size
#   SZ <- if(class(SZ) == "numeric") {
#     SZ = SZ
#   }else if (class(SZ) == "character"){
#     SZ = output$data$pcdf[,SZ]
#   }
#   #SZ = output$pcdf$SZ
#
#   #alpha
#   AL <- if(class(AL) == "numeric") {
#     AL = AL
#   }else if (class(AL) == "character"){
#     AL = output$data$pcdf[,AL]
#   }
#   #AL = output$pcdf$AL
#
#
#   my_fn1 <- function(data, mapping, method="stat_ellipse"){
#     p <- ggplot(data = output$data$pcdf, mapping=mapping) +
#       geom_point(aes(color = .data[[colour]], shape = SH, size=SZ, alpha=AL)) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#     p
#   }
#
#   title <- list()
#   for (i in 1:thresh) {
#     title[[i]] <- paste0('PC', i, ' (', round(output$data$explained_variance_ratio[i], 1), '%)')
#   }
#   title<-unlist(title)
#
#   pcagridplot<-GGally::ggpairs(output$data$pcdf[,1:thresh],
#                                columnLabels = c(title),
#                                diag="blank",
#                                upper="blank",
#                                #upper=list(continuous = my_fn1),
#                                lower=list(continuous = my_fn1),
#                                #legend = grab_legend(test),
#                                progress = F,
#                                switch = "both") +
#     theme_bw() +
#     theme(strip.background = element_rect(fill = "white"),
#           axis.text.x = (element_text(size=rel(0.7), angle=0)),
#           axis.text.y = (element_text(size=rel(0.7), angle=0)),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_rect(fill = NA,colour = "grey35"))
#   pcagridplot
#
# }
# pcagrid <-function(pcresultobject, colour, SH, SZ, AL){
#
#   output <- pcresultobject
#   #t <- 4
#   thresh <- 4
#   #as.numeric(output$data$treshold)
#
#   # Get required data for plotting
#
#   #shape
#   SH <- if(class(SH) == "NULL") {
#     SH = SH
#   }else if(class(SH) == "character"){
#     SH = output$pcdf[,SH]
#   }
#   #SH = output$pcdf$SH
#
#   #size
#   SZ <- if(class(SZ) == "numeric") {
#     SZ = SZ
#   }else if (class(SZ) == "character"){
#     SZ = output$pcdf[,SZ]
#   }
#   #SZ = output$pcdf$SZ
#
#   #alpha
#   AL <- if(class(AL) == "numeric") {
#     AL = AL
#   }else if (class(AL) == "character"){
#     AL = output$pcdf[,AL]
#   }
#   #AL = output$pcdf$AL
#
#
#   my_fn1 <- function(data, mapping, method="stat_ellipse"){
#     p <- ggplot(data = df, mapping=mapping) +
#       geom_point(aes(color = .data[[colour]], shape = SH, size=SZ, alpha=AL)) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#     p
#   }
#
#   title <- list()
#   for (i in 1:thresh) {
#     title[[i]] <- paste0('PC', i, ' (', round(output$explained_variance_ratio[i], 1), '%)')
#   }
#   title<-unlist(title)
#
#   pcagridplot<-GGally::ggpairs(output$pcdf[,1:thresh],
#                                columnLabels = c(title),
#                                diag="blank",
#                                upper="blank",
#                                #upper=list(continuous = my_fn1),
#                                lower=list(continuous = my_fn1),
#                                #legend = grab_legend(test),
#                                progress = F,
#                                switch = "both") +
#     theme_bw() +
#     theme(strip.background = element_rect(fill = "white"),
#           axis.text.x = (element_text(size=rel(0.7), angle=0)),
#           axis.text.y = (element_text(size=rel(0.7), angle=0)),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_rect(fill = NA,colour = "grey35"))
#   pcagridplot
#
# }

#pcagrid <-function(screecumulativethresholdobject, colour, SH, SZ, AL){
#
#   output <- screecumulativethresholdobject
#   #t <- 4
#   thresh <- as.integer(output$data$treshold)
#   df <- output$data$pcdf
#   # Get required data for plotting
#
#   #shape
#   SH <- if(class(SH) == "NULL") {
#     SH = SH
#   }else if(class(SH) == "character"){
#     SH = df[,SH]
#   }
#   #SH = output$pcdf$SH
#
#   #size
#   SZ <- if(class(SZ) == "numeric") {
#     SZ = SZ
#   }else if (class(SZ) == "character"){
#     SZ = df[,SZ]
#   }
#   #SZ = output$pcdf$SZ
#
#   #alpha
#   AL <- if(class(AL) == "numeric") {
#     AL = AL
#   }else if (class(AL) == "character"){
#     AL = df[,AL]
#   }
#   #AL = output$pcdf$AL
#
#
#   myfun <- ggplot(data = df) +
#   #myfun <- ggplot(data = output$pcdf, mapping = mapping) +
#     geom_point(aes(color = .data[[colour]], shape = SH, size = SZ, alpha = AL)) +
#     stat_ellipse(aes(group = interaction(.data[[colour]], color = .data[[colour]]), color = .data[[colour]]))+
#     theme_minimal()+
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
#     geom_vline(xintercept = 0, linetype = "dashed", color = "black")
#
#   title <- list()
#   for (i in 1:thresh) {
#     title[[i]] <- paste0('PC', i, ' (', round(output$data$explained_variance_ratio[i], 1), '%)')
#   }
#   title<-unlist(title)
#
#   pcagridplot<-GGally::ggpairs(df[,1:thresh],
#                          columnLabels = c(title),
#                          diag="blank",
#                          upper="blank",
#                          #upper=list(continuous = my_fn1),
#                          lower=list(continuous = myfun),
#                          #legend = grab_legend(test),
#                          progress = F,
#                          switch = "both") +
#                          theme_bw() +
#                          theme(strip.background = element_rect(fill = "white"),
#                                 axis.text.x = (element_text(size=rel(0.7), angle=0)),
#                                 axis.text.y = (element_text(size=rel(0.7), angle=0)),
#                                 panel.grid.major = element_blank(),
#                                 panel.grid.minor = element_blank(),
#                                 panel.border = element_rect(fill = NA,colour = "grey35"))
#   pcagridplot
#}
