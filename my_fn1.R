#function for the lower half of the PCA grid, the plotscores with ellipses set to the same parameter colour is set to.

  my_fn1 <- function(output, data, mapping, method="stat_ellipse"){
    p <- ggplot(data = output$data$pcdf, mapping=mapping) +
    geom_point(aes(color = .data[[colour]], shape = output$SH, size=output$SZ, alpha=output$AL)) +
    stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
    theme_minimal()+
    geom_hline(yintercept=0, linetype="dashed", color = "black")+
    geom_vline(xintercept=0, linetype="dashed", colour= "black")
  p
}

#my_fn1 <- function(pcresultsobject, colour, SH, SZ, AL, mapping, method="stat_ellipse"){
  # a <- pcresultsobject
  #
  # # Get required data for plotting
  #
  # #shape
  # SH <- if(class(SH) == "NULL") {
  #   SH = SH
  # }else if(class(SH) == "character"){
  #   SH = a$pcdf[,SH]
  # }
  # #SH = a$pcdf$SH
  #
  # #size
  # SZ <- if(class(SZ) == "numeric") {
  #   SZ = SZ
  # }else if (class(SZ) == "character"){
  #   SZ = a$pcdf[,SZ]
  # }
  # #SZ = a$pcdf$SZ
  #
  # #alpha
  # AL <- if(class(AL) == "numeric") {
  #   AL = AL
  # }else if (class(AL) == "character"){
  #   AL = a$pcdf[,AL]
  # }
  # #AL = a$pcdf$AL
  #
  #
  #  myfun <- ggplot(data = a$pcdf, mapping = mapping) +
  #     geom_point(aes(color = .data[[colour]], shape = SH, size = SZ, alpha = AL)) +
  #     stat_ellipse(aes(group = interaction(.data[[colour]], color = .data[[colour]]), color = .data[[colour]]))+
  #     theme_minimal()+
  #     geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  #     geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  #
  #  myfun
#}







#JW
# results<-prcomp(BIOspcglyc[,1:8], center = TRUE, scale. = TRUE)
# data<-cbind(as.data.frame(results[["x"]]), BIOspcglyc[,-1:-8])
# explained_variance_ratio <- summary(results)[["importance"]]['Proportion of Variance',]
# explained_variance_ratio <- 100 * explained_variance_ratio
#
# my_fun1<-function(data, mapping){
#   nameColour <- mapping$colour
#   nameShape <- mapping$shape
#   nameSize <- mapping$size
#   nameAlpha<- mapping$alpha
#
#   colour <- unlist(data[nameColour])
#   if(!is.numeric(colour)) {
#     colour <- as.numeric(factor(colour))
#   }
#   shape <- unlist(data[nameShape])
#   if(!is.numeric(shape)) {
#     shape <- as.numeric(factor(shape))
#   }
#   size <- unlist(data[nameSize])
#   if(!is.numeric(size)) {
#     size <- as.numeric(factor(size))
#   }
#   alpha <- unlist(data[nameAlpha])
#   if(!is.numeric(alpha)) {
#     alpha <- as.numeric(factor(alpha))
#   }
#
#   df <- data.frame(x = unname(data[nameX]),
#                    y = unname(data[nameY]),
#                    shape = unname(shape),
#                    colour=unname(colour),
#                    size=unname(size),
#                    alpha=unname(alpha))
#
#   p <- ggplot(data = df,
#               aes(x, y)) +
#     geom_point(color = colour, shape = shape, size=size, alpha=alpha)+
#     stat_ellipse(aes(group=interaction(colour, color=colour), color=colour))+
#     theme_minimal()+
#     geom_hline(yintercept=0, linetype="dashed", color = "black")+
#     geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   print(p)
# }
#
# my_fun1(data, mapping = list(x = "PC1", y = "PC2", colour="covid_status", shape="sex", size="bmi", alpha="age"))
#
# my_fn1 <- function(data, mapping, method="stat_ellipse", ...){
#   p <- ggplot(data = df, mapping = mapping) +
#     geom_point(aes(color = df$covid_status, shape = df$sex), size = 2) +
#     stat_ellipse(aes(group=interaction(df$covid_status, color=df$covid_status), color=df$covid_status))+
#     theme_minimal()+
#     geom_hline(yintercept=0, linetype="dashed", color = "black")+
#     geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   p
# }

#LG 1
# shape<-if(class(shape)=="NULL") {
#   shape=shape
# }if(class(shape)=="character"){
#   shape=.data[[shape]]
# return(shape)
#   }
#
# SZ<-if(class(SZ)=="numeric") {
#   size=SZ
# }if(class(SZ)=="character"){
#   size=.data[[SZ]]
# return(size)
#   }
#
# AL<-if(class(AL)=="numeric") {
#   alpha=AL
# }if(class(AL)=="character"){
#   alpha=.data[[AL]]
# return(alpha)
#   }











# my_fn1(data=df, colour="covid_status", SH=NULL, SZ=1, AL=0.4)
# #LG 2
#
# g_p<-function(data, colour, shape, size, alpha){
#   if(class(shape)== 'NULL' & class(size)=='numeric' & class(alpha)=='numeric'){
#     gp<-geom_point(aes(color = .data[[colour]], shape = shape, size=size, alpha=alpha))
#   }else if(class(shape)== 'character'& class(size)=='numeric' & class(alpha)=='numeric'){
#     gp<-geom_point(aes(color = .data[[colour]], shape = .data[[shape]], size=size, alpha=alpha))
#   }else if(class(shape)== 'character'& class(size)=='character' & class(alpha)=='numeric'){
#     gp<-geom_point(aes(color = .data[[colour]],shape = .data[[shape]],size=.data[[size]],alpha=alpha))
#   }else if(class(shape)== 'character'& class(size)=='character' & class(alpha)=='character'){
#     gp<-geom_point(aes(color = .data[[colour]], shape = .data[[shape]], size=.data[[size]], alpha=.data[[alpha]]))
#   }else if(class(shape)== 'NULL'& class(size)=='character' & class(alpha)=='numeric'){
#     geom_point(aes(color = .data[[colour]], shape = shape, size=.data[[size]], alpha=alpha))
#   }else if(class(shape)== 'NULL'& class(size)=='character' & class(alpha)=='character'){
#     geom_point(aes(color = .data[[colour]], shape = shape, size=.data[[size]], alpha=.data[[alpha]]))
#   }else if(class(shape)== 'NULL'& class(size)=='numeric' & class(alpha)=='character'){
#     geom_point(aes(color = .data[[colour]], shape = shape, size=size, alpha=.data[[alpha]]))
#   }else if(class(shape)== 'character'& class(size)=='numeric' & class(alpha)=='character'){
#     geom_point(aes(color = .data[[colour]], shape = shape, size=size, alpha=.data[[alpha]]))
#   }
#   return(g_p)}
#
# g_p(data=df, colour="covid_status", shape="NULL", size=1, alpha="age")
#
# my_fn1<-function(data, colour, shape, size, alpha, mapping, method="stat_ellipse", ...){
#   p <- ggplot(data = df, mapping = mapping) +
#     g_p +
#     stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#     theme_minimal()+
#     geom_hline(yintercept=0, linetype="dashed", color = "black")+
#     geom_vline(xintercept=0, linetype="dashed", colour= "black")
# }
#
# #LG 3
# my_fn1 <- function(data, colour, shape, size, alpha, mapping, method="stat_ellipse", ...){
#   if(class(colour)=='character' & class(shape)== 'NULL' & class(size)=='numeric' & class(alpha)=='numeric'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = shape, size=size, alpha=alpha)) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'character'& class(size)=='numeric' & class(alpha)=='numeric'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = .data[[shape]], size=size, alpha=alpha)) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'character'& class(size)=='character' & class(alpha)=='numeric'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]],shape = .data[[shape]],size=.data[[size]],alpha=alpha)) +
#       stat_ellipse(aes(group=interaction(.data[[colour]],color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal() +
#       geom_hline(yintercept = 0,linetype="dashed",color = "black")+
#       geom_vline(xintercept = 0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'character'& class(size)=='character' & class(alpha)=='character'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = .data[[shape]], size=.data[[size]], alpha=.data[[alpha]])) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'NULL'& class(size)=='character' & class(alpha)=='numeric'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = shape, size=.data[[size]], alpha=alpha)) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'NULL'& class(size)=='character' & class(alpha)=='character'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = shape, size=.data[[size]], alpha=.data[[alpha]])) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'NULL'& class(size)=='numeric' & class(alpha)=='character'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = shape, size=size, alpha=.data[[alpha]])) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }else if(class(colour)=='character' & class(shape)== 'character'& class(size)=='numeric' & class(alpha)=='character'){
#     p <- ggplot(data = df, mapping = mapping) +
#       geom_point(aes(color = .data[[colour]], shape = shape, size=size, alpha=.data[[alpha]])) +
#       stat_ellipse(aes(group=interaction(.data[[colour]], color=.data[[colour]]), color=.data[[colour]]))+
#       theme_minimal()+
#       geom_hline(yintercept=0, linetype="dashed", color = "black")+
#       geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   }
#   p
# }
# my_fn1(data=df, colour="covid_status", mapping=mapping, shape="NULL", size=1, alpha=0.4)
#
# my_fn1 <- function(data, mapping, method="stat_ellipse", ...){
#   p <- ggplot(data = df, mapping = mapping) +
#     geom_point(aes(color = df$covid_status, shape = df$sex, size=df$age, alpha=0.4)) +
#     stat_ellipse(aes(group=interaction(df$covid_status, color=df$covid_status), color=df$covid_status))+
#     theme_minimal()+
#     geom_hline(yintercept=0, linetype="dashed", color = "black")+
#     geom_vline(xintercept=0, linetype="dashed", colour= "black")
#   p
# }
