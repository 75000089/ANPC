#This function handles the shape (SH), size (SZ) and alpha (AL) input for the pcagrid.
plotinput<- function(screecumulativethresholdobject, CO, SH, SZ, AL){

  output <- screecumulativethresholdobject

  #colour
  CO <- if(class(CO) == "numeric") {
    output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
    CO = output$data$pcdf$CO
  }else if(class(CO) == "character"){
    CO = output$data$pcdf[,CO]
  }

  # if(class(CO) == "numeric"){
  #   output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
  # }
  # CO = output$data$pcdf[,CO]

    SZ <- if(class(SZ) == "numeric") {
      SZ = SZ
    }else if (class(SZ) == "character"){
      SZ = output$data$pcdf[,SZ]
    }

  #alpha
  AL <- if(class(AL) == "numeric") {
    AL = AL
  }else if (class(AL) == "character"){
    AL = output$data$pcdf[,AL]
  }

  #shape
  SH <- if(class(SH) == "NULL") {
     output$data$pcdf$SH = rep_len("circle", nrow(output$data$pcdf))
     SH = output$data$pcdf$SH
  }else if(class(SH) == "character"){
    SH = output$data$pcdf[,SH]
  }

  output<- append(output, list(CO=CO,
                               SH = SH,
                               SZ = SZ,
                               AL = AL))

  return(output)
}
# plotinput<- function(screecumulativethresholdobject, CO, SH, SZ, AL){
#
#   output <- screecumulativethresholdobject
#
#   # Get required data for plotting
#   #colour
#   CO <- if(class(CO) == "numeric") {
#     output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
#     CO = output$data$pcdf[,CO]
#   }else if(class(CO) == "character"){
#     CO = output$data$pcdf[,CO]
#   }
#
#   # if(class(CO) == "numeric"){
#   #   output$data$pcdf$CO = rep_len(CO, nrow(output$data$pcdf))
#   # }
#   # CO = output$data$pcdf[,CO]
#
#   #size
#   SZ <- if(class(SZ) == "numeric") {
#     output$data$pcdf$SZ = rep_len(SZ, nrow(output$data$pcdf))
#     SZ = output$data$pcdf[,SZ]
#   }else if (class(SZ) == "character"){
#     SZ = output$data$pcdf[,SZ]
#   }
#
#   #alpha
#   AL <- if(class(AL) == "numeric") {
#     output$data$pcdf$AL = rep_len(AL, nrow(output$data$pcdf))
#     AL = output$data$pcdf[,AL]
#   }else if (class(AL) == "character"){
#     AL = output$data$pcdf[,AL]
#   }
#
#   #shape
#   SH <- if(class(SH) == "NULL") {
#     #output$data$pcdf$SH = rep_len(1, nrow(output$data$pcdf))
#     SH = 1
#   }else if(class(SH) == "character"){
#     SH = output$data$pcdf[,SH]
#   }
#
#   output<- append(output, list(CO=CO,
#                               SH = SH,
#                               SZ = SZ,
#                               AL = AL))
#
# return(output)
# }
