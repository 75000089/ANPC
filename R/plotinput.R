#This function handles the shape (SH), size (SZ) and alpha (AL) input for the pcagrid.

plotinput<- function(screecumulativethresholdobject, SH, SZ, AL){

  output <- screecumulativethresholdobject

  # Get required data for plotting

  #shape
  SH <- if(class(SH) == "NULL") {
    SH = SH
  }else if(class(SH) == "character"){
    SH = output$data$pcdf[,SH]
  }

  #size
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

  output<- append(output, list(SH = SH,
                          SZ = SZ,
                          AL = AL))
  #output<-list.append(output, forplots)
  #output<-append(output, forplots)
return(output)
}
