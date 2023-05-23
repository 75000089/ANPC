#This function handles the shape (SH), size (SZ) and alpha (AL) input for the pcagrid.

plotinput<- function(screecumulativethresholdobject, CO, SH, SZ, AL){

  output <- screecumulativethresholdobject

  # Get required data for plotting
  #colour
  CO <- if(class(CO) == "numeric") {
    CO = CO
  }else if(class(CO) == "character"){
    CO = output$data$pcdf[,CO]
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

  #shape
  SH <- if(class(SH) == "NULL") {
    SH = SH
  }else if(class(SH) == "character"){
    SH = output$data$pcdf[,SH]
  }

  output<- append(output, list(CO=CO,
                              SH = SH,
                              SZ = SZ,
                              AL = AL))

return(output)
}
