#function for the lower half of the PCA grid, the plotscores with ellipses set to the same parameter colour is set to.

  myFn1 <- function(data, mapping, method="stat_ellipse"){
    p <- ggplot(data = data, mapping = mapping) +
    theme_minimal()+
    geom_hline(yintercept=0, linetype="dashed", color = "black")+
    geom_vline(xintercept=0, linetype="dashed", colour= "black")
  return(p)
}
