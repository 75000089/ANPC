myFn2 <- function(data, mapping){
  p <- ggplot(data = data, mapping = mapping) +
    scale_x_continuous(limits = symmetric_limits) +
    scale_y_continuous(limits = symmetric_limits) +
    theme_minimal()+
    theme(text = element_text(size = 3))+
    geom_hline(yintercept=0, linetype="dashed", color = "black")+
    geom_vline(xintercept=0, linetype="dashed", colour= "black")
  p
}
