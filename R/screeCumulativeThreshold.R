#threshold and screeplot, cumulative variance and threshold figures

screeCumulativeThreshold <- function(pcResultsObject, cutoff = 99){

  a <- pcResultsObject
  t <- length(which(a$pcSum$`Cumulative Proportion` < cutoff))

#Make cumulative variance plot

cumulativeVariance <- ggplot(data = a$pcSum, aes(x = `rowname`, y = `Cumulative Proportion`)) +
    geom_point(colour = "blue") +
    geom_line(group = 1, colour = "blue") +
    ggtitle("Cumulative Variance") +
    xlab("PC") +
    ylab("Cumulative Variance Explained (%)")

#Make Screeplot

screeplot <- ggplot(data = a$pcSum, aes(x = `rowname`, y = (`Proportion of Variance`))) +
              geom_point(colour = "red") +
              geom_line(group = 1, colour = "red") +
              ggtitle("Scree Plot") +
              xlab("PC") +
              ylab("Proportion of Variance (%)")

combinedScreeCumulative <- ggplot(a$pcSum)  +
  geom_bar(aes(x = `rowname`, y = `Proportion of Variance`), stat = "identity", fill = "grey20", color = "black", alpha = 0.4) +
  geom_line(aes(x = `rowname`, y = `Cumulative Proportion`), stat = "identity", color = "orange2", size = 2, group = 1) +
  labs(title = "Screeplot and Cumulative Variance",
       x = "PC", y = "Cumulative Variance (%)") +
  scale_y_continuous(sec.axis = sec_axis(~.*0.5, name = "Proportion of Variance (%)")) +
  theme(axis.title.y = element_text(color = "gray30"), axis.title.y.right = element_text(color = "orange3"))


# #Make the threshold table

thresholdTable <- a$pcSum[t,]%>%
                  ggtexttable(theme = ttheme("mOrange"))

#Put all the information in one figure

screeCumulativeThresholdPlot <- ggdraw() +
                                draw_plot(cumulativeVariance,x = 0, y = .5, width = .5, height = .5) +
                                draw_plot(screeplot, x = .5, y = .5, width = .5, height = .5) +
                                draw_plot(thresholdTable, x = 0, y = 0, width = 1, height = 0.5) +
                                draw_plot_label(label = c("A", "B", "C"), size = 15, x = c(1, 1, 0.5), y = c(0, 0.5, 0))

# return list with plots and threshold included

a <- append(a, list(threshold = t))

return(list(data = a,
            plots = list(screeCumulativeThreshold = screeCumulativeThresholdPlot,
                         screeplot = screeplot,
                         cumulativeVariance = cumulativeVariance,
                         thresholdTable = thresholdTable)))

}
