#single figure for screeplot, cumulative variance and threshold

screecumulativethreshold <- function(pcresultsobject, cutoff=99){

  a <- pcresultsobject
  t <-length(which(a$pcsum$`Cumulative Proportion` < cutoff))
  # a <-length(which(pcresultsobject$pcsum$`Cumulative Proportion` < cutoff))
#Make Screeplot
Screeplot <- ggplot(data = a$pcsum, aes(x = `rowname`, y = (`Proportion of Variance`))) +
              geom_point(colour = "red") +
              geom_line(group = 1, colour = "red") +
              ggtitle("Scree Plot") +
              xlab("PC") +
              ylab("Proportion of Variance (%)")

#Make cumulative variance plot
CumulativeVariance <- ggplot(data = a$pcsum, aes(x = `rowname`, y = `Cumulative Proportion`)) +
                      geom_point(colour = "blue") +
                      geom_line(group = 1, colour = "blue") +
                      ggtitle("Cumulative Variance") +
                      xlab("PC") +
                      ylab("Cumulative Variance Explained(%)")

# #Make the threshold table
library(ggpubr)

ThresholdTable <- a$pcsum %>%
                  slice(t) %>%
                  ggtexttable(theme = ttheme("mOrange"))

#Put all the information in one figure
library("cowplot")
screecumulativethresholdplot <- ggdraw() +
                                draw_plot(Screeplot, x = 0, y = .5, width = .5, height = .5) +
                                draw_plot(CumulativeVariance, x = .5, y = .5, width = .5, height = .5) +
                                draw_plot(ThresholdTable, x = 0, y = 0, width = 1, height = 0.5) +
                                draw_plot_label(label = c("A", "B", "C"), size = 15,
                                                x = c(0, 0.5, 0), y = c(1, 1, 0.5))
 # return(screecumulativethresholdplot)
a <- append(a, list(treshold = t))

return(list(data = a,
            plots = list(screeCumulativeTreshold = screecumulativethresholdplot,
                         screePlot = Screeplot,
                         cumulativeVariance = CumulativeVariance,
                         thresholdTable = ThresholdTable)))

}
