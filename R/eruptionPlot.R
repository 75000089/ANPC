
eruptionPlot <- function(data, title){
                        df <- data

                        eruptionPlot<- ggplot(data = df, aes(x = df$LogFC, y = df$p_adj)) +
                                      geom_point() +
                                      theme_minimal() +
                                      geom_hline(yintercept = -log10(0.05), col = "red") +
                                      geom_vline(xintercept = c(-0.6, 0.6), col = "red") +
                                      geom_label_repel(aes(label = liponame), size = 2) +
                                      labs(title = "title", x = "log2FoldChange", y = "-log10 adj.p-val")

                        return(eruptionPlot)
                        }

#From metabom8:

  function (model, pc = 1, p_adj = "BH", invert_es = FALSE)
{
  if (is.na(p_adj) || is.infinite(p_adj) || length(p_adj) >
      1) {
    p_adj <- "none"
  }
  if (length(unique(model@Y$ori)) > 2) {
    stop("Eruption plot defined for two-level outcome.")
  }
  if (is.na(pc) || is.infinite(pc) || length(pc) > 1)
    stop("Check pc argument.")
  if (grepl("o", pc)) {
    pc1 <- as.numeric(gsub("o", "", pc))
    if (is.na(pc1) || is.infinite(pc1) || pc1 > nrow(model@p_orth)) {
      stop("Check pc argument and help section.")
    }
    ddl <- data.frame(x = model@p_orth[pc1, ], id = colnames(model@X))
  }
  else {
    ddl <- data.frame(x = model@p_pred[1, ], id = colnames(model@X))
  }
  Y <- model@Y$ori
  if (invert_es) {
    comp <- unique(Y)[2]
    cat("Using ", unique(Y)[1], " as reference group for Cliff's delta. Swap reference / comparator with invert_es=TRUE (see function help).")
  }
  else {
    comp <- unique(Y)[1]
    cat("Using ", unique(Y)[2], " as reference group for Cliff's delta.")
  }

#scores? and cliffs delta
  uni <- t(apply(model@X, 2, function(x, idx = which(Y == comp),
                                    y = Y) {
    c(es_cdelta(x[idx], x[-idx]), kruskal.test(x, y)$p.value)
  }))

#data frame
  ddl <- cbind(ddl, uni)
  colnames(ddl) <- c("p1", "id", "Cd", "pval")
  ddl$p1 <- abs(ddl$p1)

  adj_log <- p_adj %in% c("holm", "hochberg", "hommel", "bonferroni",
                          "BH", "BY", "fdr")
  if (adj_log) {
    ddl$pval_adjusted <- p.adjust(ddl$pval, method = p_adj)
    ddl$pval_transformed <- abs(log10(ddl$pval_adjusted))
  }
  else {
    ddl$pval_transformed <- abs(log10(ddl$pval))
  }

#eruption plot
  gl2 <- ggplot(ddl, aes_string(x = "Cd", y = "p1", colour = "pval_transformed")) +
    geom_label_repel(aes_string(label = "id"), colour = "black", min.segment.length = 0.001) +
    geom_point(size = 3, shape = 1) +
    geom_point(size = 3, shape = 16, alpha = 0.3) +
    theme_bw() +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_colour_gradientn(colours = matlab.like2(10)) +
    labs(x = "Cliff's Delta", y = paste0("|p_", pc, "|")) +
    theme(panel.grid.minor = element_blank(), plot.tag = element_text(face = "bold", size = 25), legend.position = "top", legend.direction = "horizontal")

 if (adj_log) {
    gl2 <- gl2 + labs(colour = expression("| log10( p value"["adj"] ~
                                            ") |"))
  }
  else {
    gl2 <- gl2 + labs(colour = "| log10( p value ) |")
  }

  ddl = ddl[, !colnames(ddl) %in% "pval_transformed"]

  return(list(data = ddl, plot = gl2))
}
