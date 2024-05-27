
## Difference plot function for bias calculation using
diff_plot <- function(df = df, group1 = group1, group2 = group2, n_bins = 15, n_breaks = 15) {
  
  data <- df %>%
    rename(group1 = !!sym(group1), group2 = !!sym(group2)) %>%
    mutate(diff = group2 - group1, mean = (group1 + group2) / 2, diff.perc = 100 * diff / mean) 
  
  specs <- data %>%
    summarise(mean_diff.abs = mean(diff), mean_diff.perc = mean(diff.perc), sd_diff.abs = sd(diff), sd_diff.perc = sd(diff.perc))
  
  ## using a t.test to produce the confidence intervals of the mean of differences. 
  ## the implicit hypothesis is that the difference between the two groups is not significant 
  ## that would mean that the confidence intervals include the zero value in the plot
  
  tab <- tibble(
    bias =  specs$mean_diff.abs,
    min.CI =  t.test(data$diff)$conf.int[1],
    max.CI =  t.test(data$diff)$conf.int[2],
    SD = specs$sd_diff.abs ,
    "mean - 1.96 SD" = specs$mean_diff.abs - 1.96 * specs$sd_diff.abs ,
    "mean + 1.96 SD" = specs$mean_diff.abs + 1.96 * specs$sd_diff.abs 
  )
  
  diff_axis <- data$diff
  
  ## A histogram to observe the distribution of the differences between the two groups
  h <- ggplot() +
    geom_histogram(aes(x = diff_axis, y = after_stat(density)),
                   fill = "white", color = "grey", bins = n_bins
    ) +
    geom_vline(aes(
      xintercept = c(tab$`mean - 1.96 SD`, tab$`mean + 1.96 SD`),
      linetype = "+/- 1,96 SD from the bias"
    ),
    color = "blue", linetype = 2
    ) +
    labs(
      x = "", y = "density",
      title = "",
      subtitle = " \n "
    ) +
    scale_x_continuous(n.breaks = n_breaks) +
    geom_density(aes(x = diff_axis), col = "red", linewidth = 0.5, linetype = 1) +
    coord_flip()
  
  
  lab_min <- layer_scales(h)$x$get_limits()[1]
  lab_max <- layer_scales(h)$x$get_limits()[2]
  
  ## The plotted data includes the difference between the two groups, 
  ## along with the mean difference and the confidence intervals associated with it.
  p <- ggplot() +
    geom_point(aes(x = data$mean, y = diff_axis)) +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_hline(aes(yintercept = tab$bias), color = "red", linewidth = 1) +
    geom_hline(aes(
      yintercept = c(tab$min.CI, tab$max.CI),
      linetype = "95% confidence intervals of the bias"
    ),
    colour = "red",
    linewidth = 1
    ) +
    geom_hline(aes(
      yintercept = c(tab$`mean - 1.96 SD`, tab$`mean + 1.96 SD`),
      linetype = "+/- 1,96 SD from the bias"
    ),
    colour = "blue"
    ) +
    scale_linetype_manual(
      name = "Legend",
      values = c(2, 2),
      guide = guide_legend(override.aes = list(color = c("blue", "red")))
    ) +
    theme(legend.position = "bottom") +
    scale_y_continuous(n.breaks = n_breaks, limits = c(lab_min, lab_max)) +
    labs(
      x = "measurement value", y = "difference between the two groups (group1 - group2)",
      title = paste0("Difference plot", " (n = ", nrow(data), ")"),
      subtitle = paste0("method2", " - ", "method1")
    ) 
  
  ## In this case, we use ggpubr to present the two plots side by side
  dplot <- ggpubr::ggarrange(p, h,
                             ncol = 2, nrow = 1, widths = c(0.7, 0.3),
                             common.legend = TRUE, legend = "bottom"
  )
  
  return(list(dplot = dplot, diff_plot = p, histogram = h, table = tab))
  
  
}


