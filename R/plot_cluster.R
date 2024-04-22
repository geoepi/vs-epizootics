plot_cluster <- function(model, parameter = "NN"){
  
  cluster_out <- model$summary.random[[parameter]][,1:6]
  names(cluster_out) <- c("Distance", "Mean", "sd", "Q_0.025", "Q_0.5", "Q_0.975")
  cluster_out$Year <-rep(c(2014, 2015), each = nrow(cluster_out)/2)
  
  gg <- ggplot(cluster_out, aes(Distance, Q_0.5)) +
    geom_hline(yintercept = 0, col="darkred", linetype="dotdash", linewidth=1) +
    geom_ribbon(aes(x=Distance, ymin=Q_0.025, ymax=Q_0.975), fill="steelblue", alpha=0.3) +
    geom_line(col="black", linewidth = 1.25) + 
    xlim(0, 175) + 
    ylim(-5, 10) +
    ylab("Cluster Coefficient (log)") +
    facet_wrap(~Year, ncol=2) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.7,1,0.7,1),"cm"),
          legend.direction = "vertical",
          legend.position= c(0.98, 0.8), 
          strip.text = element_text(size=26, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(face="bold", size=12, vjust=1, 
                                     hjust=1, angle=45),
          axis.text.y = element_text(size=12, face="bold"),
          plot.title = element_text(size=28, face="bold"))
  
  return(gg)
}