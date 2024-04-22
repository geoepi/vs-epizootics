plot_seirsei <- function(output){
  
  output_mlt <- reshape2::melt(output, "date") %>%
    filter(variable %in% c("Sh", "Eh", "Ih", "Rh"))
  
  adj_inc <- output %>% select(date, adj_inc)
  
  scaling_factor <- max(output$Ih) / max(adj_inc$adj_inc)
  adj_inc$rescaled_inc <- adj_inc$adj_inc * scaling_factor
  
  min_date <- min(output$date)
  max_date <- max(output$date)

    gg <- ggplot() +
      geom_bar(data = adj_inc,
               aes(date, rescaled_inc), stat="identity", alpha=0.3, 
                   inherit.aes=FALSE, color="transparent", 
               fill="gray40", width=1) +
      geom_line(data=output_mlt, 
                aes(date, value, group=variable, color=variable), linewidth = 1.0) +
      scale_x_date(date_breaks = "43 day", date_labels = "%b %d",
                   limits = c(min_date, max_date)) +
      scale_y_continuous(sec.axis = sec_axis(~ . / scaling_factor, name = "Simulated Incidence")) +
      scale_colour_manual(values = c("#B15928", "#1F78B4", "#E31A1C", "#33A02C")) +
      scale_fill_manual(values = c("#FFFF99",  "#A6CEE3","#FB9A99", "#B2DF8A")) + 
      xlab(" ") + 
      ylab("Population") +
      theme_bw() +
      theme(plot.margin = unit(c(1,0.5,1,0.5),"cm"),
            legend.direction = "horizontal",
            legend.position="none", 
            strip.text = element_text(size=18, face="bold"),
            strip.background = element_blank(),
            legend.key.size = unit(2,"line"),
            legend.key.width = unit(3,"line"),
            legend.text = element_text(size=16, face="bold"),
            legend.title = element_text(size=18, face="bold"),
            axis.title.x = element_text(size=24, face="bold"),
            axis.title.y = element_text(size=24, face="bold"),
            axis.text.x = element_text(face="bold", size=18, vjust=1, 
                                       hjust=1, angle=45),
            axis.text.y = element_text(size=20, face="bold"),
            plot.title = element_text(size=22, face="bold"))
    
    return(gg)
    
}
