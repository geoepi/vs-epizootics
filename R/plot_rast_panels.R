plot_rast_panels <- function(raster_stack, domain_fort, plot_sim, color_palette_name) {
  
  df1 <- as.data.frame(obs_stack.r[[1]], xy = TRUE, na.rm = TRUE, layer = 1)
  names(df1)[3] <- "RF"
  df2 <- as.data.frame(obs_stack.r[[2]], xy = TRUE, na.rm = TRUE, layer = 2)
  names(df2)[3] <- "RF"
  
  # Combine the data frames and add an ID to distinguish the layers
  df1$year <- 2014
  df2$year <- 2015
  combined_df <- rbind(df1, df2)
  
  # Define the color palette from the pals package
  color_palette <- color_palette_name
  
  domain_fort <- fortify(as(domain_fort, "Spatial"))
  
  # Create the ggplot
  gg <- ggplot(combined_df, aes(x = x, y = y, fill = RF)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradientn(colours = color_palette, limits = c(0, 1), values = c(0, 0.05, 0.25, 1)) +
    geom_polygon(data = domain_fort, aes(long,lat, group=group),
                 fill = "transparent", col="gray50", linewidth = 0.1) +
    geom_point(data = plot_sim,
               aes(x, y), col="gray40", size=2.25, shape=1, fill=NA, alpha=0.5) +
    xlab("Easting") +
    ylab("Northing") +
    facet_wrap(~ year, ncol = 2) +
    theme_minimal() +
    theme(plot.margin = unit(c(2,0.7,2,0.7),"cm"),
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