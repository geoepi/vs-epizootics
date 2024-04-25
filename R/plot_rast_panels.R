plot_rast_panels <- function(raster_stack, domain_fort, plot_sim, color_palette_name, point_col = "gray40", plot_pnts = TRUE, center=FALSE) {
  
  df1 <- as.data.frame(raster_stack[[1]], xy = TRUE, na.rm = TRUE, layer = 1)
  names(df1)[3] <- "RF"
  df2 <- as.data.frame(raster_stack[[2]], xy = TRUE, na.rm = TRUE, layer = 2)
  names(df2)[3] <- "RF"
  
  # Combine the data frames and add an ID to distinguish the layers
  df1$year <- 2014
  df2$year <- 2015
  joined_df <- rbind(df1, df2)
  
  # Define the color palette from the pals package
  color_palette <- color_palette_name
  
  domain_fort <- fortify(as(domain_fort, "Spatial"))
  
  min_RF <- min(min(joined_df$RF, na.rm = TRUE))
  max_RF <- ceiling(max(joined_df$RF, na.rm = TRUE))
  
  if(center == FALSE){
    col_values = c(0, 0.05, 0.25, 1)
  } else{
    col_values = c(0, 0.25, 0.5, 0.75, 1)
  }
  
  # Create the ggplot
  gg <- ggplot(joined_df, aes(x = x, y = y, fill = RF)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradientn(colours = color_palette, limits = c(min_RF, max_RF), values = col_values) +
    geom_polygon(data = domain_fort, aes(long,lat, group=group),
                 fill = "transparent", col="gray50", linewidth = 0.1) +
    xlab("Easting") +
    ylab("Northing") +
    facet_wrap(~ year, ncol = 2) +
    theme_minimal() +
    theme(plot.margin = unit(c(2,0.4,2,0.3),"cm"),
          legend.direction = "vertical",
          legend.position= c(0.97, 0.8), 
          strip.text = element_text(size=26, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.text = element_text(size=12, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(face="bold", size=12, vjust=1, 
                                     hjust=1, angle=45),
          axis.text.y = element_text(size=12, face="bold"),
          plot.title = element_text(size=28, face="bold"))
  
  
  if(plot_pnts == TRUE){
    
    gg <- gg + geom_point(data = plot_sim,
                          aes(x, y), col=point_col, size=2.25, shape=1, fill=NA, alpha=0.5) 
    
  }
  
  return(gg)
}