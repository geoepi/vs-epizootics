plot_exposure_seq <- function(mesh = mesh.dom, dd_nodes = dd, rast_data = host_pop.r, rast_scale = 10^3,
                              xmin = -11500, xmax = -10500, ymin = 3500, ymax = 4500){
  
  bbox_extent <- raster::extent(xmin, xmax, ymin, ymax)
  tessel_sp <- crop(as(tessel_mesh, "Spatial"), bbox_extent)
  
  rast_data <- crop(rast_data, bbox_extent)
  values(rast_data) <- values(rast_data)/rast_scale
  
  dd_nodes <- dd_nodes %>%
    filter(x >= xmin & x <= xmax,
           y >= ymin & y <= ymax)
  
  dd_nodes$horse_pop <- extract(tessel_sp,dd_nodes[,c("x","y")])[,"host_pop"]
  
  tessel_sp@data$id <- 1:nrow(tessel_sp@data)
  tessel_fort <- fortify(tessel_sp, region = "id")
  tessel_fort$id <- as.integer(tessel_fort$id)
  tessel_fort = left_join(tessel_fort, tessel_sp@data, by = "id")
  
  mesh_df <- extract_mesh_network(mesh)
  
  edges <- mesh_df$edges
  
  lines_list <- vector("list", length = nrow(edges))
  
  for (i in 1:nrow(edges)) {
    startPoint <- c(edges$x1[i], edges$y1[i])
    endPoint <- c(edges$x2[i], edges$y2[i])
    
    line <- Line(rbind(startPoint, endPoint))
    
    lines_list[[i]] <- Lines(list(line), ID = as.character(i))
  }
  
  spatial_lines <- SpatialLines(lines_list)
  proj4string(spatial_lines) <- crs(tessel_mesh)
  
  spatial_lines <- crop(spatial_lines, bbox_extent)
  line_data <- data.frame(LineData = 1:length(spatial_lines))
  rownames(line_data) <- sapply(slot(spatial_lines, "lines"), function(l) slot(l, "ID"))
  lines_df <- SpatialLinesDataFrame(spatial_lines, data = line_data)
  lines_fort <- fortify(lines_df)
  
  grid_tiles <- as.data.frame(rast_data, xy = TRUE, na.rm = TRUE, layer = 1)
  
  gg_tile.1 <- ggplot() +
    geom_raster(data=grid_tiles, 
                aes(x = x, y = y, fill = host_pop), interpolate = TRUE) +
    scale_fill_viridis_c(option = "plasma", alpha = 0.3, direction = -1) + 
    geom_path(data = lines_fort,
              aes(x = long, y = lat, group = group), 
              color = "gray40", linewidth = 0.15) +
    xlab(" ") +
    ylab("Northing") +
    theme_minimal() + 
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.direction = "vertical",
          legend.position= "none", 
          strip.text = element_text(size=26, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=12, face="bold"),
          plot.title = element_text(size=28, face="bold"))
  
  gg_tile.2 <- ggplot() +
    geom_raster(data=grid_tiles, 
                aes(x = x, y = y, fill = host_pop), interpolate = TRUE) +
    scale_fill_viridis_c(option = "plasma", alpha = 0.3, direction = -1) + 
    geom_polygon(data = tessel_fort,
                 aes(long,lat, group=group), fill = "transparent",
                 col="gray50", linetype = "dotdash", linewidth = 0.5) +
    geom_point(data = dd_nodes,
               aes(x, y), fill="gray10", col = "gray1", size=1.75, shape=21) +
    xlab(" ") +
    ylab(" ") +
    theme_minimal() + 
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.direction = "vertical",
          legend.position= "none", 
          strip.text = element_text(size=26, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(1,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size=28, face="bold"))
  
  gg_tile.3 <- ggplot() +
    geom_polygon(data = tessel_fort,
                 aes(long,lat, group=group, fill = host_pop),
                 col="gray50", linetype = "dotdash", linewidth = 0.5) +
    scale_fill_viridis_c(option = "plasma", alpha = 0.3, direction = -1) + 
    xlab("Easting") +
    ylab("Northing") +
    theme_minimal() + 
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.direction = "vertical",
          legend.position= "none", 
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
  
  gg_tile.4 <- ggplot() +
    geom_path(data = lines_fort,
              aes(x = long, y = lat, group = group), 
              color = "gray40", linewidth = 0.15) +
    geom_point(data = dd_nodes,
               aes(x, y, fill = horse_pop), col = "gray1",size=1.75, shape=21) +
    scale_colour_viridis_c(option = "plasma", direction = -1) +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    xlab("Easting") +
    ylab(" ") +
    theme_minimal() + 
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.direction = "vertical",
          legend.position= "none", 
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
          axis.text.y =  element_blank(),
          plot.title = element_text(size=28, face="bold"))
  
  return(gridExtra::grid.arrange(gg_tile.1, gg_tile.2, gg_tile.3, gg_tile.4, ncol=2))
}