plot_mesh <- function(mesh, node_col = "darkred", edge_col = "gray60"){
  
  mesh_df <- extract_mesh_network(mesh)
  
  gg <- ggplot(data = mesh_df$nodes, 
              aes(x = .data[['x']], y = .data[['y']],
                  colour = .data[['type']], size = .data[['type']])) +
    geom_segment(data = mesh_df$edges, 
                 aes(x = .data[['x1']], y = .data[['y1']],
                     xend = .data[['x2']], yend = .data[['y2']])) +
    geom_point(shape = 19) +
    scale_colour_manual(values = c(node_col, edge_col, 'gray20', 'gray40', 'black'), drop = FALSE) +
    scale_size_manual(values = c(1.3, 0.5, 1.3, 1.3, 0), drop = FALSE) +
    xlab("Easting") +
    ylab("Northing") +
    theme_minimal() +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
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
  
  return(gg)
}