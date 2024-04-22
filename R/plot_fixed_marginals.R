plot_fixed_marginals <- function(model, parameters = c("pred_var_14", "pred_var_15")){
  
  marginals_df <- model$marginals.fixed
  
  for(i in 1:length(names(marginals_df))){
    
    tmp_fxd = as.data.frame(marginals_df[[i]])
    tmp_fxd$variable = names(marginals_df)[i]
    
    if(i == 1){linear_df = tmp_fxd
    } else{linear_df = rbind(linear_df, tmp_fxd)}
  }
  
  unique(linear_df$variable)
  
  pred_vars <- linear_df %>%
    filter(variable %in% parameters)
  
  gg <- ggplot(pred_vars,aes(x,y, group = variable, col=variable)) +
    geom_line() +
    xlab("Coefficient Value") +
    ylab("Posterior Density") +
    theme_minimal() +
    theme(plot.margin = unit(c(0.7,1,0.7,1),"cm"),
          legend.direction = "vertical",
          legend.position= c(0.9, 0.8), 
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