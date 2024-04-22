spatial_sample <- function(inc_df, obs_rast, targ_year = "2014") {
  
  results <- list()
  
  # Loop through each row 
  for (i in 1:nrow(inc_df)) {
    
    n <- inc_df$adj_inc[i]
    
    if (n == 0) {
      next
    }
    
    # Generate random points 
    sample_points <- as.data.frame(
      raptr::randomPoints(obs_rast, n = n, prob = TRUE)
    ) %>%
      mutate(year = year(inc_df$date[i]),
             set = "sim",
             date = inc_df$date[i])
    
    results[[i]] <- sample_points
  }
  
  rand_sample <- bind_rows(results)
  
  return(rand_sample)
}
