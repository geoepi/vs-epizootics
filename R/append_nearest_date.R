append_nearest_date <- function(df1, df2, column_to_join) {
  # Ensure Date format
  df1$date <- as.Date(df1$date)
  df2$date <- as.Date(df2$date)
  
  # Add an identifier 
  df1$id_df1 <- 1:nrow(df1)
  df2$id_df2 <- 1:nrow(df2)
  
  # Create a cross-join
  complete_df <- merge(df1, df2, by = NULL)
  
  # Calculate the time difference 
  complete_df$diff <- abs(as.numeric(difftime(complete_df$date.x, complete_df$date.y, units = "days")))
  
  # Find the minimum difference 
  closest_dates <- complete_df %>%
    group_by(id_df1) %>%
    filter(diff == min(diff)) %>%
    slice(1) %>%
    ungroup() %>%
    select(id_df1, date_closest = date.y, !!sym(column_to_join) := all_of(column_to_join))
  
  df1 <- df1 %>%
    left_join(closest_dates, by = "id_df1") %>%
    select(-id_df1, -date_closest)
  
  return(df1)
}