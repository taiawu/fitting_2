get_drfu_tma <- 
  function(data,
           .x_vec = "Temperature", # is Temperature
           .y_vec = "drfu_norm", # is drfu
           .precision = 0.1) {
    
    # handle either quoted or symbol inputs
    .x_vec <- as.name(substitute(.x_vec))
    .y_vec <- as.name(substitute(.y_vec))
    
    col_nm <- c(rlang::as_string(.x_vec),
                rlang::as_string(.y_vec))
    
    #_____Check input column names____
    if (!all( col_nm %in% names(data))) { # ensure user columns present in df
      abort_bad_argument("supplied column names not present in dataframe. All columns",
                         must = glue::glue("be in dataframe names: {glue::glue_collapse(names(data), sep = ', ')}"),
                         not = NULL ) }
    
    
    df <- 
      tibble::tibble(x =  data[[.x_vec]],
                     y = data[[.y_vec]])
    
    grid <-
      tibble::tibble( x = seq(min(df$x), max(df$x), by = .precision)) %>%
      modelr::add_predictions(loess(y ~ x, data = df, span = .precision))
    
    tma <- grid$x[which(grid$pred == max(grid$pred))][[1]] # 1 is incase there are ~equal maxes
    
  }