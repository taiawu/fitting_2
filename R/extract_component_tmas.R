add_component_preds <-
  function(df) {
    # this should be updated so that the tidied column doesn't need 
    out <- 
      df %>%
      dplyr::mutate(tidied = purrr::map(tidied, annotate_tidied_components),
                    data = purrr::map2(tidied, data, add_component_predictions))
  }

add_component_tmas <-
  function(df) {
    out <- 
      df %>%
      dplyr::mutate(model_tma = 
                      tryCatch(purrr::map(data, extract_component_tma), 
                               error = function(e) {NA}, warning = function(w) {NA})) %>%
      dplyr::select(.var, model_name, model_tma) %>%
      tidyr::unnest(cols = c(model_tma))
    
  }

extract_component_tma <-
  function(data,
           component_names = c("sigmoid_1", "sigmoid_2", "sigmoid_3")) {
    # dplyr filter group_by mutate ungroup select
    # tidyr nest
    # purrr map
    # scales rescale

    df <- data %>%
      dplyr::filter(which_value %in% component_names) %>%
      dplyr::group_by(which_value) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~ dplyr::mutate(.x,
                                                            value_norm = scales::rescale(value, to = c(0,1)),
                                                            drfu_norm = sgolay(value_norm, m = 1)
      )),
      tma = purrr::map(data, get_drfu_tma, .y_vec = "drfu_norm")[[1]])

    out <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(which_value, tma)
  }

sort_tmas <-
  function(df, 
           sort_type = "low_high", 
           close_val = 0,
           .tma_col = "tma") {
    # dplyr group_by mutate min_rank arrange
    # tidyr nest unnest
    # purrr map
    if(is.null(df)) { stop("Input dataframe is NULL")}
    if(!is.numeric(close_val)) {
      warning("`close_val` is not numeric. Coercing to numeric.")
      close_val <- as.numeric(close_val)
      if(is.na(close_val)) {warning("`close_val couldn't be converted to numeric. Coerced to 0")}
    }
    
    .tma_col <- base::as.name(base::substitute(.tma_col))
    
    close_val <-
      switch(sort_type,
             "low_high" = 0,
             "closest_to" = close_val)
    
    df <- df %>%
      dplyr::group_by(.var, model_name) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~ 
                                        dplyr::mutate(.x,
                                                      rank =  try(dplyr::min_rank(abs({{ .tma_col }} - close_val)) )))) %>% 
      tidyr::unnest(cols = c(data)) %>%
      dplyr::arrange(.var, model_name, rank)
  }