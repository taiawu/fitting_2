annotate_tidied_components <-
  function(tidied) {
    # dplyr mutate if_else
    # readr parse_number
    # tidyr unite
    
    tryCatch({
      tidied %>% # the result of broom::tidy
        dplyr::mutate(component_number = readr::parse_number(term),
                      term_type = gsub(pattern = "([0-9]+).*$", replacement = "", term),
                      component_type = if_else(term_type %in% c("id_d", "id_b"),
                                               true = "initial",
                                               false = "sigmoid")) %>%
        tidyr::unite(component, c(component_type, component_number), sep = "_", remove = FALSE)
    }, warning = function(w) {tidied},
    error = function(e) {tidied})
  }

add_component_predictions <- 
  function(tidied, data, 
           temp_col = "Temperature",
           rescale_temp_col = TRUE) {
    # dplyr pull select group_by mutate bind_rows
    # purrr map set_names
    # tidyr nest unnest
    # scales rescale
    # rlang `!!` `:=`
    
    tryCatch({
      temp_vec <- data %>% dplyr::pull({{ temp_col }})
      
      #### needs to be for temp norm
      if(rescale_temp_col) { temp_vec <- scales::rescale(temp_vec, to = c(0,1))}
      
      component_pred <- 
        tidied %>%
        dplyr::select(term, term_type, component_type, component, estimate) %>%
        dplyr::group_by(component) %>%
        tidyr::nest() %>%
        dplyr::mutate(comp_pred = purrr::map(data, predict_component, x_vec = temp_vec)) %>%
        dplyr::select(component, comp_pred) %>%
        tidyr::unnest(cols = c(comp_pred)) %>%
        purrr::set_names(c("which_value", temp_col, "value"))  %>%
        dplyr::mutate(!!temp_col := data %>% pull({{ temp_col }})) # replacing again with non-norm temperature
      
      out <- data %>% 
        bind_rows(component_pred)
    },
    error = function(e){print("error on pred")
      data},
    warning = function(w){print("warning on pred")
      data})
  }

get_component_exprs <-
  function(component_type) {
    switch(component_type,
           "sigmoid" = quote(Asym/(1 + exp((xmid - x_val)/scal)) * exp(d * (x_val - xmid))),
           "initial" = quote(id_d * exp(x_val * id_b)))
  }

predict_component <-
  function(df, 
           eval_switch = dsfworld_components,
           x_vec = seq(from = 0, to = 1, length = 70),
           ... # currently does nothing, but good for passing extra external pars
  ) {
    
    # tibble tibble
    
    component_type <- unique(df$component_type) 
    to_eval <- get_component_exprs(component_type)
    
    par_list <- as.list(df$estimate)
    names(par_list) <- df$term_type
    par_list <- c(par_list,
                  "x_val" = list(x_vec))
    
    tibble::tibble(x = x_vec,
                   y = eval(to_eval, par_list))
  }

