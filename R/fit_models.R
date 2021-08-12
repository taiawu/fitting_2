fit_to_model <- # an updated version of compose_dsf_model, with a better name
  function(df,
           start_pars,
           formula, 
           lower_bound,
           control_list = list(maxiter = 500)) {
    
    # minpack.lm nlsLM
    tryCatch(
      
      minpack.lm::nlsLM(formula,
                        data = df,
                        start = start_pars,
                        lower = lower_bound,
                        control = control_list),
      
      warning = function(w) return(NA), error = function(e) return(NA)
      
    )
  }

fit_all <- 
  function(by_variable,
           which_pars, # formulas$model_2$pars %>% names())[1]
           formula, # formulas$model_2$formula
           lower_bound, # manual_pars$m2_low
           drop_raw = TRUE,
           temperature_col = c("Temperature"),
           keep_data_cols = c("resid", "pred"),
           ...
  ) {
    
    # dplyr filter
    # purrr map map2
    # broom tidy glance augment
    # modelr add_residuals add_predictions
    out <- 
      by_variable %>%
      ### ultimately, I think we could / should add starting parameter estimates to this function
      dplyr::mutate(est = map(data, add_estimates),
                    pars = tryCatch( # translate estimates to start pars
                      purrr::map(est, 
                                 pars_from_estimates, 
                                 which_pars = which_pars)[1], 
                      warning = function(w) return(NA), error = function(e) return(NA)),
                    model = tryCatch( # fit to the given formula
                      purrr::map2(data, 
                                  pars, 
                                  fit_to_model, 
                                  formula = formula, 
                                  lower_bound = lower_bound),
                      warning = function(w) return(NA), error = function(e) return(NA)),
                    # model qualities and predictions
                    tidied  = tryCatch(purrr::map(model, broom::tidy), warning = function(w) return(NA), error = function(e) return(NA)), # extract model parameters
                    glanced = tryCatch(purrr::map(model, broom::glance), warning = function(w) return(NA), error = function(e) return(NA)),
                    data    = tryCatch(purrr::map2(data, model, modelr::add_residuals), warning = function(w) return(data), error = function(e) return(data)), # add the residuals
                    data    = tryCatch(purrr::map2(data, model, modelr::add_predictions), warning = function(w) return(data), error = function(e) return(data)) # model is the column we created earlier, not the actual model itself
      ) 
    
    if( drop_raw ) {
      out <- out %>%
        mutate(data = tryCatch(map(data, ~ select(.x, any_of(c(temperature_col, keep_data_cols)))), warning = function(w) return(data), error = function(e) return(data))) 
    }
    ###### give this a more informative error message or when the model didn't converge
    #        Error : Problem with `mutate()` column `data`.
    # ℹ `data = map(data, ~select(.x, any_of(keep_data_cols)))`.
    # x no applicable method for 'select' applied to an object of class "NULL"
    # ℹ The error occurred in group 100: well = "E12".
    
    # pivot longer -- is cleaner in the downstream handling steps    
    out <- out %>%
      mutate(data  = tryCatch(map(data, pivot_longer, 
                                  cols = any_of(keep_data_cols), 
                                  names_to = "which_value", 
                                  values_to = "value"),
                              warning = function(w) return(data), 
                              error = function(e) return(data)
      )
      )
    
  }

get_model_tma <-
  function(by_var,
           which_models, # options c("model_1", "model_2", "model_3", "model_4", "model_5", "model_6")
           #dsfworld_formulas, # should we make this an argument? unsure how to pass formulas to this function
           dsfworld_models = TRUE,
           model_pars = NULL,
           model_formula = NULL,
           model_lower_bound = NULL,
           ...) {
    
    dsfworld_model_names <- c("model_1", "model_2", "model_3", "model_4", "model_5", "model_6")
    
    if( which_models == "all") { which_models <- dsfworld_model_names }
    # model names are unique
    if(! length(which_models) == length(unique(which_models))) {
      warning("model names supplied to `which_models` are not all unique. \n Fitting each model name only once.")
      which_models <- unique(which_models) }
    
    # if dsfworld models, names can be found
    if(dsfworld_models == TRUE) {
      if(is.null(formulas)) {
        stop("dsfworld models selected, but `formulas` object cannot be found. Please defines `formulas` object in the global workspace.")
      }
      
      if(! all(which_models %in% dsfworld_model_names)) {
        stop(glue::glue("Supplied model names not in dsfworld models. \n dsfworld model names are: {glue::glue_collapse(dsfworld_model_names, sep = ', ')}"))
      }
    }
    
    # if user-supplied models, all input are legit
    if(dsfworld_models == FALSE) {
      if (any(is.null(model_pars),
              is.null(model_formula),
              is.null(model_lower_bound))) {
        stop("`dsfworld_models` set to FALSE, but user-supplied model aruguments `model_pars`, `model_formula`, `model_lower_bound` are missing.")
      }
    }
    
    out <-
      which_models %>%
      lapply( . ,function(x) {
        fit_all(by_var,
                formulas[[x]]$pars %>% names(),
                formulas[[x]]$formula,
                formulas[[x]]$lower_bound) %>%
          mutate(model_name = x) %>%
          dplyr::relocate(.var, model_name)}
      ) %>%
      dplyr::bind_rows()
  }