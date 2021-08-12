###### ---- given drfu data, estimates major and minor transitions, and returns them ordered by their magnitude
estimate_transitions <- 
  function( values, 
            norm_raw_y,
            norm_raw_x,
            transition = "major",
            min_peak_height = 0.0002,
            min_valley_height = 0.000001,
            low_temp_margin = 5,
            high_temp_margin = 95) {
    
    # stats loess predict
    # quantmod findPeaks findValleys
    # secondary dependency: signal sgolayfilt (from sgolay function)
    # tibble tibble
    # dplyr arrange desc mutate
    
    # smooth the input vector ### is this really necessary?
    norm_length <- c(1:length(values))/length(values)
    interp <- stats::loess( values ~ norm_length, span = 0.1) %>%  stats::predict()
    
    if (transition == "major") {
      points <- interp %>% quantmod::findPeaks()
      
    } else if (transition == "minor") {
      dval <- values %>% sgolay(m = 1)
      interp <- loess( dval ~ norm_length, span = 0.1) %>%  predict()
      
      points <- interp %>% abs() %>% quantmod::findValleys() 
      
    } else if (transition == "initial") {
      
      init_mean <- norm_raw_y[c(1:3)] %>% mean() 
      
      out <- 
        tibble::tibble(est_row = 1,
                       val_at_est =  init_mean,
                       est_val = init_mean,
                       est_raw_x = norm_raw_x[1],
                       est_raw_y = norm_raw_y[1],
                       est_type = transition) %>%
        dplyr::arrange(dplyr::desc(val_at_est)) %>%
        dplyr::mutate(est_rank = c(1:nrow(.)))
      
      return(out) ###### RETURNS HERE
      
    } else {
      print("Invalid transition type requested; returning major transition")
      points <- interp %>% quantmod::findPeaks()
    }
    
    # correct for indexing
    points <- points - 1
    
    # return peaks, removing ones which fall outside the legitimate tma range
    points[points > low_temp_margin | points > high_temp_margin]
    
    out <- 
      tibble::tibble(est_row = points,
                     val_at_est =  values[points], # the value from drfu
                     est_val = norm_raw_x[points], # the value passed to par
                     est_raw_x = norm_raw_x[points],
                     est_raw_y = norm_raw_y[points],
                     est_type = transition) %>%
      dplyr::arrange(dplyr::desc(val_at_est)) %>%
      dplyr::mutate(est_rank = c(1:nrow(.)))
  }


add_estimates <-
  function(df,
           .est_peak_col = drfu_norm, 
           .est_init_col = value_norm,
           .norm_raw_x = Temperature_norm,
           .norm_raw_y = value_norm) {
    ##### how to we handle places where this fails?? 
    # probably in "estimate_transitions" function--correct?
    
    # dplyr bind_rows
    out <- 
      lapply(X = c("major", "minor", "initial"), # all three transtion types
             FUN = function(.x) { 
               estimate_transitions(values = df %>% pull({{ .est_peak_col }}),
                                    norm_raw_x = df %>% pull({{ .norm_raw_x }}),
                                    norm_raw_y = df %>% pull({{ .norm_raw_y }}),
                                    transition = .x) 
             }) %>%
      dplyr::bind_rows()
  }

pars_from_estimates <- #__translate estimates df to pars list for nlsLM
  function(estimates, # df with all estimate information
           which_pars, # all parameters desired
           pars_defaults = list(Asym1 = 1, xmid1 = 0.4, scal1 = 0.03, d1 = -0.5, 
                                Asym2 = 0.5,  xmid2 = 0.6, scal2 = 0.03,  d2 = -1, 
                                Asym3 = 0.3, xmid3 = 0.2, scal3 = 0.03, d3 = -1.5,
                                id_d1 = 0.2, id_b1 = -5), # not everything gets an estimate
           par_order = c("Asym1", "xmid1", "scal1", "d1", # par order must match final formula
                         "Asym2", "xmid2", "scal2", "d2",
                         "Asym3", "xmid3", "scal3", "d3",
                         "id_d1", "id_b1")) {
    
    ##### work to do on this function ########
    # intelligent estimate of Asym from data?
    # improved ordering of xmid values -- when to use minor or major transtions?
    
    # dplyr mutate if_else filter bind_rows arrange
    # tidyr unite
    # tibble tibble
    # purrr as_vector
    
    ## match estimates to par names
    par_df <- # keep as intermediate - useful for additional intelligent ordering
      estimates %>%
      # match parameter names to forumula
      dplyr::mutate(par_type = 
                      dplyr::if_else(est_type %in% c("minor", "major"), 
                                     true = "xmid", 
                                     false = "id_d")) %>%
      tidyr::unite(par_name, c(par_type, est_rank), 
                   sep = "", remove = FALSE) %>%
      
      # drop pars irrelevant or redundant to given formula
      dplyr::filter(est_type != "minor",  ### CURRENTLY IGNORES MINOR TRANSITIONS!!!!!!
                    par_name %in% which_pars) 
    
    ## create the final parameter list
    defaults <-
      tibble::tibble(par_name = names(pars_defaults),
                     est_val =  pars_defaults %>% purrr::as_vector()) %>%
      dplyr::filter(par_name %in% which_pars, # drop pars not in the model
                    ! par_name %in% par_df$par_name) # drop pars supplied by estimates
    
    #-----PARAMETER LIST ORDER HAS TO MATCH FORMULA -----#
    df <- # combine estimated and default pars
      dplyr::bind_rows(par_df, defaults) %>%
      dplyr::mutate(par_f = factor(par_name, levels = par_order)) %>%
      dplyr::arrange(par_f) 
    
    #### HERE -- ADD A CHECK TO REPLACE THE ESTIMATES WITH THE DEFAULTS IF THEY ARE WHACKY ###
    
    ## nlsLM expects a list
    out <- as.list(df$est_val)
    names(out) <- df$par_name
    out # can be fed directly to nlsLM as start pars
    
  }