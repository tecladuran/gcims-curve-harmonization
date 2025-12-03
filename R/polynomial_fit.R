fit_polynomials <- function(df, analytes, conc_col = "concentration",
                            degree = 3, intercept = FALSE) {
  
  coef_table <- tibble()
  metrics_table <- tibble()
  models <- list()
  
  for (analyte in analytes) {
    
    # detect shifted concentration
    conc_shifted <- paste0("c_", analyte)
    use_shifted <- conc_shifted %in% names(df)
    concentration_var <- ifelse(use_shifted, conc_shifted, conc_col)
    
    # build df
    tmp <- df %>%
      dplyr::select(all_of(concentration_var), all_of(analyte)) %>%
      dplyr::rename(
        concentration = all_of(concentration_var),
        intensity = all_of(analyte)
      )
    
    # formula
    if (intercept) {
      form <- as.formula(paste0("intensity ~ poly(concentration, ", degree, ", raw = TRUE)"))
    } else {
      form <- as.formula(paste0("intensity ~ poly(concentration, ", degree, ", raw = TRUE) - 1"))
    }
    
    # fit model
    fit <- lm(form, data = tmp)
    sfit <- summary(fit)
    
    # metrics essentials
    R2 <- sfit$r.squared
    adjR2 <- sfit$adj.r.squared
    RSE <- sfit$sigma
    RSS <- sum(fit$residuals^2)
    
    # save model
    models[[analyte]] <- fit
    
    # coefficients row
    cf <- coef(fit)
    coef_row <- tibble(
      Analyte = display_name(analyte),
      a0 = ifelse(intercept, cf[1], 0),
      a1 = cf[ifelse(intercept, 2, 1)],
      a2 = cf[ifelse(intercept, 3, 2)],
      a3 = cf[ifelse(intercept, 4, 3)]
    )
    coef_table <- bind_rows(coef_table, coef_row)
    
    # metrics row
    metrics_row <- tibble(
      Analyte = analyte,
      R2 = R2,
      adjR2 = adjR2,
      RSE = RSE,
      RSS = RSS
    )
    metrics_table <- bind_rows(metrics_table, metrics_row)
  }
  
  return(list(
    coef_table = coef_table,
    metrics_table = metrics_table,
    models = models
  ))
}




plot_polynomial_fits <- function(df, models_list) {
  
  for (analyte in names(models_list)) {
    
    fit <- models_list[[analyte]]
    cf <- coef(fit)
    
    # detect concentration variable
    conc_shifted <- paste0("c_", analyte)
    concentration_var <- ifelse(conc_shifted %in% names(df), conc_shifted, "concentration")
    
    tmp <- df %>%
      dplyr::select(all_of(concentration_var), all_of(analyte)) %>%
      dplyr::rename(
        concentration = all_of(concentration_var),
        intensity = all_of(analyte)
      )
    
    grid <- seq(min(tmp$concentration), max(tmp$concentration), length.out = 200)
    preds <- predict(fit, newdata = data.frame(concentration = grid))
    
    # equation text
    a0 <- ifelse("(Intercept)" %in% names(cf), cf[1], 0)
    a1 <- cf[ifelse("(Intercept)" %in% names(cf), 2, 1)]
    a2 <- cf[ifelse("(Intercept)" %in% names(cf), 3, 2)]
    a3 <- cf[ifelse("(Intercept)" %in% names(cf), 4, 3)]
    
    eq_text <- sprintf("f(C) = %.3f + %.3f·C + %.3f·C² + %.3f·C³",
                       a0, a1, a2, a3)
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = tmp,
        aes(x = concentration, y = intensity),
        color = "black", size = 2
      ) +
      ggplot2::geom_line(
        data = tibble(concentration = grid, intensity = preds),
        aes(x = concentration, y = intensity),
        color = "navy", size = 1
      ) +
      ggplot2::annotate(
        "text",
        x = min(tmp$concentration),
        y = max(tmp$intensity),
        label = eq_text,
        hjust = 0, vjust = 1, size = 5,
        color = "navy"
      ) +
      ggplot2::labs(
        title = display_name(analyte),
        x = "Concentration",
        y = "Intensity"
      ) +
      ggplot2::theme_minimal()+
      ggplot2::theme_minimal(base_size = 14) +  
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,      
          face = "bold",      
          size = 16   
        ))
    
    print(p)
  }
}