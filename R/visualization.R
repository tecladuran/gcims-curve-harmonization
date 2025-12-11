plot_harmonization <- function(h, title = "Calibration Curve Harmonization") {
  
  # POOL original points
  pool_orig <- h$fitted_points %>%
    dplyr::transmute(
      concentration = concentration,
      intensity     = intensity,  
      type          = "POOL original"
    )
  
  # SU original curve
  su_orig <- h$su_grid %>%
    dplyr::mutate(type = "SU original")
  
  # SU transformed curve
  su_trans <- h$su_grid_trans %>%
    dplyr::mutate(type = "SU transformed")
  
  combined_df <- dplyr::bind_rows(
    pool_orig,
    su_orig,
    su_trans
  )
  
  ggplot(
    combined_df,
    aes(x = concentration, y = intensity, color = type, linetype = type)
  ) +
    # POOL points
    geom_point(
      data = dplyr::filter(combined_df, type == "POOL original"),
      size = 2.2,
      alpha = 0.9
    ) +
    # SU curves
    geom_line(
      data = dplyr::filter(combined_df, type != "POOL original"),
      linewidth = 0.8
    ) +
    scale_color_manual(values = c(
      "POOL original"  = "#E0711A",
      "SU transformed" = "navy",
      "SU original"    = "grey40"
    )) +
    scale_linetype_manual(values = c(
      "POOL original"  = "blank",
      "SU transformed" = "solid",
      "SU original"    = "dashed"
    )) +
    labs(
      title    = title,
      x        = "Concentration (ppb)",
      y        = "Intensity (a.u.)",
      color    = "Series",
      linetype = "Series"
    ) +
    theme_minimal(base_size = 14)
}

# PLOT MANUAL
plot_harmonization_manual <- function(
    h, 
    scale_manual, 
    shift_manual,
    title = "Manual Calibration Harmonization",
    xlim_range = c(0, 30) 
) {
  
  # POOL original
  pool_orig <- h$fitted_points %>%
    dplyr::transmute(
      concentration,
      intensity = intensity,
      type = "POOL"
    )
  
  # SU original
  su_orig <- h$su_grid %>%
    dplyr::mutate(type = "SU_original")
  
  # SU transformada manual
  C_grid  <- h$su_grid$concentration
  I_grid  <- h$su_grid$intensity
  C_trans <- C_grid / scale_manual - shift_manual
  
  su_trans_manual <- tibble::tibble(
    concentration = C_trans,
    intensity     = I_grid,
    type          = "SU_transformed"
  )
  
  combined_df <- dplyr::bind_rows(
    pool_orig,
    su_orig,
    su_trans_manual
  )
  
  # Fixed aesthetics
  color_values <- c(
    "POOL"           = "#E0711A",
    "SU_original"    = "grey40",
    "SU_transformed" = "navy"
  )
  
  linetype_values <- c(
    "POOL"           = "blank",
    "SU_original"    = "dashed",
    "SU_transformed" = "solid"
  )
  
  ggplot(
    combined_df,
    aes(x = concentration, y = intensity, color = type, linetype = type)
  ) +
    geom_point(
      data = dplyr::filter(combined_df, type == "POOL"),
      size = 2.2,
      alpha = 0.9
    ) +
    geom_line(
      data = dplyr::filter(combined_df, type != "POOL"),
      linewidth = 0.8
    ) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    labs(
      title = paste0(
        title,
        "\nscale = ", round(scale_manual, 2),
        " | shift = ", round(shift_manual, 2)
      ),
      x = "Concentration (ppb)",
      y = "Intensity (a.u.)",
      color = "Series",
      linetype = "Series"
    ) +
    theme_minimal(base_size = 14) +
    xlim(xlim_range[1], xlim_range[2])
}



plot_loss_heatmaps <- function(h,
                               title_prefix = "",
                               scale_window = NULL,
                               shift_window = NULL,
                               loss_factor = 2) {
  
  if (is.null(h$paraboloid)) {
    stop("No paraboloid fit stored in this harmonization result.")
  }
  
  # Extract uncertainties
  scale_err <- h$scale_error
  shift_err <- h$shift_error
  
  if (is.na(scale_err) | is.na(shift_err)) {
    stop("Uncertainty values are NA. Paraboloid fit may have failed.")
  }
  
  # Default windows = ± 2 × error
  if (is.null(scale_window)) scale_window <- 2 * scale_err
  if (is.null(shift_window)) shift_window <- 2 * shift_err
  
  # Limits
  scale_min <- h$scale - scale_window
  scale_max <- h$scale + scale_window
  
  shift_min <- h$shift - shift_window
  shift_max <- h$shift + shift_window
  
  # Filter loss grid
  lg <- h$loss_grid %>%
    dplyr::filter(
      is.finite(loss),
      scale >= scale_min, scale <= scale_max,
      shift >= shift_min, shift <= shift_max
    )
  
  # Paraboloid parameters
  a <- h$paraboloid$a
  b <- h$paraboloid$b
  c <- h$paraboloid$c
  
  s0 <- h$scale
  t0 <- h$shift
  Lmin <- h$loss
  
  # Paraboloid surface in filtered region
  lg_parab <- lg %>%
    dplyr::mutate(
      loss_parab = Lmin +
        a * (scale - s0)^2 +
        b * (shift - t0)^2 +
        c * (scale - s0) * (shift - t0)
    )
  
  contour_level <- loss_factor * Lmin
  
  # ---------------------------
  # REAL LOSS HEATMAP + white contour
  # ---------------------------
  p_real <- ggplot(lg, aes(x = scale, y = shift, fill = loss)) +
    geom_tile() +
    geom_contour(aes(z = loss),
                 breaks = contour_level,
                 colour = "white",
                 linewidth = 0.9) +
    geom_point(
      data = data.frame(scale = h$scale, shift = h$shift),
      aes(scale, shift),
      inherit.aes = FALSE,
      color = "red",
      size = 3
    ) +
    scale_fill_viridis_c(option = "inferno") +
    labs(
      title = paste0(title_prefix, "Loss landscape (real)"),
      x = "Scale",
      y = "Shift",
      fill = "Loss"
    ) +
    theme_minimal(base_size = 14)
  
  # ---------------------------
  # PARABOLOID LOSS HEATMAP + white contour
  # ---------------------------
  p_parab <- ggplot(lg_parab, aes(x = scale, y = shift, fill = loss_parab)) +
    geom_tile() +
    geom_contour(aes(z = loss_parab),
                 breaks = contour_level,
                 colour = "white",
                 linewidth = 0.9) +
    geom_point(
      data = data.frame(scale = h$scale, shift = h$shift),
      aes(scale, shift),
      inherit.aes = FALSE,
      color = "red",
      size = 3
    ) +
    scale_fill_viridis_c(option = "inferno") +
    labs(
      title = paste0(title_prefix, "Loss landscape (paraboloid fit)"),
      x = "Scale",
      y = "Shift",
      fill = "Loss (fit)"
    ) +
    theme_minimal(base_size = 14)
  
  list(real = p_real, paraboloid = p_parab)
}

plot_loss_1d <- function(h,
                         param = c("scale", "shift"),
                         range_limit = NULL,
                         loss_limits = NULL,
                         tol = 1e-6,
                         title = NULL) {
  
  param <- match.arg(param)
  
  if (is.null(h$paraboloid))
    stop("No paraboloid fit available.")
  
  # Extract optimum and curvature
  if (param == "scale") {
    a <- h$paraboloid$a
    if (is.na(a)) stop("Paraboloid coefficient 'a' is NA.")
    
    x0  <- h$scale
    err <- h$scale_error
    
    lg <- h$loss_grid %>%
      dplyr::filter(
        is.finite(loss),
        abs(.data$shift - h$shift) < tol
      ) %>%
      dplyr::arrange(.data$scale) %>%
      dplyr::mutate(
        x = .data$scale,
        loss_parab = h$loss + a * (x - x0)^2
      )
    
  } else {  # param == "shift"
    
    b <- h$paraboloid$b
    if (is.na(b)) stop("Paraboloid coefficient 'b' is NA.")
    
    x0  <- h$shift
    err <- h$shift_error
    
    lg <- h$loss_grid %>%
      dplyr::filter(
        is.finite(loss),
        abs(.data$scale - h$scale) < tol
      ) %>%
      dplyr::arrange(.data$shift) %>%
      dplyr::mutate(
        x = .data$shift,
        loss_parab = h$loss + b * (x - x0)^2
      )
  }
  
  # Default X-limits = ±2×error
  if (is.null(range_limit)) {
    range_limit <- c(x0 - 2 * err, x0 + 2 * err)
  }
  
  # Subset
  lg_local <- lg %>% dplyr::filter(x >= range_limit[1], x <= range_limit[2])
  
  # Default Y-limits
  if (is.null(loss_limits)) {
    ymin <- min(lg_local$loss, lg_local$loss_parab, na.rm = TRUE)
    ymax <- max(lg_local$loss, lg_local$loss_parab, na.rm = TRUE)
    pad  <- 0.10 * (ymax - ymin)
    loss_limits <- c(ymin - pad, ymax + pad)
  }
  
  if (is.null(title)) {
    title <- paste0("Loss vs ", param)
  }
  
  # Error bounds
  x_lower <- x0 - err
  x_upper <- x0 + err
  
  # Corresponding Y-values on the fitted paraboloid
  if (param == "scale") {
    y_lower <- h$loss + a * (x_lower - x0)^2
    y_upper <- h$loss + a * (x_upper - x0)^2
  } else {
    y_lower <- h$loss + b * (x_lower - x0)^2
    y_upper <- h$loss + b * (x_upper - x0)^2
  }
  
  # Plot
  p <- ggplot(lg_local, aes(x = x)) +
    
    geom_line(aes(y = loss, linetype = "Real loss"),
              color = "black", linewidth = 1) +
    
    geom_line(aes(y = loss_parab, linetype = "Paraboloid"),
              color = "navy", linewidth = 0.9) +
    
    geom_vline(aes(xintercept = x0, linetype = "Optimum"),
               color = "red", linewidth = 0.9) +
    
    # The corrected uncertainty markers:
    geom_point(aes(x = x_lower, y = y_lower),
               color = "red", shape = 4, size = 3, stroke = 1.3) +
    geom_point(aes(x = x_upper, y = y_upper),
               color = "red", shape = 4, size = 3, stroke = 1.3) +
    
    scale_linetype_manual(values = c(
      "Real loss"  = "solid",
      "Paraboloid" = "dashed",
      "Optimum"    = "dotted"
    )) +
    
    labs(
      title = title,
      x = param,
      y = "Loss",
      linetype = "Legend"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(legend.position = "right") +
    
    xlim(range_limit) +
    ylim(loss_limits)
  
  p
}

