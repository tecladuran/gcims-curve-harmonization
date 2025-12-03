extrapolate_curve <- function(df, delta_ppb = 5) {
  
  df2 <- df %>% arrange(intensity)
  
  # Últims dos punts
  p1 <- df2[nrow(df2)-1, ]
  p2 <- df2[nrow(df2), ]
  
  # Pendent en escala harmonitzada
  slope_h <- (p2$concentration_harmonized - p1$concentration_harmonized) /
    (p2$intensity - p1$intensity)
  
  # Intensitat extrapolada (a partir de p2)
  extrap_intensity <- p2$intensity +
    (delta_ppb / slope_h)
  
  # Concentració harmonitzada extrapolada
  extrap_conc_h <- p2$concentration_harmonized + delta_ppb
  
  # Pendent original
  slope_o <- (p2$concentration_original - p1$concentration_original) /
    (p2$intensity - p1$intensity)
  
  # Concentració original extrapolada
  extrap_conc_o <- p2$concentration_original +
    slope_o * (extrap_intensity - p2$intensity)
  
  extrap_row <- tibble(
    intensity = extrap_intensity,
    concentration_original = extrap_conc_o,
    concentration_harmonized = extrap_conc_h
  )
  
  list(
    full = bind_rows(df2, extrap_row),
    seg_original = bind_rows(p2, extrap_row),
    seg_harmonized = bind_rows(p2, extrap_row),
    extrap = extrap_row
  )
}


plot_extrapolated_curve <- function(extra, title) {
  ggplot() +
    
    # Original curve
    geom_line(
      aes(x = intensity, y = concentration_original, color = "Original"),
      data = extra$full, linewidth = 1
    ) +
    
    # Harmonized curve
    geom_line(
      aes(x = intensity, y = concentration_harmonized, color = "Harmonized"),
      data = extra$full, linewidth = 1
    ) +
    
    # Extrapolated original (red)
    geom_line(
      aes(x = intensity, y = concentration_original, color = "Extrapolated"),
      data = extra$seg_original, linewidth = 1
    ) +
    
    # Extrapolated harmonized (red)
    geom_line(
      aes(x = intensity, y = concentration_harmonized, color = "Extrapolated"),
      data = extra$seg_harmonized, linewidth = 1
    ) +
    
    scale_color_manual(
      name = "Curve type",
      values = c(
        "Original" = "darkorange",
        "Harmonized" = "navy",
        "Extrapolated" = "red"
      )
    ) +
    
    labs(
      title = title,
      x = "Intensity",
      y = "Concentration (ppb)"
    ) +
    
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom"
    )
}
