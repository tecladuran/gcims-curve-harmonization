interpolate_concentration <- function(intensity, calibration_intensity, calibration_concentration) {
  # extreure els límits del calibratge
  min_int <- min(calibration_intensity, na.rm = TRUE)
  max_int <- max(calibration_intensity, na.rm = TRUE)
  
  # aproximem només dins del rang (rule = 1 → NA fora de [min, max])
  est <- approx(
    x    = calibration_intensity,
    y    = calibration_concentration,
    xout = intensity,
    rule = 1
  )$y
  
  # per sota del mínim: posem 0
  est[intensity < min_int] <- 0
  
  return(est)
}
