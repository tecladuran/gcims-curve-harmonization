### Principal Function

#' Harmonize Calibration Curves by Scale and Shift Optimization
#'
#' This function finds the optimal \code{scale} and \code{shift} that best align
#' a reference calibration curve (SU) with a target dataset (POOL).
#'
#' The algorithm:
#' \itemize{
#'   \item Fits a cubic polynomial to the SU calibration curve.
#'   \item Samples the polynomial on a dense grid.
#'   \item Searches over a grid of scale/shift values.
#'   \item Computes a loss for each (scale, shift) pair
#'         (optionally variance-normalized).
#'   \item Selects the parameters that minimize the loss.
#'   \item Fits a local quadratic model (paraboloid) to estimate uncertainties.
#' }
#'
#' @param df_target Data frame with columns \code{concentration} and \code{intensity}.
#'        These are the POOL calibration points to be matched.
#'
#' @param df_sub Data frame with columns \code{concentration} and \code{intensity}.
#'        These form the SU reference calibration curve.
#'
#' @param scale_range Numeric vector of length 2. Range of scale values to explore.
#'
#' @param shift_range Numeric vector of length 2. Range of shift values to explore.
#'
#' @param n_scale Number of scale grid points.
#'
#' @param n_shift Number of shift grid points.
#'
#' @param n_grid Number of grid points for sampling the SU polynomial.
#'
#' @param min_fraction Minimum fraction of valid interpolated points required
#'        to compute a finite loss value.
#'
#' @param optimize_shift Logical. If \code{TRUE}, both scale and shift are optimized.
#'        If \code{FALSE}, only scale is optimized (shift fixed to zero).
#'
#' @param local_scale_win Window half-width (in scale units) for the local
#'        paraboloid fitting region.
#'
#' @param local_shift_win Window half-width (in shift units) for the local
#'        paraboloid fitting region.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{scale}, \code{shift} — optimal parameters.
#'   \item \code{loss} — minimal loss.
#'   \item \code{scale_error}, \code{shift_error} — uncertainty estimates.
#'   \item \code{paraboloid} — fitted quadratic model and Hessian-related values.
#'   \item \code{fitted_points} — POOL intensities and predicted values.
#'   \item \code{su_grid}, \code{su_grid_trans} — SU curve before/after transformation.
#'   \item \code{loss_grid} — full grid of loss values.
#' }
#'
#' @examples
#' \dontrun{
#'   res <- harmonize(pool_df, su_df)
#'   res$scale
#'   res$shift
#' }
#'
#' @export


harmonize <- function(df_target, df_sub,
                      scale_range = c(0.2, 1.5),
                      shift_range = c(0, 5),
                      n_scale = 200,
                      n_shift = 200,
                      n_grid = 200,
                      min_fraction = 0.6,
                      optimize_shift = TRUE,
                      local_scale_win = 0.1,
                      local_shift_win = 1) {
  
  # 1. Validate & prepare input
  x <- check_and_prepare_input(df_target, df_sub)
  df_target <- x$df_target
  df_sub    <- x$df_sub
  
  # 2. Fit SU model
  model_su <- fit_su_model(df_sub)
  
  # 3. Grid SU
  su_grid <- make_su_grid(df_sub, n_grid, model_su)
  
  # 4. Variance normalisation
  vn <- compute_variance_normalisation(df_target)
  df_target <- vn$df_target
  
  # 5. Grid search
  scale_seq <- seq(scale_range[1], scale_range[2], length.out = n_scale)
  shift_seq <- if (optimize_shift) seq(shift_range[1], shift_range[2], length.out = n_shift) else 0
  
  loss_grid <- compute_loss_grid(df_target, su_grid, scale_seq, shift_seq, min_fraction)
  
  best <- find_best_parameters(loss_grid)
  
  best_pred <- predict_best_curve(su_grid, df_target, best$best_scale, best$best_shift)
  
  # 6. Local paraboloid with Hessiana
  parab <- fit_local_paraboloid(
    loss_grid,
    best_scale = best$best_scale,
    best_shift = best$best_shift,
    best_loss  = best$best_loss,
    local_scale_win = local_scale_win,
    local_shift_win = local_shift_win
  )
  
  # Output structure
  list(
    scale = best$best_scale,
    shift = best$best_shift,
    loss  = best$best_loss,
    
    scale_error = parab$scale_error,
    shift_error = parab$shift_error,
    loss_threshold = parab$loss_threshold,
    paraboloid = parab,
    
    fitted_points = df_target %>%
      dplyr::mutate(intensity_pred = best_pred),
    
    su_grid = su_grid,
    su_grid_trans = tibble::tibble(
      concentration = su_grid$concentration / best$best_scale - best$best_shift,
      intensity     = su_grid$intensity
    ),
    
    loss_grid = loss_grid,
    variance_norm = vn$use_var_norm,
    min_fraction = min_fraction,
    optimize_shift = optimize_shift
  )
}




# AUXILIAR FUNCTIONS

# VALIDATE AND PREPARE INPUT
check_and_prepare_input <- function(df_target, df_sub) {
  
  if (!all(c("concentration", "intensity") %in% colnames(df_target)))
    stop("df_target must contain columns 'concentration' and 'intensity'.")
  
  if (!all(c("concentration", "intensity") %in% colnames(df_sub)))
    stop("df_sub must contain columns 'concentration' and 'intensity'.")
  
  df_target <- df_target %>%
    dplyr::mutate(
      concentration = as.numeric(concentration),
      intensity     = as.numeric(intensity)
    )
  
  df_sub <- df_sub %>%
    dplyr::mutate(
      concentration = as.numeric(concentration),
      intensity     = as.numeric(intensity)
    )
  
  list(df_target = df_target, df_sub = df_sub)
}

# FIT SU MODEL AND PREDICT

fit_su_model <- function(df_sub) {
  lm(intensity ~ poly(concentration, 3, raw = TRUE) - 1, data = df_sub)
}

predict_su <- function(model_su, C) {
  cf <- coef(model_su)
  cf[1] * C + cf[2] * C^2 + cf[3] * C^3
}

# MAKE SU GRID (sample the polynomial model)
make_su_grid <- function(df_sub, n_grid, model_su) {
  C_grid <- seq(0, max(df_sub$concentration, na.rm = TRUE), length.out = n_grid)
  I_grid <- predict_su(model_su, C_grid)
  tibble::tibble(concentration = C_grid, intensity = I_grid)
}

# CHOOSE IF VARIANCE NORMALIZATION IS APPLIED IN THE LOSS

compute_variance_normalisation <- function(df_target) {
  
  df_var <- df_target %>%
    dplyr::group_by(concentration) %>%
    dplyr::summarise(
      n             = dplyr::n(),
      var_intensity = if (n() > 1) var(intensity) else NA_real_,
      .groups       = "drop"
    )
  
  use_var_norm <- all(!is.na(df_var$var_intensity)) &&
    all(df_var$var_intensity > 0) &&
    all(df_var$n >= 2)
  
  df_target <- df_target %>%
    dplyr::left_join(df_var, by = "concentration") %>%
    dplyr::mutate(denom = ifelse(use_var_norm, var_intensity, 1))
  
  list(df_target = df_target, use_var_norm = use_var_norm)
}


# LOSS GRID COMPUTATION

compute_loss_grid <- function(df_target, su_grid, scale_seq, shift_seq, min_fraction) {
  
  C_t <- df_target$concentration
  I_t <- df_target$intensity
  D_t <- df_target$denom
  n_total <- length(C_t)
  
  C_grid <- su_grid$concentration
  I_grid <- su_grid$intensity
  
  loss_grid <- expand.grid(scale = scale_seq, shift = shift_seq)
  loss_grid$loss <- NA_real_
  
  for (i in seq_len(nrow(loss_grid))) {
    
    sc <- loss_grid$scale[i]
    sh <- loss_grid$shift[i]
    
    C_trans <- C_grid / sc - sh
    
    # Strict interpolation (no extrapolation)
    I_pred <- approx(x = C_trans, y = I_grid, xout = C_t, rule = 1)$y
    
    # Check the fraction of points within range
    valid <- !is.na(I_pred)
    
    # Not enough coverage → reject this (scale, shift)
    if (sum(valid) < min_fraction * n_total) {
      loss_grid$loss[i] <- Inf
      next
    }
    
    # Linear extrapolation only above the SU range
    above <- which(is.na(I_pred) & C_t > max(C_trans))
    if (length(above) > 0) {
      x1 <- C_trans[length(C_trans) - 1]
      x2 <- C_trans[length(C_trans)]
      y1 <- I_grid[length(I_grid) - 1]
      y2 <- I_grid[length(I_grid)]
      slope <- (y2 - y1) / (x2 - x1)
      I_pred[above] <- y2 + slope * (C_t[above] - x2)
    }
    
    # Compute loss using all points
    res_sq <- (I_t - I_pred)^2
    loss_grid$loss[i] <- mean(res_sq / D_t)
  }
  
  # Identify non-covered region and report approximate boundary line
  bad <- loss_grid[!is.finite(loss_grid$loss), , drop = FALSE]
  
  if (nrow(bad) > 2) {
    
    fit_line <- lm(shift ~ scale, data = bad)
    b0 <- coef(fit_line)[1]   # intercept
    b1 <- coef(fit_line)[2]   # slope
    
    # Linear boundary in:  a*shift + b*scale >= c
    a <- 1
    b <- -b1
    c <- b0
    
    
    cat(
      "Warning: insufficient SU coverage in part of the search grid.\n",
      "Approximate boundary of the uncovered region:\n",
      "   ", a, " * shift  +  ",
      b, " * scale  >=  ",
      c, "\n",
      "Points above this boundary typically fail the minimum coverage criterion.\n",
      "Consider avoiding this region or reducing the 'min_fraction' parameter."
    )
  }
  
  loss_grid
}


# OPTIMAL PARAMETERS SELECTION

find_best_parameters <- function(loss_grid) {
  
  finite <- which(is.finite(loss_grid$loss))
  
  if (length(finite) == 0) {
    return(list(
      best_scale = NA_real_,
      best_shift = NA_real_,
      best_loss  = Inf
    ))
  }
  
  idx <- finite[which.min(loss_grid$loss[finite])]
  
  list(
    best_scale = loss_grid$scale[idx],
    best_shift = loss_grid$shift[idx],
    best_loss  = loss_grid$loss[idx]
  )
}

# OPTIMAL CURVE PREDICTION

predict_best_curve <- function(su_grid, df_target, best_scale, best_shift) {
  
  C_grid <- su_grid$concentration
  I_grid <- su_grid$intensity
  C_t    <- df_target$concentration
  
  C_trans <- C_grid / best_scale - best_shift
  approx(x = C_trans, y = I_grid, xout = C_t, rule = 1)$y
}


# PARABOLOID AND HESSIAN MATRIX

fit_local_paraboloid <- function(loss_grid, best_scale, best_shift,
                                 best_loss, local_scale_win, local_shift_win) {
  
  local <- loss_grid %>%
    dplyr::filter(
      is.finite(loss),
      scale >= best_scale - local_scale_win,
      scale <= best_scale + local_scale_win,
      shift >= best_shift - local_shift_win,
      shift <= best_shift + local_shift_win
    )
  
  if (nrow(local) < 10) return(NULL)
  
  x1 <- local$scale - best_scale
  x2 <- local$shift - best_shift
  y  <- local$loss  - best_loss
  
  model <- try(
    lm(y ~ I(x1^2) + I(x2^2) + I(x1*x2) - 1),
    silent = TRUE
  )
  
  if (inherits(model, "try-error")) return(NULL)
  
  cf <- coef(model)
  
  a <- cf["I(x1^2)"]
  b <- cf["I(x2^2)"]
  c <- cf["I(x1 * x2)"]
  
  M <- matrix(c(a, c/2, c/2, b), nrow = 2, byrow = TRUE)
  
  eig <- eigen(M)
  lambda <- eig$values
  V      <- eig$vectors
  
  if (any(lambda <= 0) || best_loss <= 0) {
    return(list(a=a,b=b,c=c,M=M,eigenvalues=lambda,
                eigenvectors=V, semiaxes=NA,
                scale_error=NA, shift_error=NA))
  }
  
  semiaxes <- sqrt(best_loss / lambda)
  
  v1 <- V[,1]
  v2 <- V[,2]
  
  proj_scale <- sqrt((semiaxes[1] * v1[1])^2 + (semiaxes[2] * v2[1])^2)
  proj_shift <- sqrt((semiaxes[1] * v1[2])^2 + (semiaxes[2] * v2[2])^2)
  
  list(
    a=a, b=b, c=c,
    M=M,
    eigenvalues=lambda,
    eigenvectors=V,
    semiaxes=semiaxes,
    scale_error=proj_scale,
    shift_error=proj_shift,
    loss_threshold = 2 * best_loss
  )
}









