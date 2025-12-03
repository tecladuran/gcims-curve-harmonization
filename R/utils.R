analyte_names <- c(
  anisole   = "Anisole",
  heptanone = "2-Heptanone",
  pentanone = "2-Pentanone"
)

display_name <- function(x) {
  if (x %in% names(analyte_names)) {
    analyte_names[[x]]
  } else {
    x
  }
}