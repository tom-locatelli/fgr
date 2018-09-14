#' Calculate the critical moments for breakage and overturning
#' @param dbh The dbh (cm) of a tree.
#' @param ht The height (m) of a tree.
#' @param cr_depth The length (m) of the tree crown.
#' @param mor Modulus of Rupture (MPa) of green wood.
#' @param fknot Knot factor. Dimensionless.
#' @param c_reg Regression coefficients (N m kg-1) of uprooting moment against stem weight.
#' @param stem_density Density (kg m-3) of green wood of the stem.
#' @param stem_vol Volume (m3) of the stem of the mean tree in the stand.
#' @name Critical_Moments_Functions
#' @title Critical Moments Functions
NULL

#' @rdname Critical_Moments_Functions
critical_moment_breakage <- function(dbh, ht, cr_depth, mor, fknot) {
  #calculate dbase:
  dbase <- diam_base_fun(dbh, ht, cr_depth) #dbase and dbh in cm in this formula
  breaking_moment <- mor * fknot * pi * ((dbase/100)^3) / 32 #note dbase in meters!
  return(breaking_moment)
}

#' @rdname Critical_Moments_Functions
critical_moment_overturning <- function(c_reg, stem_density, stem_vol) {
  overturning_moment <- c_reg * stem_density * stem_vol
  return(overturning_moment)
}
