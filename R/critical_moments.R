#' Calculate the critical moments for breakage and overturning
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm). Depending on the method used ('roughness' or 'TMC') this can be either the arithmetic average of the dbh of all the trees in the stand, or the dbh of an individual tree.
#' @param ht Tree height. Depending on the method used ('roughness' or 'TMC'), this can be either the mean tree in the stand, or each individual tree (m).
#' @param cr_depth Length of the tree crown (m).
#' @param mor Modulus of Rupture of green wood (MPa).
#' @param fknot Knot factor (dimensionless).
#' @param c_reg Regression coefficient of uprooting moment against stem weight (N m kg-1).
#' @param stem_density Density of green wood of the stem (kg m-3).
#' @param stem_vol Volume of the tree stem of the mean tree in the stand (m3). For the roughness method, this is stem volume of the mean tree. For the TMC method, this is individual tree stem voume.
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
