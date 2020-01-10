#' Convert top to mean tree height (and viceversa), calculate crown dimensions (width and depth) from dbh and tree height, calculate diameter at base of the tree for Deflection Loading Factor, and calculate "equivalent mean height" from stand top height for the TMC method.
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm). Depending on the method used ('roughness' or 'TMC') this can be either the arithmetic average of the dbh of all the trees in the stand, or the dbh of an individual tree.
#' @param ht Tree height. Depending on the method used ('roughness' or 'TMC'), this can be either the mean tree in the stand, or each individual tree (m).
#' @param cr_depth Length of the tree crown (m).
#' @param top_ht Dominant tree height in the stand (m).
#' @param mean_ht Mean tree height in the stand. For the TMC method, arithmetic mean height of the trees in the stand (m).
#' @param equivalent_mean_ht Height of stand-level wind moment absorption hypothesised for the TMC method. Calculated from stand top height using a linear relationship derived from the mean height - top height regressions values across all tree species currently included in the model.
#' @param param0_height Species-specific parameter for conversions between mean height and top height. Offset in linear regression equation.
#' @param param1_height Species-specific parameter for conversions between mean height and top height. Multiplier in linear regression equation.
#' @param param0_cr_width Species-specific parameter for calculation of crown width from dbh. Offset in linear regression equation.
#' @param param1_cr_width Species-specific parameter for calculation of crown width from dbh. Multiplier in linear regression equation.
#' @param param0_cr_depth Species-specific parameter for calculation of crown length from tree height. Offset in linear regression equation.
#' @param param1_cr_depth Species-specific parameter for calculation of crown length from tree height. Multiplier in linear regression equation.
#' @name Tree_Dimensions_Functions
#' @title Tree Dimensions Functions
NULL

#' @rdname Tree_Dimensions_Functions
top_ht_to_mean_ht <- function(param0_height, param1_height, top_ht){
  mean_ht <- param0_height + param1_height *top_ht
  return(mean_ht)
}

#' @rdname Tree_Dimensions_Functions
mean_ht_to_top_ht <- function(param0_height, param1_height, mean_ht){
  top_ht <- (mean_ht - param0_height) / param1_height
  return(top_ht)
}

#' @rdname Tree_Dimensions_Functions
canopy_width_fun <- function(param0_cr_width, param1_cr_width, dbh){
  crown_width <- param0_cr_width + param1_cr_width * dbh
  return(crown_width)
}

#' @rdname Tree_Dimensions_Functions
canopy_depth_fun <- function(param0_cr_depth, param1_cr_depth, ht){
  crown_depth <- param0_cr_depth + param1_cr_depth * ht
  return(crown_depth)
}

#' @rdname Tree_Dimensions_Functions
diam_base_fun <- function(dbh, ht, cr_depth) {
  wind_action_point <- ht - (cr_depth/2)
  if(wind_action_point < 1.3*2) {wind_action_point <- 1.3*2}
  dbase <- (dbh / (wind_action_point - 1.3)^0.333) * wind_action_point^0.333 #dbase and dbh in cm in this formula
  return(dbase)
}

#' @rdname Equivalent_Mean_Height
eq_mean_ht_fun <- function(top_ht) {
  equivalent_mean_ht <- 1.03 * top_ht - 1.8
  return(equivalent_mean_ht)
}
