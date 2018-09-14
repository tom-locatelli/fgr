#' Convert top to mean tree height (and viceversa) and calculate crown dimensions and diameter at base of the tree.
#' @param dbh The dbh (cm) of a tree.
#' @param ht The height (m) of a tree.
#' @param cr_depth The length (m) of the tree crown.
#' @param top_ht Dominant tree height in the stand.
#' @param mean_ht Mean tree height in the stand.
#' @param param0_height Species-specific parameter for top/mean tree height calculation.
#' @param param1_height Species-specific parameter for top/mean tree height calculation.
#' @param param0_cr_width Species-specific parameter for crown width calculation.
#' @param param1_cr_width Species-specific parameter for crown width calculation.
#' @param param0_cr_depth Species-specific parameter for crown length calculation.
#' @param param1_cr_depth Species-specific parameter for crown length calculation.
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
