#' Calculate the turning moment coefficient.
#' @param dbh The dbh (cm) of a tree.
#' @param ht The height (m) of a tree.
#' @param ci Competition Index (\code{BAL}, \code{Hegyi}) used. Can be \code{None}.
#' @param ci_value Value of \code{ci}.
#' @name Turning_Moment_Coefficient_Functions
#' @title Turning Moment Coefficient Functions
NULL

#' @rdname Turning_Moment_Coefficient_Functions
tc_intercept_fun <- function(dbh, ht, ci, ci_value) {
  if (ci == "None") {
    tc_intercept <- -9.64 + 113.78*(dbh/100)^2 * ht #Note dbh in meters!
  } else if (ci == "Bal") {
    tc_intercept <- 40.58 - 0.5337*ci_value + 111.79*(dbh/100)^2 * ht - 0.776*(dbh/100)^2 * ht*ci_value #note dbh in meters! #Bal
  } else if (ci == "Heg") {
    tc_intercept <- 50.958 - 7.734*ci_value + 123.819*(dbh/100)^2 * ht - 26.535*(dbh/100)^2 * ht*ci_value #note dbh in meters! #Hegyi
  }
  if (tc_intercept == 0) {
    tc_intercept <- 9.64 + 113.78*(dbh/100)^2 * ht #note dbh in meters!
  }
  return(tc_intercept)
}

#' @rdname Turning_Moment_Coefficient_Functions
tc_zero_intercept_fun <- function(dbh, ht, ci, ci_value) {
  if(ci =="None") {
    tc_zero_intercept <- 111.91*(dbh/100)^2 * ht #note dbh in meters!
  } else if (ci == "Bal") {
    tc_zero_intercept <- 0.130*ci_value + 116.304*(dbh/100)^2 * ht - 0.617*(dbh/100)^2 *ht*ci_value #note dbh in meters! #Bal
  } else if (ci == "Heg") {
    tc_zero_intercept <- 3.86*ci_value + 124.252*(dbh/100)^2 * ht - 17.285*(dbh/100)^2 * ht*ci_value #note dbh in meters! #Hegyi
  }
  if (tc_zero_intercept ==0) {
    tc_zero_intercept <- 111.91 * (dbh/100)^2 * ht #note dbh in meters!
  }
  return(tc_zero_intercept)
}

#' @rdname Turning_Moment_Coefficient_Functions
tc_zero_intercept_new_bal_fun <- function (dbh, ht, ci, ci_value) {
  if(ci =="None") { # | ci_value == 0
    tc_zero_intercept_new_bal <- 111.91 * (dbh/100)^2 * ht #note dbh in meters!
  } else if (ci == "Bal") {
    tc_zero_intercept_new_bal <- 0.274*ci_value #Bal
  } else if (ci == "Heg") {
    tc_zero_intercept_new_bal <- 3.86*ci_value + 124.252*(dbh/100)^2 * ht - 17.285*(dbh/100)^2 * ht*ci_value #note dbh in meters! #Hegyi
  }
  if (tc_zero_intercept_new_bal ==0) {
    tc_zero_intercept_new_bal <- 111.91 * (dbh/100)^2 * ht
  }
  return(tc_zero_intercept_new_bal)
}
