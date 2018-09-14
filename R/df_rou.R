#'Example input dataset for the \code{fg_rou} function
#'
#'A dataset used for the examples on how to use the \emph{roughness} method in the \emph{forestgales} package.
#' It contains data for the input variables of the \code{fg_rou} function.
#' Variables with \emph{NAs} default automatically to a species-specific value, or to a constant.
#' Some variables are Essential, others are Desirable, and quite a few are Advanced. See the \code{Wrapper Functions} documentation for more information on inputs.
#' The variables are as follows:
#'
#' \itemize{
#'   \item{stand_id.}{ We think it's useful to tag stands for batch simulations. Essential.}
#'   \item{species.}{  Vulnerability is to a large extent species-specific. Note that the species acronyms need to be wrapped in inverted commas (e.g. "SP" for Scots pine). Essential.}
#'   \item{mean_ht.}{ Mean tree height (m). Tree height is the main driver of vulnerability. Essential unless \code{top_ht} is provided.}
#'   \item{mean_dbh.}{ Mean Diameter at Breast Height (cm). Dbh is the second main driver of vulnerability. Essential.}
#'   \item{spacing.}{ Current average spacing (m) between trees. Essential.}
#'   \item{weib_a.}{ Scale parameter (A, m s-1) of the Weibull distribution of mean hourly wind speeds. This parameter is proportional to the mean wind speed. Only used to calculate probabilities of damage, not the CWS. In the UK, if \code{weib_a} is not available it can be calculated from DAMS scores (see the \code{Wrapper Functions} documentation).}
#'   \item{weib_k.}{ Shape parameter (k, dimensionless) of the Weibull distribution of mean hourly wind speeds. This parameter (typically ranging between 1 and 3) is indicative of the variability of the mean wind speeds (small values indicating larger variability). Only used to calculate probabilities of damage, not the CWS. Defaults to 1.85.}
#'   \item{top_ht.}{ Stand top height (m). If \code{mean_ht} is not provided, it can be calculated from \code{top_ht}. Desirable unless \code{mean_ht} is not available.}
#'   \item{mean_cr_width.}{ Mean tree crown width (m). Calculated from species-specific parameters relative to \code{species} if not provided. Desirable.}
#'   \item{mean_cr_depth.}{ Mean tree crown depth (m). Calculated from species-specific parameters relative to \code{species} if not provided. Desirable.}
#'   \item{soil_group.}{ Follows the soil classification described in the \code{Critical Moments} documentation. If not provided it defaults to Soil A - Freely draining mineral soils. Desirable.}
#'   \item{rooting.}{ Categorised as: 1 - Shallow (<80cm); 2 - Deep (>80cm); 3 - The average of the Shallow and Deep values. Defaults to the Average value. Desirable.}
#'   \item{new_edge.}{ Differentiate between an established - \emph{green} - edge (\code{new_edge} = 0) and a new - \emph{brown} edge (\code{new_edge} = 1). It defaults to 0 (i.e. \emph{green} edge). Desirable.}
#'   \item{gap_size.}{ Size (m) of the upwind gap. Defaults to 0m (i.e. no gap), which effectively treats the edge as a \emph{green} edge. Desirable.}
#'   \item{moe.}{ Modulus of Elasticity (Young's modulus, Pa) of green wood. Defaults to species-specific values unless provided. Advanced.}
#'   \item{mor.}{ Modulus of Rupture (Pa) of green wood. Defaults to species-specific values unless provided. Advanced.}
#'   \item{fknot.}{ Knot factor (dimensionless), scaled (0, 1]. It defaults to species-specific values unless provided. Advanced.}
#'   \item{stem_vol.}{ Volume (m3) of the stem. Calculated with species-specific formulas and parameters unless provided. Advanced.}
#'   \item{crown_vol.}{ Volume (m3) of the crown. Calculated with species-specific formulas and parameters unless provided. Advanced.}
#'   \item{stem_density.}{ Density (kg m-3) of the green wood of the stem. It defaults to species-specific values unless provided. Advanced.}
#'   \item{crown_density.}{ Density (kg m-3) of the crown. It defaults to species-specific values unless provided. Advanced.}
#'   \item{c_reg.}{ Regression coefficient of the critical overturning moment \emph{vs} stem weight. It defaults to species-specific values for the chosen \code{soil_group} \emph{x} \code{rooting} depth combination unless provided. Advanced.}
#'   \item{c_drag.}{ The value (dimensionless) of the drag coefficient at rest. See the \code{Streamlining} documentation. It defaults to species-specific values unless provided. Advanced.}
#'   \item{n_drag.}{ The exponent of the power fit of drag to wind tunnel data. See the \code{Streamlining} documentation. It defaults to species-specific values unless provided. Advanced.}
#'   \item{drag_upper_limit.}{ The experimental wind speed limit (m s-1) used to calculate \code{c_drag} and \code{n_drag}. It defaults to species-specific values unless provided. Advanced.}
#'   \item{snow_depth.}{ Depth (m) of snow layer on the canopy. It defaults to 0m (no snow). Advanced.}
#'   \item{snow_density.}{ Density (kg m-3) of snow. It defaults to 150 kg m-3. Advanced.}
#'   \item{ro.}{ Air density (kg m-3). It defaults to 1.2226. Advanced.}
#'   \item{x, y, z.}{ Placeholders for stand (centroid) coordinates. Advanced.}
#'   \item{dams.}{ Detailed Aspect Method of Scoring values for UK. Only used to calculate probabilities of damage, not the CWS. Advanced.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name df_rou
#' @usage data(df_rou)
#' @format A data frame with 7 rows and 32 variables
NULL
