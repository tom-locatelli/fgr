#'Example "long" input dataset for the \code{fg_tmc} function
#'
#'A dataset used for the examples on how to use the \emph{TMC} method in the \emph{forestgales} package.
#' It contains data for most of the input variables of the \code{fg_tmc} function.
#' Variables with \emph{NAs} default automatically to a species-specific value, or to a constant.
#' Some variables are Essential, others are Desirable, and quite a few are Advanced. See the \code{Wrapper Functions} documentation for more information on inputs.
#' The variables are as follows:
#'
#' \itemize{
#'   \item{stand_id.}{ We think it's useful to tag stands for batch simulations. Essential.}
#'   \item{tree_id.}{ We think it's useful to tag trees for batch simulations. Essential.}
#'   \item{species.}{  Vulnerability is to a large extent species-specific. Note that the species acronyms need to be wrapped in inverted commas (e.g. "SP" for Scots pine). Essential.}
#'   \item{tree_ht.}{ Tree height (m) is the main driver of vulnerability. Essential.}
#'   \item{dbh.}{ Diameter at Breast Height (cm). Dbh is the second main driver of vulnerability. Essential.}
#'   \item{spacing_current.}{ Current average spacing (m) between trees. When modelling thinnings, this is the spacing \emph{after} thinning. Essential.}
#'   \item{stand_mean_ht.}{ The mean tree height (m) of the trees in the stand. Used to calculate the effect of edge and gap, and indirectly to elevate the critical wind speeds (CWS) to match anemometer data. Essential.}
#'   \item{stand_mean_dbh.}{ The mean dbh (cm) of the trees in the stand. Used indirectly to elevate the critical wind speeds to match anemometer data. Essential.}
#'   \item{predominant_species.}{ Required to calculate \code{stand_top_ht} and/or \code{stand_mean_ht}. Note that the species acronyms need to be wrapped in inverted commas (e.g. "SP" for Scots pine). Essential.}
#'   \item{weib_a.}{ Scale parameter (A, m s-1) of the Weibull distribution of mean hourly wind speeds. This parameter is proportional to the mean wind speed. Only used to calculate probabilities of damage, not the CWS. In the UK, if \code{weib_a} is not available it can be calculated from DAMS scores (see the \code{Wrapper Functions} documentation).}
#'   \item{weib_k.}{ Shape parameter (k, dimensionless) of the Weibull distribution of mean hourly wind speeds. This parameter (typically ranging between 1 and 3) is indicative of the variability of the mean wind speeds (small values indicating larger variability). Only used to calculate probabilities of damage, not the CWS. Defaults to 1.85.}
#'   \item{cr_width.}{ Individual tree crown width (m). Calculated from species-specific parameters if not provided. Desirable.}
#'   \item{cr_depth.}{ Individual tree crown depth (m). Calculated from species-specific parameters if not provided. Desirable.}
#'   \item{stand_cr_width.}{ Mean tree crown width (m). Calculated from species-specific parameters relative to \code{predominant_species} if not provided. Used to elevate the critical wind speeds to match anemometer data.Desirable.}
#'   \item{stand_cr_depth.}{ Mean tree crown depth (m). Calculated from species-specific parameters relative to \code{predominant_species} if not provided. Used to elevate the critical wind speeds to match anemometer data.Desirable.}
#'   \item{soil_group.}{ Follows the soil classification described in the \code{Critical Moments} documentation. If not provided it defaults to Soil A - Freely draining mineral soils. Desirable.}
#'   \item{rooting.}{ Categorised as: 1 - Shallow (<80cm); 2 - Deep (>80cm); 3 - The average of the Shallow and Deep values. Defaults to the Average value. Desirable.}
#'   \item{dist_edge.}{ Distance (m) between an individual tree and the upwind stand edge. It defaults to 9 times \code{stand_mean_ht}. Desirable.}
#'   \item{gap_size.}{ Size (m) of the upwind gap. Defaults to 0m (i.e. no gap), which effectively treats all trees as "middle of the stand" trees. Desirable.}
#'   \item{spacing_before.}{ Average spacing (m) between trees before a thinning. It defaults to \code{spacing_current}. Desirable.}
#'   \item{years_since_thin.}{ Years since the last thinning. It defaults to 5 years (e.g. assumes full acclimation to new wind climate). Desirable.}
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
#'   \item{ci.}{ Competition Index. Three options available: "None", "Bal", "Heg" (see \code{Turning coefficients functions} documentation). It defaults to "None". Advanced.}
#'   \item{ci_value.}{ Value of the Competition Index. It defaults to 0. Advanced.}
#'   \item{ro.}{ Air density (kg m-3). It defaults to 1.2226. Advanced.}
#'   \item{x, y, z.}{ Placeholders for tree coordinates. Advanced.}
#'   \item{dams.}{ Detailed Aspect Method of Scoring values for UK. Only used to calculate probabilities of damage, not the CWS. Advanced.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name df_tmc
#' @usage data(df_tmc)
#' @format A data frame with 8 rows and 42 variables
NULL
