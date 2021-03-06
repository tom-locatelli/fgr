#'Example "short" input dataset for the \code{fg_tmc} function
#'
#'A dataset used for the examples on how to use the \emph{TMC} method in the \emph{forestgales} package.
#' It contains data for only the Essential input variables of the \code{fg_tmc} function (see the \code{Wrapper Functions} documentation).
#' Non-essential (i.e. Desirable, or Advanced) variables are populated with \emph{NAs} as they will default automatically to a species-specific value, or to a constant.
#' The variables are as follows:
#'
#' \itemize{
#'   \item{stand_id.}{ We think it's useful to tag stands for batch simulations. Essential.}
#'   \item{tree_id.}{ We think it's useful to tag trees for batch simulations. Essential.}
#'   \item{species.}{ Vulnerability is to a large extent species-specific. Note that the species acronyms need to be wrapped in inverted commas (e.g. "SP" for Scots pine). Essential.}
#'   \item{tree_ht.}{ Tree height is the main driver of vulnerability (m). Essential.}
#'   \item{dbh.}{ Diameter at Breast Height (cm). Dbh is the second main driver of vulnerability. Essential.}
#'   \item{spacing_current.}{ Current average spacing of trees (m). When modelling thinnings, this is the spacing \emph{after} thinning. Essential.}
#'   \item{stand_mean_ht.}{ The mean tree height of the trees in the stand (m). Used to calculate the effect of edge and gap, and indirectly to elevate the critical wind speeds (CWS) to match anemometer data. Essential.}
#'   \item{stand_mean_dbh.}{ The mean dbh of the trees in the stand (cm). Used indirectly to elevate the critical wind speeds to match anemometer data. Essential.}
#'   \item{predominant_species.}{ Required to calculate \code{stand_top_ht} and/or \code{stand_mean_ht}. Note that the species acronyms need to be wrapped in inverted commas (e.g. "SP" for Scots pine). Essential.}
#'   \item{weib_a.}{ \strong{Populated with \emph{NAs}}. Scale parameter (A) of the Weibull distribution of mean hourly wind speeds. This parameter is proportional to the mean wind speed. Only used to calculate probabilities of damage, not the CWS. In the UK, if \code{weib_a} is not available it can be calculated from DAMS scores (see the \code{Wrapper Functions} documentation).}
#'   \item{weib_k.}{ \strong{Populated with \emph{NAs}}. Shape parameter (k) of the Weibull distribution of mean hourly wind speeds. This parameter (typically ranging between 1 and 3) is indicative of the variability of the mean wind speeds (small values indicating larger variability). Only used to calculate probabilities of damage, not the CWS. Defaults to 1.85.}
#'   \item{cr_width.}{ \strong{Populated with \emph{NAs}}. Individual tree crown width (m). Calculated from species-specific parameters if not provided. Desirable.}
#'   \item{cr_depth.}{ \strong{Populated with \emph{NAs}}. Individual tree crown depth (m). Calculated from species-specific parameters if not provided. Desirable.}
#'   \item{stand_cr_width.}{ \strong{Populated with \emph{NAs}}. Mean tree crown width (m). Calculated from species-specific parameters relative to \code{predominant_species} if not provided. Used to elevate the critical wind speeds to match anemometer data. Desirable.}
#'   \item{stand_cr_depth.}{ \strong{Populated with \emph{NAs}}. Mean tree crown depth (m). Calculated from species-specific parameters relative to \code{predominant_species} if not provided. Used to elevate the critical wind speeds to match anemometer data. Desirable.}
#'   \item{soil_group.}{ \strong{Populated with \emph{NAs}}. Follows the soil classification described in the \code{Critical Moments} documentation. If not provided it defaults to Soil A - Freely draining mineral soils. Desirable.}
#'   \item{rooting.}{ \strong{Populated with \emph{NAs}}. Categorised as: 1 - Shallow (<80cm); 2 - Deep (>80cm); 3 - The average of the Shallow and Deep values. Defaults to the Average value. Desirable.}
#'   \item{dist_edge.}{ \strong{Populated with \emph{NAs}}. Distance of an individual tree from the upwind stand edge (m). It defaults to 9 times \code{stand_mean_ht}. Desirable.}
#'   \item{gap_size.}{ \strong{Populated with \emph{NAs}}. Size of the upwind gap (m). Defaults to 0m (i.e. no gap), which effectively treats all trees as "middle of the stand" trees. Desirable.}
#'   \item{spacing_before.}{ \strong{Populated with \emph{NAs}}. Average spacing of trees before a thinning (m). It defaults to \code{spacing_current}. Desirable.}
#'   \item{years_since_thin.}{ \strong{Populated with \emph{NAs}}. Years since the last thinning. It defaults to 5 years (e.g. assumes full acclimation to new wind climate). Desirable.}
#'   \item{moe.}{ \strong{Populated with \emph{NAs}}. Modulus of Elasticity (Young's modulus) of green wood (MPa). Defaults to species-specific values unless provided. Advanced.}
#'   \item{mor.}{ \strong{Populated with \emph{NAs}}. Modulus of Rupture of green wood (MPa). Defaults to species-specific values unless provided. Advanced.}
#'   \item{fknot.}{ \strong{Populated with \emph{NAs}}. Knot factor, scaled (0, 1] (dimensionless). It defaults to species-specific values unless provided. Advanced.}
#'   \item{stem_vol.}{ \strong{Populated with \emph{NAs}}. Volume of the stem (m3). Calculated with species-specific formulas and parameters unless provided. Advanced.}
#'   \item{crown_vol.}{ \strong{Populated with \emph{NAs}}. Volume of the crown (m3). Calculated with species-specific formulas and parameters unless provided. Advanced.}
#'   \item{stem_density.}{ \strong{Populated with \emph{NAs}}. Density of the green wood of the stem (kg m-3). It defaults to species-specific values unless provided. Advanced.}
#'   \item{crown_density.}{ \strong{Populated with \emph{NAs}}. Density of the crown (kg m-3). It defaults to species-specific values unless provided. Advanced.}
#'   \item{c_reg.}{ \strong{Populated with \emph{NAs}}. Regression coefficient of the critical overturning moment \emph{vs} stem weight. It defaults to species-specific values for the chosen \code{soil_group} \emph{x} \code{rooting} depth combination unless provided. Advanced.}
#'   \item{c_drag.}{ \strong{Populated with \emph{NAs}}. Vvalue of the drag coefficient at rest. See the \code{Streamlining} documentation. It defaults to species-specific values unless provided. Advanced.}
#'   \item{n_drag.}{ \strong{Populated with \emph{NAs}}. Exponent of the power fit of drag to wind tunnel data. See the \code{Streamlining} documentation. It defaults to species-specific values unless provided. Advanced.}
#'   \item{drag_upper_limit.}{ \strong{Populated with \emph{NAs}}. Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1). It defaults to species-specific values unless provided. Advanced.}
#'   \item{snow_depth.}{ \strong{Populated with \emph{NAs}}. Depth of snow layer on the canopy (m3). It defaults to 0m (no snow). Advanced.}
#'   \item{snow_density.}{ \strong{Populated with \emph{NAs}}. Density of snow (kg m-3). It defaults to 150 kg m-3. Advanced.}
#'   \item{ci.}{ \strong{Populated with \emph{NAs}}. Competition Index. Three options available: "None", "Bal", "Heg" (see \code{Turning coefficients functions} documentation). It defaults to "None". Advanced.}
#'   \item{ci_value.}{ \strong{Populated with \emph{NAs}}. Value of the Competition Index. It defaults to 0. Advanced.}
#'   \item{ro.}{ \strong{Populated with \emph{NAs}}. Air density (kg m-3). It defaults to 1.2226. Advanced.}
#'   \item{x, y, z.}{ \strong{Populated with \emph{NAs}}. Placeholders for tree coordinates. Advanced.}
#'   \item{dams.}{ \strong{Populated with \emph{NAs}}. DAMS (Detailed Aspect Method of Scoring) value to describe the local wind climate (for UK conditions only). Only used to calculate probabilities of damage, not the CWS. Advanced.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name df_tmc_2
#' @usage data(df_tmc_2)
#' @format A data frame with 8 rows and 42 variables
NULL
