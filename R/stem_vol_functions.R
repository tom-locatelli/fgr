#' Calculates the stem volume of a tree.
#' @title Stem Volume Function.
#' @param species Tree species.
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm). Depending on the method used ('roughness' or 'TMC') this can be either the arithmetic average of the dbh of all the trees in the stand, or the dbh of an individual tree.
#' @param ht Tree height. Depending on the method used ('roughness' or 'TMC'), this can be either the mean tree in the stand, or each individual tree (m).
#' @return The volume of the stem (m3). Applies the method in Fonweban et al. (2012) to calculate stem volume.
stem_volume_fonweban <- function(dbh, ht, species, species_parameters){ #From Fonweban et al. 2012. Eq 4
  param0_tvf <- species_parameters[species, "param0_vol"] #get(paste0("param0_tvf_", species))
  param1_tvf <- species_parameters[species, "param1_vol"] #get(paste0("param1_tvf_", species))
  param2_tvf <- species_parameters[species, "param2_vol"] #get(paste0("param2_tvf_", species))
  stem_vol <- param0_tvf * (dbh^param1_tvf * ht^param2_tvf)
  return(stem_vol)
}

#' @describeIn stem_volume_fonweban Applies the method in Honer (1967) to calculate stem volume.
stem_volume_quebec <- function(dbh, ht, species) { #From JC Ruel
  param0_tvq <- species_parameters[species, "param0_vol"] #get(paste0("param0_tvq_", species))
  param1_tvq <- species_parameters[species, "param1_vol"] #get(paste0("param1_tvq_", species))
  param2_tvq <- species_parameters[species, "param2_vol"] #get(paste0("param2_tvq_", species))
  stem_vol <- 0.004389*dbh^2 * (1- 0.04365*param0_tvq)^2 / (param1_tvq + 0.3048*param2_tvq/ht)
  return(stem_vol)
}

#' @describeIn stem_volume_fonweban Applies the method in Laasasenaho (1982) to calculate stem volume.
stem_volume_laasasenaho <- function(dbh, ht, species) {
  param0_tvl <- species_parameters[species, "param0_vol"]
  param1_tvl <- species_parameters[species, "param1_vol"]
  param2_tvl <- species_parameters[species, "param2_vol"]
  param3_tvl <- species_parameters[species, "param3_vol"]
  param4_tvl <- species_parameters[species, "param4_vol"]
  stem_vol <- (dbh^param1_tvl * ht^param2_tvl * (ht - 1.3)^param3_tvl * exp(param0_tvl + param4_tvl*dbh)) / 1000
  return(stem_vol)
}

#' @describeIn stem_volume_fonweban Calculates stem volume for Japanese Larch grown in Japan.
stem_volume_japanese_larch_japan <- function(dbh, ht) { #parameters for this Volume function need to be kept here rather than in the species_parameters df because
  if (dbh < 11) {                                           #they depend on the value of dbh, and as such they need to be evaluated
    param1_tvjl <- 0.77430
    param2_tvjl <- 1.87385
    param3_tvjl <- 0.94852
    param4_tvjl <- 1.0026
  } else if (dbh < 21) {
    param1_tvjl <- 0.58495
    param2_tvjl <- 1.96416
    param3_tvjl <- 1.04523
    param4_tvjl <- 1.0027
  } else if (dbh < 31) {
    param1_tvjl <- 0.67205
    param2_tvjl <- 1.84173
    param3_tvjl <- 1.11080
    param4_tvjl <- 1.0012
  } else {
    param1_tvjl <- 0.79071
    param2_tvjl <- 1.74034
    param3_tvjl <- 1.13316
    param4_tvjl <- 1.0016
  }
  stem_vol <- param4_tvjl * 10^(-5 + param1_tvjl + param2_tvjl*log10(dbh) + param3_tvjl*log10(ht))
  return(stem_vol)
}
