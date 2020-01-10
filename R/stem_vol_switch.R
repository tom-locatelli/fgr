#' Acts as a switch to select the appropriate function to calculate the stem volume of a tree.
#' @title Stem Volume Function.
#' @param species Tree species. If working with a non-default species (i.e. named "U^"), the stem_volume_fonweban function to calulate volume is adopted.
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm). Depending on the method used ('roughness' or 'TMC') this can be either the arithmetic average of the dbh of all the trees in the stand, or the dbh of an individual tree.
#' @param ht Tree height. Depending on the method used ('roughness' or 'TMC'), this can be either the mean tree in the stand, or each individual tree (m).
#' @return The volume of the stem (m3).
stem_vol_fun <- function (species, dbh, ht, species_parameters) {
  if(species == "SP" | species == "SS" | species == "NS" | species == "CP" | species == "LP" | species == "EL" | species == "HL" | species == "DF" | species == "JL" |
     species == "NF" | species == "GF" | species == "WH" | species == "BE" | species == "OK" | species == "MP" | species == "RP" | species == "EG" | grepl("U", species)) { #MP=U1; RP=U2; EG=U3
    stem_vol <- stem_volume_fonweban(dbh, ht, species, species_parameters)
  }
  if(species == "JLJ") {
    stem_vol <- stem_volume_japanese_larch_japan(dbh, ht)
  }
  if(species == "WS" | species == "BS" | species == "BF" | species == "JP") {# WS=U4; BS=U5; BF=U6; JP=U7
    stem_vol <- stem_volume_quebec(dbh, ht, species)
  }
  if(species == "BI") {
    stem_vol <- stem_volume_laasasenaho(dbh, ht, species)
  }
  return(stem_vol)
}

stem_vol_fun2 <- function(species, dbh, ht){
  stem_vol <- ifelse(species == "SS", stem_volume_fonweban(dbh, ht, species), 1)
  return(stem_vol)
}
