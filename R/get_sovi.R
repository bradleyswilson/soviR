#' Get specified social vulnerability variables from the census
#'
#' @param geography A dataframe
#' @param state A variable in the dataframe
#' @param variables A variable in the dataframe
#' @param year A variable in the dataframe
#' @param geometry A variable in the dataframe
#'
#' @return The dataframe with new mean and sum columns

#' @export

get_sovi <- function(geography, state = NULL, variables = NULL, year = 2015, geometry = FALSE) {

    # Census API codes needed for each SOVI Variable
    var_dict <- list(total_pop = c("B01003_001"),
                     housing_units = c("B25001_001"),
                     medage_acs = c("B01002_001"),
                     qasian_acs = c("B03002_006", "B03002_001"),
                     qblack_acs = c("B03002_004", "B03002_001"),
                     qhisp_acs = c("B03002_012", "B03002_001"),
                     qnatam_acs = c("B03002_005", "B03002_001"),
                     qagedep_acs = c("B06001_002", "B09020_001", "B01003_001") ,
                     qfam = c("B09002_002", "B09002_001"),
                     qpunit_acs = c("B25008_001", "B25002_002"),
                     qrenter_acs = c("B25003_003", "B25002_001"),
                     qrentburden = c("B25070_007", "B25070_008", "B25070_009",
                                     "B25070_010", "B25070_001"),
                     qhouseburden = c("B25106_001","B25106_006", "B25106_010", "B25106_014",
                                      "B25106_018", "B25106_022", "B25106_028",
                                      "B25106_032", "B25106_036", "B25106_040",
                                      "B25106_044"),
                     qnrres_acs = c("B09020_021", "B01003_001"),
                     qfemale_acs = c("B01001_026", "B01003_001"),
                     qfhh_acs = c("B11001_006", "B11001_001"),
                     qunochhu_acs = c("B25002_003", "B25002_001"),
                     percap = c("B19025_001", "B25008_001"),
                     qesl = c("B06007_005", "B06007_008", "B06007_001"),
                     qcvlun = c("B23022_025", "B23022_049", "B23022_001"),
                     qpovty = c("B17021_002", "B17021_001"),
                     qmoho = c("B25024_010", "B25024_001"),
                     qed12les_alt = c("B16010_002", "B16010_001"),
                     qfemlbr = c("C24010_038", "C24010_001"),
                     qextrct_alt = c("C24050_002", "C24050_001"),
                     qserv_alt = c("C24050_029", "C24050_001"),
                     qssben = c("B19055_002", "B19055_001"),
                     qnoauto_alt = c("B25044_003", "B25044_010", "B25044_001"),
                     qrich200k = c("B19001_017", "B11001_001"),
                     mdgrent_alt = c("B25064_001"),
                     mhseval_alt = c("B25077_001"))

    # Calculation formulas
    var_fms <- list(c("GEOID = dat$GEOID"),
                    c("total_pop = dat$estimate_B01003_001"),
                    c("housing_units = dat$estimate_B25001_001"),
                    c("medage_acs = dat$estimate_B01002_001"),
                    c("qasian_acs = dat$estimate_B03002_006 / (dat$estimate_B03002_001)"),
                    c("qblack_acs = dat$estimate_B03002_004 / (dat$estimate_B03002_001)"),
                    c("qhisp_acs = dat$estimate_B03002_012 / (dat$estimate_B03002_001)"),
                    c("qnatam_acs = dat$estimate_B03002_005 / (dat$estimate_B03002_001)"),
                    c("qagedep_acs = (dat$estimate_B06001_002 + dat$estimate_B09020_001) / (dat$estimate_B01003_001)"),
                    c("qfam = (dat$estimate_B09002_002) / dat$estimate_B09002_001"),
                    c("qpunit_acs = dat$estimate_B25008_001 / (dat$estimate_B25002_002)"),
                    c("qrenter_acs = dat$estimate_B25003_003 / (dat$estimate_B25002_001)"),
                    c("qrentburden = (dat$estimate_B25070_007 + dat$estimate_B25070_008 +
                      dat$estimate_B25070_009 + dat$estimate_B25070_010) / dat$estimate_B25070_001"),
                    c("qhouseburden = (dat$estimate_B25106_006 + dat$estimate_B25106_010 +
                      dat$estimate_B25106_014 + dat$estimate_B25106_018 + dat$estimate_B25106_022 +
                      dat$estimate_B25106_028 + dat$estimate_B25106_032 + dat$estimate_B25106_036 +
                      dat$estimate_B25106_040 + dat$estimate_B25106_044) / dat$estimate_B25106_001"),
                    c("qnrres_acs = dat$estimate_B09020_021 / (dat$estimate_B01003_001)"),
                    c("qfemale_acs = dat$estimate_B01001_026 / (dat$estimate_B01003_001)"),
                    c("qfhh_acs = dat$estimate_B11001_006 / (dat$estimate_B11001_001)"),
                    c("qunochhu_acs = dat$estimate_B25002_003 / (dat$estimate_B25002_001)"),
                    c("percap = dat$estimate_B19025_001 / (dat$estimate_B25008_001)"),
                    c("qesl = ((dat$estimate_B06007_005 + dat$estimate_B06007_008)) / dat$estimate_B06007_001"),
                    c("qcvlun = ((dat$estimate_B23022_025 + dat$estimate_B23022_049)) /dat$estimate_B23022_001"),
                    c("qpovty = (dat$estimate_B17021_002) / dat$estimate_B17021_001"),
                    c("qmoho = (dat$estimate_B25024_010) / dat$estimate_B25024_001"),
                    c("qed12les_alt = (dat$estimate_B16010_002) / dat$estimate_B16010_001"),
                    c("qfemlbr = (dat$estimate_C24010_038) / dat$estimate_C24010_001"),
                    c("qextrct_alt = (dat$estimate_C24050_002) / dat$estimate_C24050_001"),
                    c("qserv_alt = (dat$estimate_C24050_029) / dat$estimate_C24050_001"),
                    c("qssben = (dat$estimate_B19055_002) / dat$estimate_B19055_001"),
                    c("qnoauto_alt =  ((dat$estimate_B25044_003 + dat$estimate_B25044_010)) / dat$estimate_B25044_001"),
                    c("qrich200k = (dat$estimate_B19001_017) / dat$estimate_B11001_001"),
                    c("mdgrent_alt = dat$estimate_B25064_001"),
                    c("mhseval_alt = dat$estimate_B25077_001"))

    # Create string of variables for API call
    var_loc <- which(names(var_dict) %in% variables)
    var_api <- paste(unlist(var_dict[var_loc]), sep=",")

    # Get raw data using tidy census
    tmp <-
        suppressMessages(purrr::map(state, ~ tidycensus::get_acs(geography = geography,
                             state=.x,
                             year= year,
                             geometrygeometry = geometry,
                             variables = var_api)))

    variable <- estimate <- moe <- NULL
    dat <-
        tidyr::pivot_wider(dplyr::bind_rows(tmp),
                           names_from = variable,
                           values_from = c(estimate, moe))

    # Calculate sovi variables from raw data
    dat_sovi <-
        purrr::map_dfc(var_fms[c(1,var_loc+1)], ~ tibble(!!parse_expr(.x)))

    names(dat_sovi) <- c("GEOID", variables)

    return(dat_sovi)
}
