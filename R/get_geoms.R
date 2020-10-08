#' Get spatial polygons for specified geography
#'
#' @param geography A dataframe
#' @param state A variable in the dataframe
#' @return The dataframe with new mean and sum columns

#' @export

get_geoms <- function(geography, state) {

    # Get geometry
    geometry = TRUE
    dat <-
        suppressMessages(purrr::map(state, ~ tidycensus::get_acs(geography = geography,
                                              state=.x,
                                              year= 2015,
                                              geometry = geometry,
                                              variables="B01003_001")))

    # Store geometries, faster to drop here and use rbindlist below, then rejoin
    GEOID <- NULL
    geoms <- purrr::map(dat, function(x) unique(dplyr::select(x, GEOID, geometry)))

    col_order <- c("GEOID", "geometry")
    split_ind <-
        purrr::map_lgl(geoms, ~ any(st_is(.x, c("MULTIPOLYGON"))))

    # Empty Geometries are gettign dropped here
    split_fixed <-
        purrr::map(geoms[split_ind], ~ st_cast(.x, to="POLYGON", warn=FALSE))

    tract_geoms <-
        sf::st_as_sf(data.table::rbindlist(c(split_fixed, geoms[!split_ind])))

    return(tract_geoms)
}
