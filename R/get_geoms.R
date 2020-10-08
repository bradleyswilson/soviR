get_geoms <- function(geography, state) {

    # Get geometry
    dat <-
        suppressMessages(map(state, ~ get_acs(geography = geography,
                                              state=.x,
                                              year= 2015,
                                              geometry = TRUE,
                                              variables="B01003_001")))

    # Store geometries, faster to drop here and use rbindlist below, then rejoin
    geoms <- map(dat, function(x) unique(dplyr::select(x, GEOID, geometry)))

    col_order <- c("GEOID", "geometry")
    split_ind <-
        geoms %>%
        map_lgl(~ any(st_is(.x, c("MULTIPOLYGON"))))

    # Empty Geometries are gettign dropped here
    split_fixed <-
        geoms[split_ind] %>%
        map(~ st_cast(.x, to="POLYGON", warn=FALSE))

    tract_geoms <-
        st_as_sf(data.table::rbindlist(c(split_fixed, geoms[!split_ind])))

    return(tract_geoms)
}
