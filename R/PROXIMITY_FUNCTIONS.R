
# PROXIMITY SCORES, FACILITY COUNTS & DENSITY ####
# AND CUSTOM INDICATORS
########################################################################## #

## COUNTS NEARBY & COUNTS PER SQMI +  PROXIMITY SCORE (create it, to later aggregate it)
##
## TASKS:
##
## - ANNUALLY PRECALCULATE for FRS  (& MAYBE SUBSETS BY PROGRAM TYPE?)
##      - "FACILITY DENSITY SCORE" (#/SQMI) PER BLOCK GROUP IN US
##      - "PROXIMITY SCORE" PER BLOCK GROUP IN US
## - ON-DEMAND-PRECALCULATED AFTER USER REQUEST (FOR USER PROVIDED FEATURES): 
##      - "site density score" per bg?? in US (#/sqmi within a large raster cell or within x miles of grid of points?? or per block or bg?)
##      - "PROXIMITY SCORE" per bg in US (just like RMP,TSDF,etc.)
## - ON-THE-FLY REPORTING OF 
##      - COUNTS NEARBY/IN each analyzed point/polygon OF CUSTOM FEATURES (FOR USER PROVIDED FEATURES in or near selected sites). (not same as count x miles from each resident who is within y miles of pt.)
##      - site density (#/sqmi in or near their bgs?) for avg person nearby
##      - a custom density or proximity score created already


## functions to create quadtree index of custom points

# R/utils_create_quaddata.R had  create_quaddata()
#
# R/utils_indexpoints.R     had  indexpoints()
# R/utils_indexfrs.R        had  indexfrs()

## functions to get distance to each

# R/getpointsnearby.R    had getpointsnearby()
# R/getfrsnearby.R       had getfrsnearby()

## functions to count pts nearby or inside, maybe based on distance to each

# R/countpoints_xyz-DRAFT.R  had
#  countpoints_after_getpoints()
#  and not yet written: 
#  countpoints_nearby()
#  countpoints_in_shape()
#  countpoints_in_fips()


## functions to CREATE (TO later use/aggreg.) PROXIMITY SCORE FOR EVERY US BLOCKGROUP (annually or maybe on demand)

# R/proxistat_via_getblocks.R  had proxistat_via_getblocks()
# R/proxistat.R  had proxistat()


# functions to do AGGREGATION  AND/OR FOR CUSTOM INDICATOR CREATION?

# R/utils_calc_ejam.R   (left in that file)
# has
# calc_ejam()
# calc_byformula()
# formula_varname()

# R/doaggregate_newscores.R had  doaggregate_newscores()

# R/custom_doaggregate.R  had  custom_doaggregate() and
# custom_ejamit()
# calc_bgwts_bysite
# calc_bgwts_overall
# calcweight


## CUSTOM PERCENTILE LOOKUP  AND  USING A LOOKUP AFTER AGGREGATION

# R/utils_pctiles_lookup_create.R  has  pctiles_lookup_create() and helpers to create the lookup table annually or on demand for a custom indicator.

# R/pctile_from_raw_lookup.R is the function to use a lookup table
# R/pctiles_from_raw_lookup_DRAFT.R  has notes on possible faster way
###################################################### # 
# ~ ----------------------------------------------------------------- ####


#' Utility to create a table used to create a quadtree spatial index of points etc.
#'
#' @description Used by [indexpoints()] that in turn is used by [proxistat()].
#'   It prepares a set of coordinates ready for indexing.
#' @details  Very similar to what is used to prepare [quaddata],
#'   which is used by [getblocksnearby()].
#'
#'   Note that BOTH this table and the index of it are
#'   needed in getblocksnearby() or getpointsnearby() !
#'
#'   For 8 million block points, this takes a couple of seconds to do,
#'   so it may be useful to store the index during a session
#'   rather than building it each time it is used. But it cannot be saved
#'   on disk because of what it is and how it works.
#' @param pts a data.frame or data.table with columns name lat and lon, one row
#' per location (point), and any other columns are ignored.
#' @param idcolname if NULL (default), a pointid column is created as a
#'   unique id 1:NROW(). If creating the index of blocks, idcolname is "blockid"
#'   If set to "id" it just uses that even if not unique id.
#'   indexpoints() does not directly refer to this column but index
#'   probably incorporates it.
#' @param xyzcolnames For creating quaddata and then localtree index of blocks,
#'   this must be set to c("BLOCK_X", "BLOCK_Z", "BLOCK_Y")
#'
#' @return returns a data.table one row per point, columns with names that are
#'   c(xyzcolnames, idcolname)
#'
#' @noRd
#'
create_quaddata <- function(pts,
                            idcolname = NULL,
                            xyzcolnames = c("x2", "z2", "y2")
                            # xyzcolnames = c("BLOCK_X", "BLOCK_Z", "BLOCK_Y") # c("x2", "z2", "y2")
) {
  
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180
  
  if (is.null(idcolname)) {
    cols_kept <-  c("lat", "lon")
  } else {
    if (!(idcolname %in% c("id", "blockid"))) {
      warning("only supports idcolname NULL, id, or blockid - ignoring the name provided and creating unique id column called pointid")
      cols_kept <- c("lat", "lon")
      idcolname <- NULL
    }
  }
  if (!is.null(idcolname)) {
    if (idcolname == "blockid") {
      cols_kept <-  c("lat", "lon", "blockid")
    }
    if (idcolname == "id") {
      cols_kept <-  c("lat", "lon", "id")
    }
  }
  if (!all(cols_kept %in% names(pts))) {stop(paste0(cols_kept, collapse = ","), " must all be among colnames of pts") }
  
  cat("creating quaddata format table before indexing\n")
  
  # ensure this will not alter pts in parent frame regardless of class etc:
  
  if (!data.table::is.data.table(pts)) {
    pts <- data.table::copy(pts[ , cols_kept])
    data.table::setDT(pts)
  } else {
    pts <- data.table::copy(pts[ , ..cols_kept])
  }
  
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180
  
  pts[ , LAT_RAD  := lat * radians_per_degree]
  pts[ , LONG_RAD := lon * radians_per_degree]
  pts[ , coslat_x_earth := cos(LAT_RAD) * earthRadius_miles]
  pts[ , x_val := coslat_x_earth    * cos(LONG_RAD)]
  pts[ , y_val := coslat_x_earth    * sin(LONG_RAD)]
  pts[ , z_val := earthRadius_miles * sin( LAT_RAD)]
  
  data.table::setnames(pts, c("x_val", "y_val", "z_val"), xyzcolnames)
  if (is.null(idcolname)) {
    idcolname <- "pointid"
    pts[ , pointid := .I] #  .I  or  seq_len(nrow(pts))
  }
  cols_returned <- c(xyzcolnames, idcolname)
  return(pts[ , ..cols_returned])
}
############################################################################### #


#' Utility to create efficient quadtree spatial index of any set of lat,lon
#'
#' @description Index a list of points (e.g., schools)
#'   so [getpointsnearby()] can find them very quickly
#'
#' @details This creates a spatial index
#'   to be used by [getpointsnearby()] to support [proxistat()],
#'   to create a new proximity score for every block and block group in the US.
#'   It relies on [create_quaddata()] for one step, then [SearchTrees::createTree()]
#'
#' @param pts a data.frame or data.table with columns name lat and lon,
#'   one row per location (point), and any other columns are ignored.
#' @param envir optional environment - default is to assign index object to globalenv()
#' @param indexname optional name to give the index
#'
#' @return Just returns TRUE when done. Side effect is to
#'   put into the globalenv()
#'   (or specified envir) that spatial index with name defined by indexname,
#'   as created by [indexpoints()].
#'
#' @examples \dontrun{
#'   # EXAMPLES NOT TESTED YET ***
#'   pts <- testpoints_10
#'   tempenv <- new.env()
#'   index10 <- indexpoints(pts, "index10", envir = tempenv)
#'   x <- getpointsnearby(pts, quadtree = get(index10, envir = tempenv))
#'   # y <- proxistat(pts)
#'   # rm(custom_index)
#'  }
#'
#' @keywords internal
#' @export
#'
indexpoints <- function(pts, indexname = "custom_index", envir = globalenv()) {
  
  # UNLIKE FOR indexblocks() which is always indexing the same blocks,
  # here WE SHOULD NOT CHECK TO SEE IF INDEX WAS ALREADY CREATED
  # we need to create a new index every time indexpoints() is called.
  
  cat("cleaning/checking latitude and longitude values \n")
  pts <- latlon_df_clean(pts)
  # that does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
  
  cat("Building index of ", nrow(pts), "points, to be called ", indexname,
      " and loaded in specified environment e.g., globalenv() \n")
  
  assign(
    indexname,
    SearchTrees::createTree(
      create_quaddata(pts, idcolname = "pointid"),
      treeType = "quad",
      dataType = "point"
    ),
    envir = envir
  )
  
  cat("  Done building index of ", nrow(pts), "points. \n")
  
  return(TRUE) # or maybe want to do something like  return(get(indexname))
  
  # WE SHOULD NOT ASSUME ANY INDEX ALREADY CREATED
  #  is an index for this particular set of pts passed here.
  # What if someone already did an index for some other set of points
  # and now is trying to create a new index for a new set of points??
  # So we need to create a new index every time indexpoints() is called
  # unlike  indexblocks() where the only thing you ever index is
  # blocks that never change (at least not within a year or more)
}
############################################################################### #


#' Utility to create an efficient quadtree spatial index of EPA-regulated facility locations
#' 
#' @description Index US EPA Facility Registry Service facility locations
#'   so [getfrsnearby()] can find them very quickly
#' 
#' @details This creates a quadtree spatial index of some or all facilities,
#'   to be used by [getfrsnearby()],
#'   such as to count the regulated facilities near some other specified sites, or to
#'   create a new proximity score for every block and block group in the US,
#'   via [proxistat()]
#' 
#' @param frspts optional, default is the frs table from the EJAM package,
#'   but could be a subset of that data.table with columns name lat and lon, one row
#'   per location (point), and any other columns are ignored.
#'   If frspts not specified and indexname exists already, just returns that index without rebuilding it.
#'   If frspts is specified, such as just frs from one industry or one state,
#'   then new index is built, even if one named indexname already existed.
#'   
#' @param envir optional environment - default is to assign index object to globalenv()
#' @return Index is returned and the side effect is it puts in the globalenv()
#'   (or specified envir) that spatial index with name defined by indexname,
#'   as created by [indexpoints()].
#'
#' @keywords internal
#' 
indexfrs <- function(frspts = NULL, indexname = "frs_index", envir = globalenv()) {
  
  if (is.null(frspts)) {
    if (exists(indexname, envir = envir)) {
      cat(indexname, " already exists in specified environment and no new frspts specified, so will not rebuild index\n")
      return(get(indexname, envir = envir))
    }
    if (exists("frs")) {
      return(indexpoints(pts = frs, indexname = indexname, envir = envir))
    } else {
      dataload_from_pins("frs")
      if (!exists("frs")) {
        stop("no frspts specified and no frs data.table found, so no index created")
      }
    }
  } else {
    if (exists(indexname, envir = envir)) {
      warning(indexname, " already existed, but will be replaced by new index since frspts was specified")
    }
    return(indexpoints(pts = frspts, indexname = indexname, envir = envir))
  }
}
############################################################################### #



# topoints or INDEX of topoints that might be called custom_index
#   created via indexpoints() -- a custom index of ANY topoints
#   (e.g., schools, not necessarily blocks)

# ~ ----------------------------------------------------------------- ####



#' Find IDs of and distances to all nearby points (e.g., schools, or EPA-regulated facilities, etc.)
#'
#' @description Given a table of frompoints (lat lon coordinates),
#'   find IDs of and distances to all nearby points
#'   that could represent e.g., schools, parks, or
#'   EPA-regulated facilities with locations in Facility Registry Services (FRS).
#'   Like [getblocksnearby()] but for nearby points of any type.
#' @details Later steps can aggregate at each frompoint to summarize
#'   - count schools or facilities, etc., near each frompoint
#'   - max/min distance for each frompoint, like proximity of nearest, etc.
#'   - a proximity score for each frompoint (e.g., block) and then each blockgroup
#'
#' @param frompoints used as the sitepoints param of [getblocksnearby()].
#'   Can be for example Census blocks (based on internal point of each block).
#'
#' @param topoints table of lat lon coordinates of points that may be nearby.
#'   These could be schools, parks, facilities, or any other set of points.
#'
#' @param radius passed to [getpointsnearbyviaQuadTree()]
#' @param maxradius  passed to [getpointsnearbyviaQuadTree()]
#' @param avoidorphans  passed to [getpointsnearbyviaQuadTree()]
#'
#' @param quadtree optional index of topoints
#'   - if not provided, created by indexpoints()
#' @param quaddatatable optional table of topoints (in
#'   format provided by internal helper function create_quaddata() as needed).
#'
#' @param quiet  passed to [getpointsnearbyviaQuadTree()]
#' @param ...  passed to [getpointsnearbyviaQuadTree()]
#'
#' @return sites2points data.table one row per pair of frompoint and nearby topoint,
#'   like output of [getpointsnearbyviaQuadTree()]
#'
#' @export
#'
getpointsnearby  <- function(frompoints, topoints,
                             radius = 3, maxradius = 31.07, avoidorphans = FALSE, retain_unadjusted_distance = TRUE,
                             quadtree = NULL,
                             quiet = FALSE,
                             parallel = FALSE,
                             updateProgress = FALSE,
                             report_progress_every_n = 1000,
                             ...
) {
  
  if (missing(frompoints)) {stop("must provide frompoints")}
  if (missing(topoints)) {stop("must provide topoints")}
  
  quaddatatable <- create_quaddata(topoints) # although done again inside indexpoints()
  
  if (missing(quadtree) || is.null(quadtree)) {
    indexpoints(pts = topoints, indexname = "custom_index") # which 1st uses create_quaddata(topoints)
    quadtree <- custom_index
  } else {
    message("topoints and quadtree were both provided - they must be (and we just assume they are) based on the same points")
  }
  
  b2s <- getpointsnearbyviaQuadTree(frompoints = frompoints, topoints = topoints,
                                    radius = radius, maxradius = maxradius, avoidorphans = avoidorphans,
                                    quadtree = quadtree,
                                    quaddatatable = quaddatatable,
                                    quiet = quiet, parallel = parallel,
                                    updateProgress = updateProgress, report_progress_every_n = report_progress_every_n,
                                    ...
  )
  
  return(b2s)
}
################################################################# #



#' Find all EPA-regulated facilities nearby each specified point and distances
#'
#' @description Given a table of frompoints (lat lon coordinates),
#'   find IDs of and distances to all nearby points
#'   that represent EPA-regulated facilities with locations in Facility Registry Services (FRS).
#'   Like [getblocksnearby()] but for regulated facilities in US EPA FRS
#' @details Later steps can aggregate at each frompoint to summarize
#'   - count facilities near each frompoint
#'   - max/min distance for each frompoint, like proximity of nearest, etc.
#'   - a proximity score for each block and then each blockgroup
#'
#' @param frompoints used as the sitepoints param of [getblocksnearby()]
#' @param radius passed to [getblocksnearby()]
#' @param maxradius  passed to [getblocksnearby()]
#' @param avoidorphans  passed to [getblocksnearby()]
#' @param quadtree index that should be created by indexpoints
#' @param quaddatatable optional table of topoints (in
#'   format provided by internal helper function create_quaddata() as needed).
#' @param quiet  passed to [getblocksnearby()]
#' @param parallel  passed to [getblocksnearby()]
#' @param ...  passed to [getblocksnearby()]
#'
#' @return sites2points data.table one row per pair of frompoint and nearby frs point,
#'   like output of [getblocksnearby()]
#'
#' @export
#'
getfrsnearby  <- function(frompoints,
                          radius = 3, maxradius = 31.07,
                          avoidorphans = FALSE,
                          quadtree = NULL,
                          quiet = FALSE,
                          parallel = FALSE,
                          ...) {
  
  if (is.null(quadtree)) {
    if (exists("frs_index")) {
      quadtree <- frs_index
    } else {
      # try to create the index on the fly? takes time.
      stop("frs_index not found, index of all Census blocks made from e.g., indexpoints()")
    }
    quadtree <- indexfrs()
  }
  
  b2s <- getblocksnearby(sitepoints = frompoints, radius = radius, maxradius = maxradius,
                         avoidorphans = avoidorphans,
                         quadtree = quadtree, # which should be custom_index index of your points not of blocks
                         quaddatatable = quaddatatable,
                         quiet = quiet,
                         parallel = parallel,
                         ...)
  return(b2s)
}
################################################################# #
# ~ ----------------------------------------------------------------- ####


#' utility - count topoints near each frompoint, AFTER getpointsnearby() or getfrsnearby() or getblocksnearby() was already run
#'
#' @param sites2points output of function like [getpointsnearby()] or [getfrsnearby()] or [getblocksnearby()]
#' @param frompoints_id_colname character string name of column in sites2points that is the unique ID of frompoints
#' @param topoints_id_colname  character string name of column in topoints that is the unique ID of counted points
#' @param radius optional, should be less than or equal to radius originally used to create
#'   sites2points. If radius is provided here, this function counts only the topoints that are at a distance of
#'   less than or equal to this radius (which is likely only a subset of all points within original radius used
#'     to create sites2points). You can run for example,
#'
#' s2s <- getpointsnearby(frompoints = testpoints_10[1,], topoints = frs_from_naics("cement"), radius = 30)
#' countpoints_after_getpoints(s2s)
#' countpoints_after_getpoints(s2s, radius = 20)
#' countpoints_after_getpoints(s2s, radius = 10)
#' countpoints_after_getpoints(s2s, radius = 5)
#' countpoints_after_getpoints(s2s, radius = 3)
#'
#' @seealso [countpoints_nearby()]
#' @return counts data.table with column N for count, and a column named via frompoints_id_colname
#' @examples countpoints_after_getpoints(testoutput_getblocksnearby_10pts_1miles)
#'
#' @keywords internal
#'
countpoints_after_getpoints <- function(sites2points,
                                        frompoints_id_colname = "ejam_uniq_id", 
                                        topoints_id_colname = "blockid", 
                                        radius = NULL) {
  
  if (!is.null(radius)) {stopifnot(length(radius) == 1, is.numeric(radius), radius >= 0)}
  if (!is.data.table(sites2points)) {stop('must be data.table from something like getblocksnearby()')}
  if (!(frompoints_id_colname %in% names(sites2points))) {stop('frompoints_id_colname ', frompoints_id_colname, " not found as column in sites2points")}
  if (!(topoints_id_colname %in% names(sites2points))) {stop('topoints_id_colname ', topoints_id_colname, " not found as column in sites2points")}
  
  if (!is.null(radius)) {
    if (radius > max(sites2points$distance)) {warning("specified radius ", radius, " is larger than max found in table provided, which was ", max(sites2points$distance))}
    sites2points <- data.table::copy(sites2points[distance <= radius, ])
  }
  x <- sites2points[ , .N, keyby = frompoints_id_colname]
  attr(x, "unique") <- uniqueN(sites2points[ , ..topoints_id_colname])
  if (NROW(x) != uniqueN(sites2points[ , ..frompoints_id_colname])) {stop('problem - number of rows in output here should be same as number of unique frompoints as identified by frompoints_id_colname')}
  # x <- unlist(x)
  return(x)
}
################################################################# #

countpoints_nearby <- function(frompoints, topoints, radius = 3) {
  
  # input lat,lon table frompoints
  # input lat,lon table topoints
  # radius in miles
  # return vector 1 row per frompoints row, column
  # attribute called "unique" will store the count of unique topoints overall
  
  # 1) we would just do the entire getblocksnearby() and then aggregate by ejam_uniq_id or siteid to get counts,
  #    if we already will be doing getblocksnearby(),
  # so need helper function to
  #  countpoints_from_s2s(sites2points, siteid_colname = "ejam_uniq_id")
  
  # but also probably
  #   want a simpler version of getpointsnearby() in case ONLY want counts
  #    to something like schools but DONT care about distances to them or their identities?
  # Would be even faster (and not require building an index if one already exists)
  # ... maybe whole new function but that would duplicate a lot of getpointsnearby...
  # ... maybe modify getpointsnearby... to be more flexible and
  
  
  
}
################################################################# #

countpoints_in_shape <- function(shp, topoints) {
  
  
  
  
}
################################################################# #

countpoints_in_fips <- function(fips, topoints) {
  
  
  
  
}
################################################################# #
################################################################# #

# ~ ----------------------------------------------------------------- ####


# PROXISTAT - ONE DRAFT ####


#' proximity score tool DRAFT - trying to use getblocksnearby() normally (pts to blocks)
#'   but then filling in that info for rest of blocks in US
#'
#' @param pts data.table with lat lon column names
#' @param countradius distance within in which nearby sites are counted to create proximity score.
#'   In miles, and default is 5 km (5000 / meters_per_mile = 3.106856 miles)
#'   which is the EJScreen zone for proximity scores based on counts.
#' @param maxradius max distance in miles to search for nearest single facility,
#'   if none found within countradius. EJScreen seems to use 1,000 km as the max to search,
#'   since the lowest scores for proximity scores of RMP, TSDF, or NPL are ROUGHLY 0.001, 
#'   (exactly 0.000747782)
#'   meaning approx. 1/1000 km and km_per_mile = 1.609344 = meters_per_mile / 1000
#'   so 1000 km is 1000 / 1.609344 = 621.3712 miles.
#'   However, the exact min value implies 1337.288 kilometers, or 830.9523 miles?
#'   
#'
#' @return data.table of block groups, with proximityscore, bgfips, lat, lon, etc.
#' 
#' @export
#'
proxistat_via_getblocks <- function(pts, countradius=5, maxradius=31) {
  
  # unique id per site ####
  if (!is.data.frame(pts)) {stop('pts must be a data.frame (or data.table, which is a type of data.frame)')}
  if (!is.data.table(pts)) {
    pts <- copy(pts)
    setDT(pts)
  }
  # pts[, newid = .I]
  if ("ejam_uniq_id" %in% names(pts)) {
    if (pts[, any(duplicated(ejam_uniq_id))]) {
      stop("pts$ejam_uniq_id exists but is not unique - unclear how proximity score should count sites near each block or blockgroup")
    }
    if (pts[, any(ejam_uniq_id != .I)]) {
      warning("caution - pts$ejam_uniq_id exists but is not 1:NROW(pts)")
    }
  } else {
    pts[, ejam_uniq_id := .I]
  }
  
  # 1 row per block-to-site distance, for all blocks with 1+ sites nearby ####
  # get all block to site pairs
  # where any of specified points and any block in US are within initial search radius apart.
  # Essentially it loops over SITEPOINTS and uses index of all BLOCKPOINTS to reveal 
  # which of 8 million blocks has sitepoint(s) within initial search radius
  
  s2b <- getblocksnearby(sitepoints = pts, radius = countradius, maxradius = maxradius, avoidorphans = FALSE, 
                         
                         quaddatatable = quaddata)
  
  # that was avoidorphans=F because we do not really need to find nearest block for every site - just need nearest site for every block!
  
  # something like this is already done by getblocksnearby() though:
  # s2b <-  blockwts[s2b, .(ejam_uniq_id, blockid, distance, blockwt, bgid, block_radius_miles), on = 'blockid']
  # s2b[distance < block_radius_miles, distance := 0.9 * block_radius_miles]  # assumes distance is in miles
  # s2b[ , block_radius_miles := NULL]
  
  # Find closest 1 site (for all other blocks) ####
  # Above left out
  # A. blocks with zero specified points within search radius, so 
  #    for each of those blocks, 
  #    we must now find the closest single specified point
  # B. specified point near no blocks at all, but we do not care about those unless needed for A)
  #
  # but how to id the blocks with no nearby pts??
  # - can we use getblocksnearby() but pass it quaddatable that is just a subset of blocks even though index is for all blocks?
  
  # do we iterate with larger and larger radius until done or just look up all now for huge radius?
  
  # iterating until done would be ok if edited version of getblocksnearby() was made more efficient by using rectangle lookup twice, 
  # once on max radius but then also on circle-in-square-min radius to search net difference of those two boxes, 
  # and only calculate distances for those and then check distances vs radius to min and max to save info for just those in annulus.
  
  
  warning('*** not done - need nearest site for each block that had 0 nearby!')
  
  # h
  
  
  
  
  
  # 1 score per BLOCK ####
  # convert from 1 row per distance pair, into 1 row (score) per BLOCK (not block group, yet)
  # by aggregating the 1 or more sites near a given block
  
  s2b[ , distance.km :=  distance *  meters_per_mile / 1000]
  
  blockscores <- s2b[ , .(
    bgid = bgid[1],
    blockscore = sum(1 / distance.km, na.rm = TRUE),
    nearestsiteid = ejam_uniq_id[which.min(distance.km)], # ???
    nearestsite_distance = min(distance.km, na.rm = TRUE)
  ), by = blockid]
  
  # 1 score per BLOCK GROUP ####
  #  = popwtd mean of block scores in bg 
  
  bgscore <- blockscores[, .(proximityscore = sum(blockscore * blockwt, na.rm = TRUE) / sum(blockwt, na.rm = TRUE)), by = bgid]
  # setnames(bgscore, 'V1', "proximityscore")
  
  # add any of these columns from bgpts?    bgid, lat, lon, bgfips, blockcount?
  # EJAM:::latlon_join_on_bgid()
  # bgscore <- merge(bgscore, bgpts, by = "bgid", all.x = TRUE, all.y = FALSE)
  bgscore[bgpts, bgfips := bgfips, on = "bgid"]
  
  return(bgscore)
}
################################################################# #
# ~ ----------------------------------------------------------------- ####

# PROXISTAT - another DRAFT ####


#' Create a custom proximity score for some user-specified set of points,
#' so that every US block group is assigned a proximity score counting those points
#'
#' Indicator of proximity of residents in each US blockgroup to a custom set of facilities or sites
#'
#' @details A "facility" proximity score for the residents in one place is an indicator of
#'   how far away those facilities are, and how many there are nearby - it accounts
#'   for the number of facilities within 5 kilometers (facility density)
#'   and the distance of each (proximity). If there are more points nearby, and/or the
#'   points are closer to the average resident in a blockgroup, that
#'   blockgroup gets a higher proximity score.
#'
#'   The formula for this proximity score is the sum of (1/d)
#'   where each d is distance of a given site in kilometers,
#'   summed over all sites that are within 5 km (or the single
#'   closest site if none are within 5 km), just as in EJScreen proximity scores
#'   like the TSDF or RMP scores.
#'
#'   Any custom user-provided set of points can be turned into a proximity score,
#'   such as locations of all industrial sites of a certain type,
#'   or all grocery stores, or all schools. A proximity score can be
#'   created for all blocks and block groups in the US (or just one State or Region).
#'   Then the proximity scores can be analyzed in a tool like EJAM, just as the
#'   existing pre-calculated proximity scores are analyzed to represent the
#'   number of nearby hazardous waste treatment stoprage and disposal facilities,
#'   weighted by how far away each one is, as provided in the EJScreen proximity
#'   score for TSDFs.
#'
#'   A custom user-specified proximity score might focus on schools, for example.
#'   The schools proximity score could be analyzed in EJAM for one or more communities,
#'   or areas near regulated facilities, or any set of analyzed places.
#'   That would provide statistics demonstrating which places have more schools
#'   closer to them (or inside the areas defined by polygons or FIPS codes, for example).
#'
#'   To create the proximity score, EJAM uses the same method EJScreen used
#'   to create proximity scores. The specified points first get indexed
#'   by a utility function called indexpoints() and are searched for and counted near
#'   every block and blockgroup in the US via a function called getpointsnearby().
#'
#' @param topoints data.table of lat lon
#'
#' @param countradius distance within in which nearby sites are counted to create proximity score.
#'   In miles, and default is 5 km (5000 / meters_per_mile = 3.106856 miles)
#'   which is the EJScreen zone for proximity scores based on counts.
#' @param maxradius max distance in miles to search for nearest single facility,
#'   if none found within countradius. EJScreen seems to use 1,000 km as the max to search,
#'   since the lowest scores for proximity scores of RMP, TSDF, or NPL are ROUGHLY 0.001,
#'   (exactly 0.000747782)
#'   meaning approx. 1/1000 km and km_per_mile = 1.609344 = meters_per_mile / 1000
#'   so 1000 km is 1000 / 1.609344 = 621.3712 miles.
#'   However, the exact min value implies 1337.288 kilometers, or 830.9523 miles?
#'
#' @param quadtree Index of sites such as facilities that will be the basis for the proximity scores.
#'   Optional, because it can be created here on the fly based on pts parameter,
#'   but can pass it if already exists - an index of block locations,
#'   built during use of EJAM package. create_quaddata()
#'
#' @param quaddatatable optional, created from pts if not passed,
#'   created by create_quaddata() utility, and used to create quadtree
#'
#' @return data.table of block groups, with proximityscore, bgfips, lat, lon, etc.
#'
#' @import data.table
#'
#' @examples
#'  # pts <- testpoints_100
#'  # x <- proxistat(topoints = pts[1:1000,])
#'  #
#'  # summary(x$proximityscore)
#'  # # analyze.stuff   pctiles(x$proximityscore)
#'  #
#'  # plot(x = x$lon, y = x$lat)
#'  # tops = x$proximityscore > 500 & !is.infinite(x$proximityscore) & !is.na(x$proximityscore)
#'  # points(x = x$lon[tops], y = x$lat[tops], col="red")
#'
#' @export
#'
proxistat <- function(topoints, bpoints=blockpoints[state_from_blockid(blockpoints) == "DE", ],
                      blocks_per_batch=1000, countradius = 3.106856, maxradius = 621.3712,
                      quadtree = NULL,
                      quaddatatable = NULL) {
  
  # index the sites, not blocks ####
  
  if (missing(quaddatatable) || is.null(quaddatatable)) {
    quaddatatable <- create_quaddata(topoints)
  }
  #
  if (missing(quadtree) || is.null(quadtree)) {
    indexpoints(topoints)  # # which does create_quaddata() and build index only if custom_index  does not yet exist in search path
    quadtree <- custom_index # custom_index was assigned to globalenv by indexpoints()
  }
  
  km_per_mile <- meters_per_mile / 1000  # km_per_mile = 1.609344  # meters_per_mile #   [1] 1609.344
  
  warning("dataset for most but not all blocks -
          PR and Island Area lacked block area data in source used as of 2023 for EJAM")
  
  warning("if none found within radius of 5km, this proximity score function does not yet create score based on single nearest - see source code for notes")
  
  ######################################## #
  # Sequence of steps in finding d value(s):
  ######################################## #
  
  # get a blocks2sites data.table ####
  # instead of sites2blocks table
  
  ## in batches of blocks ####
  # blocks_per_batch <- blocks_per_batch
  nbatches <- (nrow(bpoints) %/% blocks_per_batch)
  blocks_per_lastbatch <- nrow(bpoints) %% blocks_per_batch
  if (blocks_per_lastbatch > 0) nbatches <- nbatches + 1
  b2s <- list()
  
  ######################################## #  ######################################## #
  
  for (batchnumber in 1:nbatches) {
    
    nstart <- 1 + ((batchnumber - 1) * blocks_per_batch)
    if (batchnumber != nbatches) {
      nstop <- nstart + blocks_per_batch - 1
    } else {
      # last batch
      nstop <- nstart + blocks_per_lastbatch
    }
    cat('checking blocks ', nstart, ' through ', nstop, "\n")
    
    ######################################## #
    
    b2s[[batchnumber]] <- getpointsnearby(
      frompoints = bpoints[nstart:nstop, ],
      topoints = pts,
      radius = countradius, maxradius = maxradius, avoidorphans = TRUE,
      quadtree = quadtree,
      quaddatatable = quaddatatable
    )
    
    # b2s[[batchnumber]] <- getblocksnearby(
    #   bpoints[nstart:nstop, ],
    #   radius = countradius, maxradius = maxradius, avoidorphans = TRUE,  # need to turn on avoidorphans feature again though?
    #   quadtree = quadtree,
    #   quaddatatable = quaddatatable
    # )
    
    # what if none in radius but want nearest 1... does avoidorphans now work for that case? when blocks are topoints not frompoints?
    
    # or do we use getpointsnearby??
    
    # and do we want min distance for each block and later for each bg? per EJScreen possible new approach
    
    ######################################## #
  }
  ######################################## #  ######################################## #
  
  b2s <- data.table::rbindlist(b2s)
  
  # but what cols does
  # AGGREGATE BY blockid, not by site  , to create proxistat
  
  b2s <- blockwts[sites2blocks, .(ejam_uniq_id, blockid, distance, blockwt, bgid, block_radius_miles), on = 'blockid']
  
  
  
  # get distance pairs ####
  # all sites near each block,
  # instead of the usual
  # all blocks near each site, which is done by getblocksnearby()
  # EFFICIENCY QUESTION:
  # A
  # Obvious possible algorithm at least if running it for the entire FRS of all facilities, is to
  #  STEP 1: loop through all 8 million US blocks, and for each block count all nearby facilities (sites),
  #   but vast majority of blocks will have zero sites nearby, so
  #   STEP 2: for every block with zero sites nearby, expand search somehow until finding nearest 1 site. HOW?
  
  #  But for users needing proxistat for a few hundred or thousand site types (e.g. CAFOs)
  #  for all 8 mill us blocks still,
  # another approach to check is
  # B
  # STEP 1: loop through just the 10k, 100k, or 1.5 million sites (is it faster than step 1 above?),
  # and for each site find all nearby blocks, maybe 1k each, say.
  #   then do STEP 2 as above.
  #
  # THE VAST MAJORITY OF BLOCKS WILL HAVE ZERO WITHIN THE 5 KM RADIUS, SO NEAREST 1 IS BASIS FOR THEIR SCORE, BUT
  #   SOME WILL EVEN HAVE ZERO WITHIN THE MAX RADIUS TO CHECK
  # if none found within radius of 5km, this func does not yet create score based on single nearest
  
  
  # 4) for each frompoints, if no distances were found, get nearest single d at any radius,
  #       originally thought perhaps by expanding outwards step by step until at least one is found
  # (but not worth the overhead vs just finding ALL d and picking min ???
  #   unless change getblocksnearby() to filter 1st to only those between some min and max radius)
  
  # steps 3 and 4 had not yet been implemented as of 1/29/23.
  
  
  # # FACILITY DENSITY INDICATOR
  #  AS PROXIMITY SCORE FOR SITES IN FRS
  
  # see also older notes:
  
  #  frsdensity/SCRIPT_how_many_blocks_near_FRS.R
  #
  # frsdensity/FACILITY_DENSITY_PROXIMITY_SCORE.R
  
  
  # not tested
  
  
  # create score per BLOCK = sum of sites wtd by 1/d ####
  
  blockscores <- b2s[ , .(proximityscore = sum(1 / distance.km, na.rm = TRUE), blockwt, bgid), by = blockid]
  
  # create score per BLOCK GROUP = popwtd mean of block scores ####
  
  bgscore <- blockscores[ , .(proximityscore = sum(proximityscore * blockwt, na.rm = TRUE) / sum(blockwt, na.rm = TRUE)), by = bgid]
  
  # to get lat lon and bgfips:
  
  bgscore[bgpts, bgfips := bgfips, on = "bgid"] # not tested
  # or # bgscore <- merge(bgscore, bgpts, by = "bgid", all.x = TRUE, all.y = FALSE)
  
  cat("proximity score is sum of (1/d) where each d is distance of a given site in km, summed over all sites within 5km \n")
  return(bgscore)
}
################################################################# #



# # a test of the functions doaggregate_newscores() and calc_ejam() etc.
#
# s2b <- getblocksnearby(testpoints_5, radius = 1)
# mybg <- data.frame(bgid = blockgroupstats$bgid, 
#                          pop = blockgroupstats$pop,
#                          ST = blockgroupstats$ST,
#                          v1 = runif(nrow(blockgroupstats)),
#                          v2 = 7,
#                          v8 = 10 + runif(nrow(blockgroupstats))
# )
# x <- doaggregate_newscores(
#   s2b, 
#   userstats = mybg,
#   formulas = c("v3 = 10 + v2 + v1", "newvar = v2 + v8"), 
#   radius = 1, 
#   popmeancols = "v8"
#     countcols should be inferred
#     calculatedcols should be inferred
# )
# ################################################################# #


#' proximity.score.in.miles - convert EJScreen proximity scores to miles per site instead of sites per kilometer
#' Shows US percentiles if no arguments used
#' @param scoresdf data.frame of simple proximity scores like for tsdf, rmp, npl 
#'   but not traffic.score or npdes one since those are weighted and not just count per km
#'
#' @export
#'
proximity.score.in.miles <- function(scoresdf=NULL) {
  if (is.null(scoresdf)) {
    scoresdf <- usastats[ , c('proximity.tsdf', 'proximity.rmp', 'proximity.npl')] # EJAM :: usastats
    defaultused = TRUE
  } else {
    defaultused = FALSE
  }
  # grep("^proximity|^traffic",names(blockgroupstats),value = TRUE) # EJAM :: blockgroupstats
  # [1] "traffic.score"   "proximity.npdes" "proximity.npl"   "proximity.rmp"   "proximity.tsdf" 
  # but .npdes is weighted by exposure so should not really be included
  # and traffic.score is weighted by volume of traffic and units or scope differ
  miles_per_km <- 0.6213712 # 1000 / meters_per_mile
  scoresdf <- as.data.frame(scoresdf)
  
  x = 1 / scoresdf
  x = miles_per_km * x
  # x = sapply(scoresdf, function(z) miles_per_km / z)
  x = round(x, 3)
  x = sapply(x, function(z) ifelse(is.infinite(z), NA, z))
  if (missing(scoresdf)) print('missing')
  if (defaultused) {x = data.frame(PCTILE = usastats$PCTILE, x, stringsAsFactors = FALSE)} # EJAM :: usastats
  return(x)
}
################################################################################################################ #
# ~ ####


# see notes about weighted means below


############################################### # 


#' aggregate blockwt values by blockgroup, by site they are in or near
#'
#' @param sites2blocks like output of [getblocksnearby()]
#'   or input to [doaggregate()] or [custom_doaggregate()]
#' @seealso [custom_doaggregate()]
#'
#' @return data.table, 1 row per site-bg pair.
#'   May have same bgid or bgfips in 2,3, more rows
#'   since it is here once per site that the bg is near.
#'   It is like a sites2blockgroups table.
#' 
#' @keywords internal
#'
calc_bgwts_bysite <- function(sites2blocks) {
  sites2blocks[, .(bgwt = sum(blockwt)), keyby = c("ejam_uniq_id", "bgid")]
  # maybe add bgfips for convenience
  # This is the same rows as results_bybg_people,
  # but lacks any joined or calculated results.
}
############################################### # 


#' aggregate blockwt values by blockgroup
#'
#' @param sites2blocks like output of [getblocksnearby()]
#'   or input to [doaggregate()] or [custom_doaggregate()]
#' @seealso [custom_doaggregate()]
#' @return data.table, 1 row per blockgroup (even if bg is near 2+ sites),
#'   so it is a table of all the unique block groups in the overall
#'   analysis (merged across all sites), with a weight that indicates
#'   what fraction of that bg population is included in the overall
#'   analysis. This can be used to get overall results if it is
#'   joined to block group demographics, etc.,
#'   to aggregate each indicator over all block groups using the weights.
#' 
#' @keywords internal
#'
calc_bgwts_overall <- function(sites2blocks) {
  sites2blocks[, .(bgwt = sum(blockwt)), keyby = "bgid"]
  # maybe add bgfips for convenience
}
############################################### # 

#' utility - what type of formula is used to aggregate this variable?
#'
#' @param varnames vector like names_d
#'
#' @return vector same length as varnames, like c("sum of counts", "wtdmean")
#' @examples 
#'  calctype("pop")
#'  calctype(names_d)
#'  x = names(testoutput_ejamit_10pts_1miles$results_overall)
#'  cbind(x, calctype(x))
#'  
#' @export
#' @keywords internal
#'
calctype <- function(varnames) {
  varinfo(varnames, "calculation_type")[, "calculation_type"]
  # map_headernames$calculation_type[match(varnames, map_headernames$rname)]
}
############################################### # 

#' utility - what variable is the weight used to aggregate this variable as a weighted mean?
#'
#' @param varnames vector like names_d
#'
#' @return vector same length as varnames, like c("pop", "povknownratio", "hhlds")
#' @examples calcweight(names_these)
#' 
#' @export
#' @keywords internal
#' 
calcweight <- function(varnames) {
  varinfo(varnames, "denominator")[, "denominator"]
  # map_headernames$denominator[match(varnames, map_headernames$rname)]
}
################################################################################################################ #

# ~ ----------------------------------------------------------------- ####


# CUSTOM DOAGGREGATE - ONE DRAFT  ####


# custom_doaggregate_from_sites2blocks


#' custom version of doaggregate(), to calculate user-provided indicators
#'
#' @param sites2blocks see [doaggregate()]
#' 
#' @param custom_blockgroupstats like blockgroupstats but with custom
#'   indicators, one value per block group, with colnames bgid, bgfips, pop
#' @param countcols vector of colnames in custom_blockgroupstats to be
#'   aggregated as sums of counts, like population counts
#' @param popmeancols vector of colnames in custom_blockgroupstats to be
#'   aggregated as weighted means, population weighted or with other weights
#'   
#' @param wtcols vector of colnames to use as the weights for wtd means,
#'   same length as popmeancols, but not used yet
#'   
#' @param custom_formulas like formulas_all,  not used yet
#' @param custom_cols not used yet
#' @param custom_map_headernames like map_headernames but for the
#'   custom indicators
#'
#' @return list of tables similar to what [doaggregate()] returns
#' 
#' @export
#'
custom_doaggregate <- function(sites2blocks,
                               custom_blockgroupstats = blockgroupstats,
                               countcols = "pop",
                               popmeancols = names_these,
                               wtcols = "pop", # or a vector exactly as long as popmeancols
                               custom_formulas = NULL, # formulas_d,
                               custom_cols = NULL,
                               custom_map_headernames = map_headernames) {
  
  # could add validation of inputs here
  
  
  #  had 2 options: 
  #   1. doaggregate() modified to do the work as usual PLUS take as new param the custom_blockgroupstats,  add in the new columns. 
  #     That would save some duplicated work since only would aggregate block weights in doagg not again in custom_doagg
  #     # and consolidate duplicated code that does error checks, etc. etc.
  #     # and could use the percentile lookup and other code that is there, and add results to 
  # or 
  #   2. custom_doaggregate() to separately do the work calculations
  #     and you can separately then merge that with final results of doaggregate (i.e.,  results_bysite and results_overall and results_bybgetcetc)
  # no percentiles looked up or reported
  # no averages looked up or created
  # no ratios calculated
  # no EJ Indexes created
  # no summary stats like from batch.summarize(), etc.
  
  #################### #
  # aggregate from blocks up to blockgroups
  
  bybg_bysite  <- calc_bgwts_bysite( sites2blocks) # 1 row per site-bg pair (like results_bybg_people) (not rolled up yet to just sites)
  bybg_overall <- calc_bgwts_overall(sites2blocks) # 1 row per bg (not rolled up yet to just one overall total)
  
  
  ################# ------------------------------------------------------------------------- ################ #
  
  ######## here we have something like  sites2blockgroups, aka  bybg_bysite,  
  ##  and sometimes you want to start from that and do the rest of doaggregate() without starting from sites2blocks !
  # therefore,
  ##            *** the code below should be made available on its own without the part above here.
  #
  # for example, if you had done this:
  # x = EJAM::counties_as_sites(fips_counties_from_state_abbrev("DE"))
  # > x
  # ejam_uniq_id countyfips  bgid
  # <int>     <char> <int>
  #   1:            1      10001 43878
  #   2:            1      10001 43879
  ################# ------------------------------------------------------------------------- ################ #
  
  # custom_doaggregate_from_sites2blocks_from_sites2blocks <- function(sites2blockgroups, 
  #                                                                    etc.,
  #                                                                    etc.,
  #                                                                    etc.) {
  #   
  # }
  
  
  #################### #
  # join nationwide indicator data to these places analyzed
  bybg_bysite  <- merge(bybg_bysite,  custom_blockgroupstats, by = "bgid")
  bybg_overall <- merge(bybg_overall, custom_blockgroupstats, by = "bgid")
  
  #################### #
  # calculations just for EACH block group 
  #  No aggregation yet (sum of counts, percentage as ratio, avg of 2 values, etc.)
  if (!is.null(custom_formulas)) {
    bybg_bysite   <- calc_ejam(bybg_bysite,  keep.old = c("ejam_uniq_id", "bgid", "pop", "bgwt"), keep.new = "all", formulas = custom_formulas)
    bybg_overall  <- calc_ejam(bybg_overall, keep.old = c("ejam_uniq_id", "bgid", "pop", "bgwt"), keep.new = "all", formulas = custom_formulas)
  }
  
  ################# ------------------------------------------------------------------------- ################ #
  
  #################### #
  # calculations that AGGREGATE across all blockgroups within each site and overall
  # wtd mean of bgs, sum of counts at bgs, or min or max of bgs.
  # bgwt is the block group weight to use since some bg are only partially included in zone
  
  #################### #
  ## sums of counts
  
  ### started this idea but would need to apply calc_ejam() by group, and would need to include bgwt * x, etc. 
  # see datacreate_formulas_d or formulas_all
  # 
  # results_overall <- calc_ejam(bybg_overall, keep.old = "", keep.new = "all", formulas = formulas_all)
  # results_bysite <- 
  # need to do rollup by group, so could apply calc_ejam by group here
  # or more efficiently for common formulas like sum or popwtdmean
  
  # how it is done in doaggregate() is this:
  
  # sum the counts but weight the counts by the bgwt (since some bgs are only partially included)
  
  countcols_inbgstats = countcols[countcols %in% names(bybg_overall)]
  
  results_overall <- bybg_overall[, lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
  }), .SDcols = countcols_inbgstats]
  
  # sum the counts but weight the counts by the bgwt, but for each site
  
  results_bysite <- bybg_bysite[, lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
  }), .SDcols = countcols_inbgstats, by = .(ejam_uniq_id)]
  
  results_bybg <- bybg_bysite # table of 1 row per bg-site pair is already done
  
  #################### #
  ## wtd means
  
  ####   see drafted  calc_wtdmeans() and see doaggregate() as updated
  stop('not done yet - see newer version of doaggregate() for weighted means')
  ################################################################################################################ #
  
  popmeancols_inbgstats = popmeancols[popmeancols %in% names(bybg_overall)]
  
  ## popwtd mean by SITE ###
  results_bysite_popmeans <- bybg_bysite[   ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop)  ## how to use value of wtcols as the colname, not always "pop" here? ***
  }), .SDcols = popmeancols_inbgstats, by = .(ejam_uniq_id) ]
  
  results_bysite <- merge(results_bysite, results_bysite_popmeans, by = "ejam_uniq_id")
  
  
  ## popwtd mean OVERALL ###
  results_overall_popmeans <- bybg_overall[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop) ## how to use value of wtcols as the colname, not always "pop" here? ***
  }), .SDcols = popmeancols_inbgstats  ]
  
  results_overall <- cbind(results_overall, results_overall_popmeans)
  
  # results_bybg  table of 1 row per bg-site pair is already done
  
  
  if (!is.null(custom_formulas)) {
    ################################################# #
    # aggregation via CUSTOM FORMULAS would be HANDLED HERE #
    ################################################# #
    
    # if ("WORKING YET?" == "YES NOW" && !is.null(custom_formulas)) {
    #   
    #   # to be written...
    #   ## *** PROBLEM HOW TO ALLOW CUSTOM FORMULAS THAT
    #   ##  ALSO WILL  INCORPORATE THE bgwt multiplication 
    #   ##   needed to rollup across block groups correctly??
    #   
    #   
    #   if (is.null(custom_cols)) {
    #     custom_cols = EJAM:::formula_varname(custom_formulas)
    #   }
    # 
    # results_bysite_custom  <-  bybg_bysite[ , calc_ejam( ..???? ), by = "ejam_uniq_id"] 
    
    # need to aggregate from 1 row per site-bg pair into just 1 row per site
    
    #   setDT(bybg_bysite)[, sitepop := sum(pop * bgwt, na.rm = TRUE), by = .(ejam_uniq_id)]
    # bybg_bysite
    
    results_bysite_custom <- list()
    ids = unique(bybg_bysite$ejam_uniq_id)
    n = length(ids)
    for (sitenum in 1:n) {
      
      # THIS IS WRONG - IT SHOULD ROLL UP BY SITE BUT STILL KEEPS ALL THE BLOCKGROUPS...
      # and should use btwt
      
      # NEED A WAY TO DO AGGREGATION BYSITE AND FORMULAS AT THE SAME TIME OR CORRECTLY SEPARATELY.
      #  calc_ejam() has each formula but  does no aggregation.
      #  and just doing data.table   dt[, xyz, by = "ejam_uniq_id"]  
      #   would aggregate but need the formula(s) in there.
      #  check formulas_all, which seemed to allow for aggregation-like calculation??
      
      results_bysite_custom[[sitenum]] <- calc_ejam(
        bybg_bysite[bybg_bysite$ejam_uniq_id == ids[sitenum], ],
        keep.old = c("bgid" ,"pop"),
        keep.new = "all",
        formulas = custom_formulas
      )
    }
    results_bysite_custom <- rbindlist(results_bysite_custom)
    
    # 
    # 
    # 
    # results_overall_custom <-  aggregate to 1 row only
    # 
    #   
    #   
    #   # add it to the other outputs
    
    #   results_overall <- .........................
    
    # }
  }
  
  ########## no ratios, percentiles, averages, etc. etc.
  ########## no other columns added like radius.miles, lat/lon, URLs, block counts, etc. etc. 
  
  ## but also note newer table_signif_round_x100()
  # results_overall = table_x100(results_overall, cnames = names_pct_as_fraction_ejamit)
  # results_bysite  = table_x100(results_bysite, cnames = names_pct_as_fraction_ejamit)
  # results_bybg    = table_x100(results_bybg, cnames = names_pct_as_fraction_ejamit)
  
  return(
    list(
      results_overall = results_overall, 
      results_bysite  = results_bysite, 
      results_bybg    = results_bybg, 
      longnames = fixcolnames(names(results_overall), 'r', 'long', custom_map_headernames)
    )
  )
}
############################################### # 



############################################### # 
# stop() !!!

### test/debug/ try these new functions ...
# 
# outapi = ejscreenit_for_ejam(testpoints_10, radius = 1)
# 
#   ## s2b = getblocksnearby(testpoints_10, radius = 1)
#   s2b = testoutput_getblocksnearby_10pts_1miles
#   x = custom_doaggregate(s2b)
if (1 == 0) {
  data.frame(
    custom = round(t(x$results_overall[, ..names_these]),3),
    ejamit = round(t(testoutput_ejamit_10pts_1miles$results_overall[, ..names_these]),3)
  )
  
  ##                    custom    ejamit
  ## Demog.Index         0.413    0.402   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean. 
  ## Demog.Index.Supp    0.180    0.177   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctlowinc           0.417    0.396   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctlingiso          0.070    0.068   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctunemployed       0.059    0.057   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctlths             0.141    0.149   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## lowlifex            0.216    0.216
  ## pctunder5           0.064    0.064
  ## pctover64           0.108    0.108
  ## pctmin              0.408    0.408
  ## pcthisp            25.471    0.255  ******* 100x
  ## pctnhba             8.590    0.086  ******* 100x
  ## pctnhaa             2.968    0.030  ******* 100x
  ## pctnhaiana          0.884    0.009  ******* 100x
  ## pctnhnhpia          0.002    0.000  ******* 100x
  ## pctnhotheralone     0.091    0.001  ******* 100x
  ## pctnhmulti          2.836    0.028  ******* 100x
  ## pctnhwa            59.158    0.592  ******* 100x
  ## pm                  8.034    8.034
  ## o3                 60.623   60.623
  ## cancer             26.527   26.527
  ## resp                0.289    0.289
  ## dpm                 0.363    0.363
  ## pctpre1960         38.380    0.413  ******* 100x and ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## traffic.score     179.121  179.121
  ## proximity.npl       0.387    0.387
  ## proximity.rmp       0.595    0.595
  ## proximity.tsdf      1.353    1.353
  ## proximity.npdes     0.014    0.014
  ## ust                 5.489    5.489
  ## rsei             6492.776 6492.776
  
  
  i = 3 
  
  cbind(
    custom = round(t(x$results_bysite[i, ..names_these]),3),
    ejamit = round(t(testoutput_ejamit_10pts_1miles$results_bysite[i, ..names_these]),3),
    ejscreenit = round(t(outapi[i, ..names_these]),3) # just one site
  )
  
  
  ######################################################### # 
  ######################################################### # 
  
  
  supressWarnings({
    x = custom_ejamit(testpoints_10, 
                      custom_blockgroupstats = data.frame(blockgroupstats[,.(pop, bgfips, bgid, pctlowinc)]), 
                      countcols = "pop", 
                      popmeancols = "pctlowinc", 
                      wtcols = "pop", 
                      custom_formulas = formulas_d, 
                      custom_map_headernames = map_headernames)
  })
  
  y = ejamit(testpoints_10)
  x$results_bysite[,.(ejam_uniq_id,      pop,  pctlowinc )]
  y$results_bysite[,.(ejam_uniq_id,      pop,  pctlowinc )]
  
  ######################################################### # 
  ######################################################### # 
  
  # define some test inputs  
  
  formulas_test = c("high_pctlowinc <- pctlowinc >= 0.50", "high_pop <- pop >= 5000")
  
  formulas_test = c(formulas_test, formulas_d)
  
  bg_test = data.frame(blockgroupstats[, .(bgid, bgfips, pop, pctlowinc)])
  
  map_headernames_test = data.frame(
    rname  = c("pop", "pctlowinc", "high_pctlowinc", "high_pop"),
    shortlabel = c("Pop", "%low-inc.", "High %lowinc?", "Large Pop.?"),
    longname_tableheader = c("Pop", "% low income", "% low income is High", "Population is Large"),
    pct_as_fraction_blockgroupstats = FALSE,
    pct_as_fraction_ejamit = FALSE
  )
  
  
  sitepoints = testpoints_10
  custom_blockgroupstats = bg_test
  countcols = "pop"
  popmeancols = "pctlowinc"
  wtcols = "pop"
  # custom_cols = NULL
  custom_formulas = formulas_test
  custom_map_headernames = map_headernames_test
  radius = 3
  
  # do this which is in custom_ejamit()
  sites2blocks <- getblocksnearby(sitepoints = sitepoints, radius = radius)
  
  
  
  # out_test <- custom_ejamit(
  #   sitepoints = testpoints_10,
  #   custom_blockgroupstats = bg_test, 
  #   countcols = "pop",
  #   popmeancols = "pctlowinc", 
  #   wtcols = "pop", 
  #   # custom_cols = NULL,
  #   custom_formulas = formulas_test, 
  #   custom_map_headernames = map_headernames_test
  # )
  
  out_test$results_bysite
  
  
  # out_test$results_bysite[out_test$results_bysite$high_pctlowinc, ]
  
}
################################################################################################################ #
# custom_ejamit() ####

#' custom version of ejamit() for calculating user-provided indicators
#'
#' @param sitepoints see [ejamit()]
#' @param radius  see [ejamit()]
#' @param fips  see [ejamit()]
#' @param shapefile  see [ejamit()]
#' @param custom_blockgroupstats like blockgroupstats but with custom
#'   indicators, one value per block group, with colnames bgid, bgfips, pop
#' @param countcols vector of colnames in custom_blockgroupstats to be
#'   aggregated as sums of counts, like population counts
#' @param popmeancols vector of colnames in custom_blockgroupstats to be
#'   aggregated as weighted means, population weighted or with other weights
#'   
#' @param wtcols vector of colnames to use as the weights for wtd means,
#'   same length as popmeancols, but not used yet
#'   
#' @param custom_formulas like formulas_all,  not used yet
#' @param custom_cols not used yet
#' @param custom_map_headernames like map_headernames but for the
#'   custom indicators
#'
#' @return returns the output of [custom_doaggregate()]
#' 
#' @export
#' 
custom_ejamit <- function(sitepoints, radius = 3, fips = NULL, shapefile = NULL,
                          custom_blockgroupstats = blockgroupstats,
                          countcols = names_wts,
                          popmeancols = names_these,
                          wtcols = names_wts, # "pop"  or a vector exactly as long as wtdmeancols
                          custom_formulas = NULL, # formulas_d,
                          custom_cols = NULL,
                          custom_map_headernames = map_headernames) {
  
  if (!is.null(fips)) {
    sites2blocks <- getblocksnearby_from_fips(fips)
  } else {
    if (!is.null(shapefile)) {
      sites2blocks <- get_blockpoints_in_shape(shapefile_from_any(shapefile))
    } else {
      sites2blocks <- getblocksnearby(sitepoints = sitepoints, radius = radius)
    }
  }
  
  return(
    custom_doaggregate(sites2blocks = sites2blocks,
                       custom_blockgroupstats = custom_blockgroupstats,
                       countcols = countcols,
                       popmeancols = popmeancols,
                       wtcols = wtcols,
                       custom_formulas = custom_formulas,
                       custom_cols = custom_cols,
                       custom_map_headernames = custom_map_headernames
    )
  )
}
################################################################################################################ #
# ~ ----------------------------------------------------------------- ####


# CUSTOM DOAGGREGATE - another DRAFT  ####


#' For user-provided indicators and formulas, aggregate at each site and overall
#' 
#' Like doaggregate() but for any user-provided indicator available for all
#' block groups (nationwide so that US percentiles make sense, or at least statewide)
#'
#' @param sites2blocks output of [getblocksnearby()], as for [doaggregate()]
#' 
#' @param userstats like blockgroupstats but data.frame or data.table of all US
#'   blockgroups and one or more columns of user provided raw indicator scores
#'   and any other variables needed for formulas to aggregate indicators
#'   across block groups in each site.
#'   
#' @param formulas a character vector of formulas in R code (see formulas_d for
#'   an example), that use variables in userstats to calculate any
#'   derived indicators or aggregated ones, for cases where just a sum or a
#'   population weighted mean is not the right way to aggregate some indicator.
#'   Formulas can include intermediate steps, or can aggregate across all places.
#'   
#'   For example one formula might be
#'   
#'   "pctover64 = ifelse(pop == 0, 0, over64 / pop)"
#'   
#'                                                                  
#' @param sites2states_or_latlon see [doaggregate()]
#' @param radius  see [doaggregate()]
#' @param countcols  see [doaggregate()]
#' @param popmeancols  see [doaggregate()]
#' @param calculatedcols  see [doaggregate()]
#' @param varsneedpctiles  see [doaggregate()]
#' @param usastats_newscores calculated if not provided
#' @param statestats_newscores calculated if not provided
#' @param ... not used
#'
#' @return  see [doaggregate()]
#' 
#' @keywords internal
#'
doaggregate_newscores <- function(
    
  # userstats must be data.frame of all census blocks in range to be analyzed (US or a State, say),
  # with columns named  bgid or bgfips, ST, pop (if any are pop weighted), and any 
  
  sites2blocks, 
  userstats, formulas = NULL,
  
  sites2states_or_latlon = NA,
  radius = NULL,
  countcols = "pop", popmeancols = NULL, calculatedcols = NULL,
  varsneedpctiles = NULL, 
  # calculate_ratios = FALSE,
  # infer_sitepoints = FALSE,
  # called_by_ejamit = FALSE, updateProgress = NULL, silentinteractive = TRUE, testing = FALSE,
  usastats_newscores = NULL,
  statestats_newscores = NULL,
  ...
) {
  ## based on doaggregate() 
  # doaggregate <- function (
    #   sites2blocks, sites2states_or_latlon = NA, radius = NULL,            
  #   countcols = NULL, popmeancols = NULL, calculatedcols = NULL,               
  #     subgroups_type = "nh", include_ejindexes = FALSE, 
  #   calculate_ratios = TRUE, 
  #     extra_demog = TRUE, need_proximityscore = FALSE, 
  #   infer_sitepoints = FALSE, 
  #     called_by_ejamit = FALSE, updateProgress = NULL, silentinteractive = TRUE, 
  #     testing = FALSE, 
  #   ...)
  
  data.table::setDF(userstats)
  
  # assumed "pop" is the only count column, ie needing to be summed, unless params indicate otherwise
  # assume anything created by a formula in formulas list is a calculated column
  if (is.null(calculatedcols)) calculatedcols <- formula_varname(formulas)
  # assume all other columns get aggregated as population weighted averages (except pop ST bgid etc.)
  if (is.null(popmeancols)) popmeancols <- setdiff(names(userstats), c("pop", calculatedcols, "bgid", "bgfips", "ST"))
  
  data.table::setDT(userstats)
  
  # use a join to get bgid if only fips was provided
  if (!("bgid" %in% names(userstats)) & !("bgfips" %in% names(userstats))) {
    userstats[ , bgid := .I]  # not the true bgid though !
    warning("unique bgid column had to be invented, as 1:N since not provided and bgid2fips missing. This means you cannot join to other blockgroup tables simply on bgid. Use bgfips if available.")
  }
  if (!("bgid" %in% names(userstats)) & "bgfips" %in% names(userstats)) {
    if (exists("bgid2fips")) {
      userstats[bgid2fips, bgid := bgid, on = "bgfips"]
    } else {
      dataload_from_pins("bgid2fips")
      if (!exists("bgid2fips")) {
        userstats[ , bgid := .I]  # not the true bgid though !
        warning("unique bgid column had to be invented, as 1:N since not provided and bgid2fips missing. This means you cannot join to other blockgroup tables simply on bgid. Use bgfips if available.")
      } else {
        userstats[bgid2fips, bgid := bgid, on = "bgfips"]
      }
    }
  }
  
  # get block weights ####
  data.table::setorder(sites2blocks, ejam_uniq_id, bgid, blockid) 
  sites2blocks_overall <- sites2blocks[ ,  .(bgid = bgid[1], blockwt = blockwt[1]), 
                                        by = "blockid"]
  sites2bgs_bysite     <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), 
                                        by = .(ejam_uniq_id, bgid)]
  # block weights aggregated by blockgroup
  #  >>>> A SLOW STEP TO OPTIMIZE   # is this redundant
  sites2bgs_overall  <- sites2blocks_overall[ , .(bgwt = sum(blockwt, na.rm = TRUE) 
  ), by =         "bgid" ]
  
  # get new scores for blockgroups at analyzed places ####
  #    via join from complete dataset of new/user indicators at all blockgroups, to
  #    just the blockgroups aggregated from distance table of blocks near sites
  countcols_inbgstats      <- intersect(countcols,      names(userstats))
  popmeancols_inbgstats    <- intersect(popmeancols,    names(userstats))
  calculatedcols_inbgstats <- intersect(calculatedcols, names(userstats))
  
  sites2bgs_plusblockgroupdata_bysite  <- merge(
    sites2bgs_bysite,  
    userstats[ , c('bgid', 'ST', ..countcols_inbgstats, ..popmeancols_inbgstats, ..calculatedcols_inbgstats)], 
    all.x = TRUE, all.y = FALSE, by = 'bgid')
  sites2bgs_plusblockgroupdata_overall <- merge(
    sites2bgs_overall, 
    userstats[ , c('bgid',       ..countcols_inbgstats, ..popmeancols_inbgstats, ..calculatedcols_inbgstats)], 
    all.x = TRUE, all.y = FALSE, by = 'bgid')
  
  # counts ####
  # if (length(countcols_inbgstats) > 0) {
  results_overall <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
  } ), .SDcols = countcols_inbgstats ]
  results_bysite <- sites2bgs_plusblockgroupdata_bysite[ ,    lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
    #} ), .SDcols = countcols_inbgstats, by = .(siteid) ]
  } ), .SDcols = countcols_inbgstats, by = .(ejam_uniq_id) ]
  # }
  
  # weighted means ####
  if (length(popmeancols_inbgstats) > 0 & "pop" %in% names(sites2bgs_plusblockgroupdata_bysite)) {
    results_bysite_popmeans <- sites2bgs_plusblockgroupdata_bysite[   ,  lapply(.SD, FUN = function(x) {
      collapse::fmean(x, w = bgwt * pop)   # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE) 
    }), .SDcols = popmeancols_inbgstats, by = .(ejam_uniq_id) ]
    results_bysite <- merge(results_bysite, results_bysite_popmeans, by = "ejam_uniq_id") 
    results_overall_popmeans <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
      collapse::fmean(x, w = bgwt * pop) # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)
    }), .SDcols = popmeancols_inbgstats  ]
    results_overall <- cbind(results_overall, results_overall_popmeans) # many columns (the popwtd mean cols)
  }
  
  # use formulas stored in object called formulas ####
  #
  ## for the   calculatedcols_inbgstats but you might need others as inputs so do not want to restrict like results_bysite[, ..calculatedcols_inbgstats]
  if (!is.null(formulas)) {
    dcalculated_overall <- calc_ejam(bg = results_overall, keep.old = "", formulas = formulas)
    results_overall <- cbind(results_overall, dcalculated_overall)
    dcalculated_bysite <- calc_ejam(results_bysite, keep.old = "", formulas = formulas)
    results_bysite <- cbind(results_bysite, dcalculated_bysite)
  }
  
  # In case some calculated variable is via formulas but not in userstats table (could happen?)
  # we would need to calculate formulas for entire USA, all bgs, not just near sites,
  # to then create lookup tables using that.
  #
  # calculate nationwide values for any newly calculated vars that are not already in userstats (not popwtd means or count ones since already were there)
  brandnewvariables <- setdiff(names(dcalculated_overall), names(userstats))
  if (length(brandnewvariables) > 0) {
    if (!is.null(formulas)) {
      dcalculated_usa <- calc_ejam(bg = userstats, keep.old = c("bgfips", "bgid", "ST"), formulas = formulas)
      dcalculated_usa <- dcalculated_usa[ , c(brandnewvariables, intersect(names(dcalculated_usa), c("bgfips", "bgid", "ST")))]
      userstats <- cbind(userstats, dcalculated_usa) # not a merge? calc_ejam should preserve order and length?
    }
  }
  
  # radius ####
  if (missing(radius)) {radius.miles <- round(max(sites2blocks$distance, na.rm = TRUE), 1)}
  ### Infer lat,lon of each ejam_uniq_id if lat,lon not already provided in sites2states_or_latlon ? ####
  # use block lat,lon values to approximate the lat,lon of each site, if we were not given that 
  ## *** this is in doaggregate and not commented out but not used there:
  # if (infer_sitepoints & !all(c("lat","lon") %in% names(results_bysite))) {
  #   sitepoints <- sites2blocks[ , list(lat = mean(lat), lon = mean(lon)), by = "ejam_uniq_id"]
  #   # *** but sitepoints is never used. where should these lat lon values be put, and is this method so inaccurate that it is not worthwhile? trilaterate was not accurate either.
  #   # sites2states # ??? see 1339 later where lat and lon added to results. 
  # }
  
  
  ################################################################################# #
  # WHAT STATE IS EACH SITE IN? (TO ENABLE STATE PERCENTILE LOOKUP) ####
  if (missing(sites2states_or_latlon) | !("ST" %in% names(sites2states_or_latlon))) { # must or should figure out state based on blockid -> blockfips -> ST
    sites2states <- ST_by_site_from_sites2blocks(sites2blocks)
    # returns a data.table with these columns:  siteid, ST  (and only 1 row per siteid! It is just to know the ST of each unique siteid)
    if (!missing(sites2states_or_latlon)) {
      # add in the lat,lon columns - this is always available if ejamit() called this since it passes the pts as sites2states_or_latlon
      if ("ejam_uniq_id" %in% names(sites2states_or_latlon) & "ejam_uniq_id" %in% names(sites2states)) {
        #if ("siteid" %in% names(sites2states_or_latlon) & "siteid" %in% names(sites2states)) {
        sites2states <- merge(sites2states, sites2states_or_latlon, by = 'ejam_uniq_id') #  error if  ejam_uniq_id is not there
        #sites2states <- merge(sites2states, sites2states_or_latlon, by = 'siteid') #  error if  siteid is not there
      } else {
        sites2states <- cbind(sites2states, sites2states_or_latlon) #   ***xxx  HAVE NOT CHECKED IF THIS WORKS OR IS CORRECT !
      }
    } else {
      # maybe get latlon of closest block?? no, just omit lat,lon in this case
    }
  } else { # hope it has ST, which is best, or latlon which is slowest, but in between was via blockid, done above!
    sites2states <- states_infer(sites2states_or_latlon)
    # returns a data.FRAME with these columns (plus others in input):  lat,lon, ejam_uniq_id, ST, statename, FIPS.ST, REGION,  n 
  }
  # sites2states  is df or dt with just 1 row/site, and columns= ejam_uniq_id, ST ; and MIGHT have lat,lon and other info.
  
  results_bysite[sites2states,  ST := ST,  on = "ejam_uniq_id"] # check this, including when ST is NA 
  results_overall$ST <- NA
  results_bysite[, statename :=  stateinfo$statename[match(ST, stateinfo$ST)]]
  results_overall$statename <- NA
  ################################################################################# #
  
  
  
  ################################################################################# #
  # PERCENTILES ####
  
  # make percentile lookup tables if not provided as parameters ####
  
  #  note we need to do this  after formulas used, in case we need to find pctiles for those derived vars... 
  
  
  if (is.null(statestats_newscores)) {
    usastats_newscores   <- EJAM:::pctiles_lookup_create(x = userstats[ , !(names(userstats) %in% c("bgid", "bgfips", "pop", "ST") )],
                                                         # wts = userstats$pop,
                                                         zoneOverallName = "USA")
  }
  if (is.null(statestats_newscores)) {
    statestats_newscores <- EJAM:::pctiles_lookup_create(x = userstats[ , !(names(userstats) %in% c("bgid", "bgfips", "pop", "ST") )],
                                                         # wts = userstats$pop,
                                                         zone.vector = userstats$ST)
  }
  
  if (is.null(varsneedpctiles)) {
    varsneedpctiles <- setdiff(unique(c(popmeancols, calculatedcols, countcols, names(usastats_newscores))), c("bgid", "bgfips", "pop", "ST", "PCTILE", "REGION"))
  }
  
  
  
  
  varnames.us.pctile    <- paste0(      'pctile.', varsneedpctiles) # but EJ indexes do not follow that naming scheme and are handled with separate code
  varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles) # but EJ indexes do not follow that naming scheme and are handled with separate code
  us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite)     <- varnames.us.pctile
  state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile
  us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall)    <- varnames.us.pctile
  # SLOW:
  for (i in seq_along(varsneedpctiles)) {
    myvar <- varsneedpctiles[i]
    if ((myvar %in% names(usastats_newscores)) & (myvar %in% names(results_bysite)) & (myvar %in% names(results_overall))) {  # use this function to look in the lookup table to find the percentile that corresponds to each raw score value:
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
        unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats_newscores) 
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
        unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats_newscores) 
      # (note it is a bit hard to explain using an average of state percentiles   in the "overall" summary)
    } else { # cannot find that variable in the percentiles lookup table
      us.pctile.cols_bysite[    , varnames.us.pctile[[i]]] <- NA
      us.pctile.cols_overall[   , varnames.us.pctile[[i]]] <- NA
    }
    if ((myvar %in% names(statestats_newscores)) & (myvar %in% names(results_bysite)) ) {
      state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(    ### VERY SLOW STEP 289 msec
        unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats_newscores, zone =  results_bysite$ST)
      ## These must be done later, as avg of sites:
      # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats_newscores, zone =  results_overall$ST)
    } else {
      state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- NA
      # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- NA
    }
  }
  results_overall <- cbind(results_overall, us.pctile.cols_overall ) # , state.pctile.cols_overall)
  results_bysite  <- cbind(           results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite )
  # *** OVERALL AVG of STATE PERCENTILES ####
  #  (as popwtd mean of sites state pctiles - which seems bizarre but not sure how else you would do it)
  varnames.state.pctile <- varnames.state.pctile
  state.pctile.cols_overall <-  results_bysite[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = pop)  # stats::weighted.mean(x, w = pop, na.rm = TRUE)
  }), .SDcols = varnames.state.pctile ]
  results_overall <- cbind(results_overall, state.pctile.cols_overall)
  ################################################################################# #
  
  # (US and STATE AVERAGES not used, no ratios calculated here) ####
  
  # RADIUS (inferred or passed here) added to results  ####
  results_overall[ , radius.miles := radius]
  results_bysite[  , radius.miles := radius]
  sites2bgs_plusblockgroupdata_bysite[ , radius.miles := radius]
  
  # LATITUDE and LONGITUDE added to results  ####
  if ("lat" %in% names(sites2states)) {
    results_bysite[sites2states, lat := lat, on = "ejam_uniq_id"]
  } else {
    results_bysite[ , lat := NA]
  }
  if ("lon" %in% names(sites2states)) {
    results_bysite[sites2states, lon := lon, on = "ejam_uniq_id"]
  } else {
    results_bysite[ , lon := NA]
  }
  # add those columns to overall and bybg, so the format is same for overall and bysite tables
  results_overall[ , lat := NA]
  results_overall[ , lon := NA]
  sites2bgs_plusblockgroupdata_bysite[ , lat := NA]
  sites2bgs_plusblockgroupdata_bysite[ , lon := NA]
  
  longnames <- fixcolnames(names(results_overall), oldtype = 'r', newtype = 'long')
  
  results <- list(
    results_overall = results_overall,  # each indicator
    results_bysite  = results_bysite,   # each indicator, at each site
    results_bybg_people = sites2bgs_plusblockgroupdata_bysite,  # each indicator, at each BG-site combo, not just each UNIQUE BG !!
    #  That allows one to see distrib within each demog at each site, not just overall, 
    #  but need be careful when looking at that stat overall to not count some bgs twice. ?
    longnames = longnames,
  )
  
  return(results)
  
}
################################################################################################################ #
################################################################################################################ #
# ~ ----------------------------------------------------------------- ####

