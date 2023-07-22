#' build pt asset
#'
#' Build an Earth Engine asset from a data.table of locs.
#'
#' @param DT data.table
#'
#' @param out pathname of output shapefile folder. Includes location of the output folder but does not matter if that folder exists. The last portion of the pathname will be used for the layer name e.g.: 'path/to/caribou' layer name = 'caribou'
#'
#' @param projection character string indicating projection
#'
#' @param id point id column name
#'
#' @param coords coordinates column name for X and Y as character vector
#'
#' @param extra columns preserved in output asset
#'
#' @param overwrite boolean passed to option \code{overwrite_layer} in \code{writeOGR}
#'
#'
#' @export
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "toast"))
#'
#'
#' # Set string of project
#' utm21N <- '+proj=utm +zone=21 ellps=WGS84'
#'
#' # Setnames to build_pt_asset defaults
#' data.table::setnames(DT, 'ID', 'id')
#'
#' # Write out shapefile and zip for EE
#' # (not run)
#' # build_pt_asset(DT, 'data/derived-data/48hr-caribou', utm21N)
build_pt_asset <-
	function(DT,
					 out,
					 projection,
					 id = 'id',
					 coords = c('X', 'Y'),
					 extra = NULL,
					 overwrite = FALSE) {
		extra <- c(id, extra)
		pts <- sp::SpatialPointsDataFrame(DT[, .SD, .SDcols = coords],
																			proj4string = sp::CRS(projection),
																			data = DT[, .SD, .SDcols = extra])

		# TODO: column checks for shapefile standards
		rgdal::writeOGR(pts,
										out,
										utils::tail(data.table::tstrsplit(out, '/'), n = 1L),
										driver = "ESRI Shapefile",
										overwrite_layer = overwrite)

		utils::zip(paste0(out, '.zip'), dir(out, full.names = TRUE))
	}

