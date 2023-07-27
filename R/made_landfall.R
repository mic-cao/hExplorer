#' Determine whether a storm made landfall
#'
#' Given the id of a storm, return whether the storm made landfall.
#'
#' @param storm_id object of type character containing a storm id
#' @param time.pt object of type POSIXct indicating a particular time point
#' of interest in the storm's trajectory at which to test whether the storm
#' made landfall; NULL by default
#' @return an object of type logical indicating whether the storm made landfall,
#' i.e., whether the storm reached the United States, including non-contiguous states
#' and surrounding islands
#'
#' @import maps
#' @import sp
#'
#' @examples
#' library(maps); library(sp)
#' landfall.logical <- made_landfall("AL032019", as.POSIXct("2019-07-22 12:00:00", tz='UTC'))
#' print(landfall.logical)
#' @export
made_landfall <- function(storm_id, time.pt = NULL)
{
  if (!requireNamespace("maps", quietly = TRUE))
  {
    stop("The required maps package is not installed.
         Please install and load the package before using this function")
  }
  if (!requireNamespace("sp", quietly = TRUE))
  {
    stop("The required sp package is not installed.
         Please install and load the package before using this function")
  }
  if (!exists("hurdat"))
  {
    stop("The required data 'hurdat' is not loaded.
         Please load the data before using this function")
  }
  if (!is.character(storm_id))
  {
    stop('error... input type must be character')
  }
  if (length(storm_id) != 1)
  {
    stop('error... input must of a single character')
  }
  if (is.na(match(storm_id, hurdat$id)))
  {
    stop('error... input id does not exist in database')
  }
  if(!is.null(time.pt))
  {
    if(mean(class(time.pt) == c("POSIXct", "POSIXt"))!=1)
    {
      stop('error... time.pt must be a POSIXct object')
    }
    if (!inherits(time.pt, "POSIXct"))
    {
      stop('error... time.pt must be in the correct format')
    }
    time.pt <- as.POSIXct(time.pt, tz = "UTC")
    if(is.na(match(time.pt, hurdat$datetime)))
    {
      stop('error... time.pt does not exist in database')
    }

    storm.info <- hurdat[hurdat$datetime == time.pt & hurdat$id == storm_id,
                         c('latitude', 'longitude')]
    if(nrow(storm.info)==0)
    {
      stop('error... no row matches the input storm_id and time.pt')
    }
  }
  else
  {
    storm.info <- hurdat[hurdat$id == storm_id, c('latitude', 'longitude')]
  }

  lat <- storm.info$latitude
  long <- storm.info$longitude

  us_coords_x <- maps::map("usa", plot = FALSE)$x
  us_coords_y <- maps::map("usa", plot = FALSE)$y
  made.landfall <- sp::point.in.polygon(long, lat, us_coords_x, us_coords_y)

  return(any(made.landfall))
}
