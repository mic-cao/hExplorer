#' Storm Track Interpolation
#'
#' Given the id of a storm, predict its longitude and latitude coordinates every
#' 30 minutes using linear interpolation.
#'
#' @param storm_id object of type character containing a storm id
#' @return a data frame containing the predicted longitude and latitude
#' coordinates every 30 minutes derived using linear interpolation
#'
#' @examples
#' interp_storm <- interpolate("AL252005")
#' print(interp_storm)
#' @export
interpolate <- function(storm_id)
{
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

  storm.info <- hurdat[hurdat$id == storm_id,
                       c('datetime', 'latitude', 'longitude')]

  storm.info <- storm.info[match(unique(storm.info$datetime), storm.info$datetime),]

  if (nrow(storm.info) ==1) {return(storm.info)}

  storm.info <- storm.info[format(storm.info$datetime, "%M") == "00" |
                             format(storm.info$datetime, "%M") == "30", ]

  datetime.new <- seq(storm.info$datetime[1],
                      storm.info$datetime[nrow(storm.info)],
                      "30 min")
  N <- length(datetime.new)
  time.diff <- diff(storm.info[,'datetime'])
  long <- rep(NA, N); lat <- rep(NA, N)
  interval <- ifelse(any(time.diff >= 30), 30, 0.5)
  for (i in 1:(nrow(storm.info)-1))
  {
    if (i==1) {start = 1}
    end <- start + floor(time.diff[i]/interval)
    long[start:end] <- seq(storm.info$longitude[i],
                           storm.info$longitude[i+1],
                           length.out = end - start + 1)
    lat[start:end] <- seq(storm.info$latitude[i],
                           storm.info$latitude[i+1],
                           length.out = end - start + 1)
    start <- end
  }

  interpolation <- data.frame(datetime = datetime.new,
                              latitude = lat,
                              longitude = long)
  return(interpolation)
}
