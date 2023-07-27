#' Plotting the Storm Track
#'
#' Given a vector of storm ids, return a map of the trajectories, or tracks, of
#' all of the storms using the available data.
#'
#' @param storm_vec vector of type character containing storm ids
#' @param show.legend logical argument indicating whether a legend with the storm ids
#' should be provided; if the length of storm_vec is less than 5, it is plotted on the
#' map, and if it is greater than 5, it is returned separately as a data frame
#' @return a color-coded map of the trajectories, or tracks, of all of the storms in
#' storm_vec using the available data
#'
#' @import maps
#'
#' @examples
#' library(maps)
#' ex.ids <- c("AL042005", "AL051897", "AL162016")
#' ex.tracks <- storm_track(ex.ids, TRUE)
#' @export
storm_track <- function(storm_vec, show.legend = FALSE)
{
  if (!requireNamespace("maps", quietly = TRUE))
  {
    stop("The required maps package is not installed.
         Please install and load the package before using this function")
  }
  if (!exists("hurdat"))
  {
    stop("The required data 'hurdat' is not loaded.
         Please load the data before using this function")
  }
  if (!is.character(storm_vec))
  {
    stop('error... storm_id must be character')
  }
  if (any(is.na(match(storm_vec, hurdat$id))))
  {
    stop('error... storm_id does not exist in database')
  }
  if (!is.logical(show.legend))
  {
    stop('error... show.legend must be logical')
  }

  storm.info <- hurdat[hurdat$id %in% storm_vec, ]
  if(diff(range(storm.info$longitude)) < 15 ||
     diff(range(storm.info$latitude)) < 15)
  {
    range.long <- range(storm.info$longitude) + c(-1,1)*15
    range.lat <- range(storm.info$latitude) + c(-1,1)*15
  }  else {
    range.long <- range(storm.info$longitude) + c(-5,5)
    range.lat <- range(storm.info$latitude) + c(-5,5)
  }

  par(mar = rep(0, 4))
  cur.col <- sample(colors(), length(storm_vec))
  maps::map("world", col = "lemonchiffon", fill = T, bg = "skyblue1",
      xlim = range.long, ylim = range.lat)
  maps::map("state", col = "thistle1", fill = T, add = T)

  for(i in 1:length(storm_vec))
  {
    row.idx <- which(hurdat$id == storm_vec[i])
    longs <- hurdat$longitude[row.idx]
    lats <- hurdat$latitude[row.idx]

    coords <- data.frame(longs, lats)
    coords <- coords[cumsum(rle(as.character(coords$longs))$lengths), ]

    arrows(coords$longs[-length(coords$longs)],
           coords$lats[-length(coords$lats)],
           coords$longs[-1], coords$lats[-1],
           length = 0.07, col = sample(colors(), 1), lwd = 2, lty = 1)
  }
  if(length(storm_vec) <= 5)
  {
    legend("bottomleft", legend = storm_vec,
           col = cur.col, lty = 1, lwd = 2, cex = 0.5, inset = c(0, 0),
           bg = 'white')
  }
  else if(show.legend)
  {
    legend.dat <- data.frame(id = storm_vec,
                             Name = hurdat$name[match(storm_vec, hurdat$id)],
                             Color = cur.col)
    return(legend.dat)
  }
}
