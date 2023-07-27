#' Storm Track Size and Location
#'
#' Given the id of a storm, return a map of its trajectory and size over time.
#'
#' @param storm_id object of type character containing a storm id
#' @param show.track a logical argument indicating whether the track of the storm
#' should be plotted on the map; FALSE by default
#' @param time.pt object of type POSIXct indicating a particular time point
#' of interest in the storm's trajectory that the map will plot; NULL by default,
#' and in this case, plots of storm's size and shape will be overlaid on the map
#' at every time point
#' @return a map plotting the trajectory and size of the storm, as indicated by
#' regions for which the maximum wind extent is at least 34, 50, and 64 knots
#'
#' @import maps
#' @examples
#' library(maps)
#' storm.loc.ex <- storm_location("AL182012")
#'
#' @export
storm_location <- function(storm_id, show.track = FALSE, time.pt = NULL)
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
  if (!is.character(storm_id))
  {
    stop('error... storm_id must be character')
  }
  if (length(storm_id) != 1)
  {
    stop('error... storm_id must be a single character')
  }
  if (is.na(match(storm_id, hurdat$id)))
  {
    stop('error... storm_id does not exist in database')
  }
  if(!is.logical(show.track))
  {
    stop('error... show.track must be logical')
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

    storm.info <- hurdat[hurdat$datetime == time.pt &
                           hurdat$id == storm_id,]
    if(nrow(storm.info)==0)
    {
      stop('error... no row matches the input storm_id and time.pt')
    }
  }
  else
  {
    storm.info <- hurdat[hurdat$id == storm_id,]
  }
  dir.distances <- storm.info[, grepl("34|50|64", colnames(storm.info))]
  dir.distances <- na.omit(dir.distances)
  if(nrow(dir.distances) == 0)
  {
    stop(paste("error...", storm_id, "has missing data"))
  }

  if(show.track)
  {
    row.idx <- which(hurdat$id == storm_id)
    longs <- hurdat$longitude[row.idx]
    lats <- hurdat$latitude[row.idx]
  }
  else
  {
    longs <- storm.info$longitude
    lats <- storm.info$latitude
  }

  if(diff(range(longs)) < 15 &&
     diff(range(lats)) > 12)
  {
    range.long <- range(longs) + c(-1,1)*12
    range.lat <- range(lats) + c(-1,1)*2
  } else if (diff(range(longs)) > 15 &&
             diff(range(lats)) < 12){
    range.long <- range(longs) + c(-1,1)*4
    range.lat <- range(lats) + c(-1,1)*10
  } else if (diff(range(longs)) < 15 &&
             diff(range(lats)) < 12){
    range.long <- range(longs) + c(-1,1)*12
    range.lat <- range(lats) + c(-1,1)*10
  } else {
    range.long <- range(longs) + c(-1,1)*4
    range.lat <- range(lats) + c(-1,1)*2
  }

  mat <- matrix(NA, nrow(dir.distances), 24)
  dist.converter <- function(mat, idx, sign.lat, sign.long)
  {
    change.lat <- dir.distances[,idx]/(sqrt(2)*111.32)
    lat <- storm.info$latitude + sign.lat*change.lat
    change.long <- dir.distances[,idx]/(sqrt(2)*111.32*cos(lat*pi/180))
    long <- storm.info$longitude + sign.long*change.long
    mat[, 2*idx - 1] <- as.matrix(lat)
    mat[, 2*idx] <- as.matrix(long)
    return(mat)
  }

  dirs <- c("NE", "SE", "SW", "NW")
  for(d in dirs)
  {
    idx <- which(grepl(d, colnames(dir.distances)))
    sign.lat <- ifelse(grepl("N", d), 1, -1)
    sign.long <- ifelse(grepl("E", d), 1, -1)
    mat <- dist.converter(mat, idx, sign.lat, sign.long)
  }

  par(mar = rep(0, 4))
  cur.col <- sample(colors(), 3)
  maps::map("world", col = "lemonchiffon", fill = T, bg = "skyblue1",
      xlim = range.long, ylim = range.lat)
  maps::map("state", col = "thistle1", fill = T, add = T)

  if(show.track)
  {
    coords <- data.frame(longs, lats)
    coords <- coords[cumsum(rle(as.character(coords$longs))$lengths), ]
    arrows(coords$longs[-length(coords$longs)], coords$lats[-length(coords$lats)],
           coords$longs[-1], coords$lats[-1],
           length = 0.07, col = sample(colors(), 1), lwd = 2, lty = 1)
  }

  for(i in 1:nrow(mat))
  {
    for(j in 1:3)
    {
      lines(mat[i,c(seq(8*(j-1) + 2, 8*j, 2), 8*(j-1) + 2)],
            mat[i,c(seq(8*(j-1) + 1, 8*j, 2), 8*(j-1) + 1)],
            col = cur.col[j], lwd = 2)
    }
  }
  legend("bottomright", legend = c("34 knots", "50 knots", "64 knots"),
         col = cur.col, lty = 1, lwd = 2, cex = 0.5, inset = c(0, 0),
         bg = 'white')
}
