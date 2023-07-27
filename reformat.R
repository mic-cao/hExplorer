hurdat <- read.csv('hurdat.txt', header = F)
head(hurdat)

get_formatted_data <- function(dat)
{
  storm_id <- c()
  storm_name <- c()
  best_track_entries <- c()

  idx.header <- which(substr(dat$V1, 1, 2 ) == "AL")
  idx.header <- c(idx.header, nrow(dat)+1)

  for (i in 1:(length(idx.header)-1))
  {
    id <- dat$V1[idx.header[i]]
    name <- dat$V2[idx.header[i]]
    bte <- dat$V3[idx.header[i]]

    storm_id[(idx.header[i]+1):(idx.header[i+1]-1)] <- id
    storm_name[(idx.header[i]+1):(idx.header[i+1]-1)] <- name
    best_track_entries[(idx.header[i]+1):(idx.header[i+1]-1)] <- bte
  }

  idx.header <- idx.header[-length(idx.header)]
  dat_formatted <- cbind(storm_id, storm_name, best_track_entries,
                         dat)[-idx.header,]

  dat_formatted[dat_formatted == -999] <- NA
  dat_formatted[dat_formatted == -99] <- NA
  colnames(dat_formatted) <- c('id', 'name', 'best_track_entries',
                               'date', 'time', 'record_identifier',
                               'system_status', 'latitude',
                               'longitude', 'max_sustained',
                               'min_pressure', '34_NE', '34_SE',
                               '34_SW', '34_NW', '50_NE',
                               '50_SE', '50_SW', '50_NW',
                               '64_NE', '64_SE', '64_SW',
                               '64_NW', 'max_radius')

  long <- as.numeric(substr(dat_formatted$longitude, 2,
                            nchar(dat_formatted$longitude)-1))
  lat <- as.numeric(substr(dat_formatted$latitude, 2,
                           nchar(dat_formatted$latitude)-1))

  store_long <- rep(NA, nrow(dat_formatted))
  store_lat <- rep(NA, nrow(dat_formatted))
  for(i in 1:nrow(dat_formatted))
  {
    long_temp <- dat_formatted$longitude[i]
    lat_temp <- dat_formatted$latitude[i]

    store_long[i] <- ifelse(substr(long_temp, nchar(long_temp),
                                   nchar(long_temp)) == "E",
                            long[i], -long[i])
    store_lat[i] <- ifelse(substr(lat_temp, nchar(lat_temp),
                                  nchar(lat_temp)) == "N",
                           lat[i], -lat[i])
  }

  dat_formatted$longitude <- store_long
  dat_formatted$latitude <- store_lat

  dat_formatted$date <- as.POSIXct(paste0(dat_formatted$date, dat_formatted$time),
                                   format = "%Y%m%d %H%M", tz = "UTC")
  dat_formatted$time <- NULL
  names(dat_formatted)[names(dat_formatted) == "date"] <- "datetime"

  long.fix <- which(dat_formatted$longitude < -180)
  dat_formatted$longitude[long.fix] <- dat_formatted$longitude[long.fix] + 360

  return(dat_formatted)
}

hurdat <- get_formatted_data(hurdat)

if (!dir.exists("data"))
{
  dir.create("data")
}

save(hurdat, file = 'data/hurdat.RData')
