devtools::load_all()
devtools::test()
devtools::document()
devtools::check()

?hurdat
?interpolate
?storm_track
?made_landfall
?storm_location
?cyclone_energy


library(maps); library(sp)


id <- sample(unique(hurdat$id), 1)
interpolate(id)


id <- sample(unique(hurdat$id), 1500)
for (i in 1:1500)
{
  print(id[i])
  interpolate(id[i])
}


id <- sample(unique(hurdat$id), 400)
storm_track(id, show.legend = T)


# horizontal line
for (id in unique(hurdat$id))
{
  storm.info <- hurdat[hurdat$id == id,
                       c('datetime', 'latitude', 'longitude')]
  if (length(unique(storm.info$latitude)) == 1 &
      length(unique(storm.info$longitude)) > 1)
  {
    print(id)
  }
}


# a single point
for (id in unique(hurdat$id))
{
  storm.info <- hurdat[hurdat$id == id,
                       c('datetime', 'latitude', 'longitude')]
  if (length(unique(storm.info$latitude)) == 1 &
      length(unique(storm.info$longitude))== 1)
  {
    print(id)
  }
}


id <- sample(unique(hurdat$id), 1)
made_landfall(id)
made_landfall("AL032019", as.POSIXct("2019-07-22 12:00:00", tz='UTC'))


id <- sample(unique(hurdat$id))
for (i in id)
{
  tryCatch({
    made_landfall(i)
  }, warning = function(w) {
    message(paste(i))
  })
}


id <- sample(hurdat$id, 1000)
for (i in 1:1000)
{
  storm.info <- hurdat[hurdat$id == id[i],]
  dir.distances <- storm.info[, grepl("34|50|64", colnames(storm.info))]
  if (sum(is.na(dir.distances)) == 0 && made.landfall(id[i])
      && (nrow(storm.info) > 20) && diff(range(storm.info$longitude)) > 5)
  {
    id <- id[i]
    break
  }
}
# id <- "AL302005"
storm_location(id)


id <- sample(hurdat$id, 1)
cyclone_energy(id)
id <- unique(hurdat$id)
ACE <- rep(NA, length(id))
for(i in 1:length(id))
{
  ACE[i] <- cyclone_energy(id[i])
}
hist(ACE, breaks = 20)
