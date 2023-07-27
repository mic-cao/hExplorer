library(maps)
library(sp)

test_that("made_landfall correctly handles exceptions", {
  expect_error(made_landfall(20)) # wrong input type
  expect_error(made_landfall('AL0000')) # ID does not exist

  test.id <- sample(hurdat$id, 10)
  expect_error(made_landfall(test.id)) # multiple IDs
  expect_error(made_landfall(test.id[1], '10:00')) # not-formatted 2nd argument
  expect_error(made_landfall(test.id[1], as.POSIXct("1840-07-22 12:00:00"))) # no match

  for (i in 1:10)
  {
    time.pt <- sample(hurdat$datetime[hurdat$id == test.id[i]], 1)
    expect_no_error(made_landfall(test.id[i]))
    expect_no_warning(made_landfall(test.id[i]))
    expect_no_error(made_landfall(test.id[i], time.pt))
    expect_no_warning(made_landfall(test.id[i], time.pt))
  }

})


test_that("made_landfall is of the correct form", {

  test.id <- sample(hurdat$id, 10)
  usa.long <- na.omit(map("usa", plot = FALSE)$x)
  usa.lat <- na.omit(map("usa", plot = FALSE)$y)

  for (i in 1:10)
  {
    expect_type(made_landfall(test.id[i]), "logical")

    long <- hurdat[hurdat$id == test.id[i], 'longitude']
    lat <- hurdat[hurdat$id == test.id[i], 'latitude']

    if (all(long <= min(usa.long) | long >= max(usa.long)) &&
        all(lat <= min(usa.lat) | lat >= max(usa.lat)))
    {
      expect_false(made_landfall(test.id[i]))
    }
  }

})
