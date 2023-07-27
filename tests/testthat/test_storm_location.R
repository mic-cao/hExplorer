library(maps)

test_that("storm_location correctly handles exceptions", {

  expect_error(storm_location(20)) # wrong input type
  expect_error(storm_location('AL0000')) # ID does not exist

  test.id <- sample(hurdat$id, 10)
  expect_error(storm_location(test.id)) # multiple IDs
  expect_error(storm_location(test.id[1], 0)) # non-logical 2nd argument
  expect_error(storm_location(test.id[1], T, '10:00')) # not-formatted 3rd argument

  for (i in 1:10)
  {
    knots.info <- hurdat[hurdat$id == test.id[i],
                         grepl("34|50|64", colnames(hurdat))]
    if (nrow(na.omit(knots.info)) == 0)
    {
      expect_error(storm_location(test.id[i])) # missing data
    }
    else
    {
      show.track <- as.logical(sample(0:1, 1))
      time.pt <- sample(hurdat$datetime[hurdat$id == test.id[i]], 1)
      expect_no_error(storm_location(test.id[i], show.track))
      expect_no_warning(storm_location(test.id[i], show.track))
      expect_no_error(storm_location(test.id[i], show.track, time.pt))
      expect_no_warning(storm_location(test.id[i], show.track, time.pt))
    }
  }

})
