library(maps)

test_that("storm_track correctly handles exceptions", {

  expect_error(storm_track(20)) # wrong input type
  expect_error(storm_track('AL0000')) # ID does not exist
  expect_error(storm_track("AL111870", 0)) # non-logical 2nd argument

  for (i in 1:5)
  {
    test.id <- sample(hurdat$id, sample(1:10, 1))
    show.legend <- as.logical(sample(0:1, 1))
    expect_no_error(storm_track(test.id, show.legend))
    expect_no_warning(storm_track(test.id, show.legend))
  }

})
