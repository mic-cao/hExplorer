test_that("interpolate correctly handles exceptions", {

  expect_error(interpolate(20)) # wrong input type
  expect_error(interpolate('AL0000')) # ID does not exist

  test.id <- sample(hurdat$id, 10)
  expect_error(interpolate(test.id)) # multiple IDs

  for (i in 1:10)
  {
    expect_no_error(interpolate(test.id[i]))
    expect_no_warning(interpolate(test.id[i]))
  }

})


test_that("interpolate is of the correct form", {

  test.id <- sample(hurdat$id, 10)

  for (i in 1:10)
  {
    interpolation <- interpolate(test.id[i])
    time.range <- range(hurdat[hurdat$id == test.id[i], 'datetime'])

    expect_s3_class(interpolation, "data.frame")
    expect_equal(range(interpolation$latitude),
                 range(hurdat[hurdat$id == test.id[i], 'latitude']))
    expect_equal(range(interpolation$longitude),
                 range(hurdat[hurdat$id == test.id[i], 'longitude']))
    expect_equal(range(interpolation$datetime), time.range)
    expect_equal(nrow(interpolation),
                 as.integer(difftime(time.range[2],
                                     time.range[1],
                                     units = "mins")/30 + 1))
  }

})
