test_that("cyclone_energy correctly handles exceptions", {

  expect_error(cyclone_energy(20)) # wrong input type
  expect_error(cyclone_energy('AL0000')) # ID does not exist

  test.id <- sample(hurdat$id, 10)
  expect_error(cyclone_energy(test.id)) # multiple IDs

  for (i in 1:10)
  {
    if (any(is.na(hurdat$max_sustained[hurdat$id == test.id[i]])))
    {
      expect_error(cyclone_energy(test.id[i])) # missing data
    }
    else
    {
      expect_no_error(cyclone_energy(test.id[i]))
      expect_no_warning(cyclone_energy(test.id[i]))
    }
  }

})


test_that("cyclone_energy is of the correct form", {

  test.id <- sample(hurdat$id, 10)

  for (i in 1:10)
  {
    speed <- hurdat$max_sustained[hurdat$id == test.id[i]]
    if (!any(is.na(speed)))
    {
      ACE <- cyclone_energy(test.id[i])
      expect_type(ACE, "double")

      if (all(speed == 0))
      {
        expect_equal(ACE, 0)
      }
      else {expect_gt(ACE, 0)}
    }
  }

})
