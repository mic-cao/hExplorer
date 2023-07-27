#' Computing the accumulated cyclone energy
#'
#' Given an id of a storm, return the accumulated energy, which is the sum of
#' the squared maximum sustained velocities recorded every 6 hours.
#'
#' @param storm_id object of type character containing a storm id
#' @return a scalar representing the accumulated energy of the storm with the
#' requested storm id
#'
#' @examples
#' AL141933.energy <- cyclone_energy("AL141933")
#' print(AL141933.energy)
#' @export
cyclone_energy <- function(storm_id)
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

  max.sustained.vels  <- hurdat$max_sustained[hurdat$id == storm_id]

  if (any(is.na(max.sustained.vels)))
  {
    stop(paste("error...", storm_id, "has missing data"))
  }

  sum.sq.vels <- sum(max.sustained.vels^2)
  ACE <- sum.sq.vels * (10^(-4))
  return(ACE)
}
