% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{hurdat}
\alias{hurdat}
\title{Revised Atlantic Hurricane Database}
\format{
A dataframe with 53972 observations (1952 unique hurricanes) of
23 variables
\describe{
    \item{id}{object of type character unique to each storm; begins with characters "AL"
    and ends with six digits, the first two representing the numbered occurrence of a storm in
    a given year, and the last four representing said year}
    \item{name}{object of type character encoding the common name given to a storm, if applicable}
    \item{best_track_entries}{object of type character indicating
    the number of time points in the storm's trajectory for which data exists}
    \item{datetime}{object of type POSIXct giving the date and time of each recorded point
    during the storm's trajectory}
    \item{record_identifier}{code that provides additional information about a
    particular event or feature related to a given hurricane; possible values include:
    C - Closest approach to a coast, not followed by a landfall;
    G - Genesis;
    I - An intensity peak in terms of both pressure and wind;
    L - Landfall (center of system crossing a coastline);
    P - Minimum in central pressure;
    R - Provides additional detail on the intensity of the cyclone when rapid changes are underway;
    S - Change of status of the system;
    T - Provides additional detail on the track (position) of the cyclone;
    W - Maximum sustained wind speed}
    \item{system_status}{the current stage of development and intensity of a
    given hurricane or weather system; possible values include:
    TD - Tropical cyclone of tropical depression intensity (< 34 knots);
    TS - Tropical cyclone of tropical storm intensity (34-63 knots);
    HU - Tropical cyclone of hurricane intensity (> 64 knots);
    EX - Extratropical cyclone (of any intensity);
    SD - Subtropical cyclone of subtropical depression intensity (< 34 knots);
    SS - Subtropical cyclone of subtropical storm intensity (> 34 knots);
    LO - A low that is neither a tropical cyclone, a subtropical cyclone, nor an extratropical cyclone (of any intensity);
    WV - Tropical Wave (of any intensity);
    DB - Disturbance (of any intensity) }
    \item{latitude}{longitude value of storm's current position}
    \item{longitude}{latitude value of storm's current position}
    \item{max_sustained}{mqximum sustained wind of the hurricane over the past 6-hour period measured in knots}
    \item{min_pressure}{minimum pressure in millibars}
    \item{34_NE}{34 knot wind radii maximum extent in northeastern quadrant (in nautical miles)}
    \item{34_SE}{34 knot wind radii maximum extent in southeastern quadrant (in nautical miles)}
    \item{34_SW}{34 knot wind radii maximum extent in southwestern quadrant (in nautical miles)}
    \item{34_NW}{34 knot wind radii maximum extent in northwestern quadrant (in nautical miles)}
    \item{50_NE}{50 knot wind radii maximum extent in northeastern quadrant (in nautical miles)}
    \item{50_SE}{50 knot wind radii maximum extent in southeastern quadrant (in nautical miles)}
    \item{50_SW}{50 knot wind radii maximum extent in southwestern quadrant (in nautical miles)}
    \item{50_NW}{50 knot wind radii maximum extent in northwestern quadrant (in nautical miles)}
    \item{64_NE}{64 knot wind radii maximum extent in northeastern quadrant (in nautical miles)}
    \item{64_SE}{64 knot wind radii maximum extent in southeastern quadrant (in nautical miles)}
    \item{64_SW}{64 knot wind radii maximum extent in southwestern quadrant (in nautical miles)}
    \item{64_NW}{64 knot wind radii maximum extent in northwestern quadrant (in nautical miles)}
    \item{max_radius}{radius of maximum wind in nautical miles}
}
}
\usage{
hurdat
}
\description{
A dataset containing information about the histories of
cyclones that formed over the Atlantic Ocean
}
\keyword{datasets}
