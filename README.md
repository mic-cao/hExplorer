# hExplorer: Storm Tracker API (An R Package)

**Authors:** Michael Cao, Aristotle Kolefas

## Overview
The `hExplorer` package provides functionalities to track US coastal storms using Atlantic basin tropical cyclone data. It enables users to interpolate and visualize the path of landfalling hurricanes, compute accumulated cyclone energy (ACE), and explore various storm characteristics.

## Key Features

- Plot storm tracks for specific years
- Visualize storm positions and sizes at landfall
- Compute storm-related statistics such as maximum wind speed, minimum pressure, and ACE
- Conduct statistical analysis on storm data to explore trends over time
- Validate media claims about tropical cyclones and climate change using data

## Installation

You can install the `hExplorer` package by downloading the tar.gz file [here](https://github.com/mic-cao/hExplorer/blob/main/hExplorer_0.1.0.tar.gz). After downloading, you can install the package in R using the following command:

```R
install.packages("/path/to/hExplorer.tar.gz", repos = NULL, type = "source")
```

## Important Functions

`storm_track:` Given a vector of storm ids, return a map of the trajectories, or tracks, of all of the storms using the available data.

```R
storm_track(storm_vec, show.legend = FALSE)
```

`storm_location:` Given the id of a storm, return a map of its trajectory and size over time.

```R
storm_location(storm_id, show.track = FALSE, time.pt = NULL)
```

`made_landfall:` Determine whether a storm made landfall given the id of a storm.

```R
made_landfall(storm_id, time.pt = NULL)
```

`interpolate:` Given the id of a storm, predict its longitude and latitude coordinates every 30 minutes using linear interpolation.

```R
interpolate(storm_id)
```

`cyclone_energy:` Compute the accumulated cyclone energy given an id of a storm.

```R
cyclone_energy(storm_id)
```
## Vignette

The package includes a detailed vignette showcasing various functionalities and analyses. Here's an overview of what you'll find in the vignette:

- Plot storm tracks for multiple years and visualize storm identification.
- Display storm positions and sizes at landfall for specific hurricanes.
- Create a dataframe summarizing storm characteristics including maximum wind speed, minimum pressure, landfall indicator, and ACE.
- Explore interesting insights such as the distribution of maximum sustained wind speeds and the duration of storms.

## Statistical Analysis

The vignette also features statistical analyses to explore trends in storm occurrences and intensity over time. Here's a summary of the analyses conducted:

- **Trend Analysis:** Investigate whether the number of landfalling hurricanes is increasing over time using linear regression.
- **Intensity Analysis:** Explore whether the intensity of tropical cyclones is increasing over time by analyzing the relationship between storm characteristics and year.

## Media Claim Validation

The package validates a media claim about tropical cyclones and climate change using statistical analysis. The claim undergoes evaluation against actual storm data to determine its validity.

## Conclusion

The `hExplorer` package offers a comprehensive suite of tools for analyzing and visualizing US coastal storm data. Whether you're a researcher studying tropical cyclones or a weather enthusiast curious about storm trends, this package provides valuable insights and functionalities.

For detailed usage instructions and examples, please refer to the [vignette](https://github.com/mic-cao/hExplorer/blob/main/vignettes/vignette.Rmd) included with the package.
