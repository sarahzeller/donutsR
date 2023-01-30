[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# donutsR
A set of R functions that complement my masters thesis.
There, I analyze the effect of landfills on the nearby population.
Since I do not know how close people should live to a landfill to be considered close, I use a parameter: inner radius.
To exclude population that lives too far from any landfill, I use another parameter: outer radius.
With this package, I can create `donut_lists`: lists of regressions with varying inner and outer radius.
There are also functions to interpret the regressions.


## Installation

`remotes::install_github("szeller42/donutsR")`

## Functions

-   `donut_analysis` is the first step in a donut analysis.
-   `plot_significance` and `extract_dist` are convenience functions for `donut_analysis` outputs.

`donut_analysis` takes a data frame with a distance parameter `dist_km` which includes the distance to a relevant geometry for all or some rows. 
Now follow two distance parameters in `dist`. 
The first defines the inner radius, i.e. the distance that is still defined as a treatment. 
The second defines the outer radius, i.e. how far the entire population is allowed to be from the relevant geometry.
The function then performs a `feols` regression and outputs the model, which includes an additional list element with the two distance parameters.  
This function can be combined with `vapply` to produce regressions with varying inner and outer radius. 
