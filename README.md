[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# myFunctions
A set of R functions that I use often.

## Installation

`remotes::install_github("szeller42/donutsR")`

## Functions

-   `convert_to_qmd_table` converts a `data.frame` or `tibble` to a `qmd` table.
-   `custom_theme` is the theme I use for ggplots.
-   `ggsave_embed` enables saving ggplots with embedded fonts to PDF.
-   `donut_analysis` is the first step in a donut analysis.
-   `plot_significance` and `extract_dist` are convenience functions for `donut_analysis` outputs.

`donut_analysis` takes a data frame with a distance parameter `dist_km` which includes the distance to a relevant geometry for all or some rows. 
Now follow two distance parameters in `dist`. 
The first defines the inner radius, i.e. the distance that is still defined as a treatment. 
The second defines the outer radius, i.e. how far the entire population is allowed to be from the relevant geometry.
The function then performs a `plm` regression and outputs the model, which includes an additional list element with the two distance parameters.  
This function can be combined with `vapply` to produce regressions with varying inner and outer radius. 
