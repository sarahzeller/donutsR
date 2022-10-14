#' Custom theme for ggplot
#'
#' Creates a custom theme for ggplot, so that all graphs have a coherent look.
#'
#' @import extrafont
#' @import ggplot2
#'
#' @export


##
# library(extrafont)
## If import doesn't work:
# remove.packages("Rttf2pt1")
# remotes::install_version("Rttf2pt1", version = "1.3.8")
#
# font_import(paths = "C:/Users/sarah/Documents/Fonts")
# loadfonts(device = "win")
# loadfonts()

# loadfonts(device = "win")
# Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.55.0/bin/gswin64c.exe")

custom_theme <- function(){
  theme_minimal() +
    theme(text = element_text(family = "Open Sans"),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 13,
                                       color = "grey50"),
          axis.line.x = element_line(colour = "grey40"),
          axis.text.y = element_text(size = 7,
                                     color = "grey10",
                                     hjust = 1),
          axis.text.x = element_text(hjust = 0,
                                     size = 7,
                                     color = "grey10",
                                     vjust = 0.5),
          axis.title.x = element_text(vjust = -.5,
                                      hjust = .5),
          # axis.title.y = element_text(hjust = -1),
          panel.grid = element_blank(),
          panel.spacing = unit(2, "lines")
          )
}
