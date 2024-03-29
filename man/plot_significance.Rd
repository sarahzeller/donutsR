% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_significance.R
\name{plot_significance}
\alias{plot_significance}
\title{Extract distances from donut_analysis model}
\usage{
plot_significance(
  donut_models,
  var,
  filter_80 = TRUE,
  p_value,
  dep_var,
  se = NULL,
  ...
)
}
\arguments{
\item{donut_models}{List of \code{donut_analysis} models}

\item{var}{The dependent variable in all \code{donut_analysis} models which should be plotted.}

\item{filter_80}{Boolean: Should only those regressions be displayed for which
the treatment group is >= 80\% of the whole population? Defaults to \code{TRUE}.}

\item{p_value}{Alternative to \code{donut_models}: already ready p-value data set.}

\item{dep_var}{If \code{p_value} param is chosen: dependent variable. Character.}

\item{se}{If \code{p_value} is chosen: How standard errors are corrected. Character.}

\item{...}{additional parameters to be passed to \code{ggplot}}
}
\value{
A ggplot2 plot
}
\description{
Convenience function to extract distances from donut_analysis model.
Input needs to be a (list of) \code{donut_analysis} models.
Consider adding \code{theme(legend.text.align = 1)} to right-align the p-value legend.
}
\examples{
data(donut_data)
models <-
donut_models(inner = 2:4, outer = c(10, 15, 20), ds = donut_data,
dep_var = "wealth_index", indep_vars = "age", fe = "id")
plot <- plot_significance(models, var = "dist")
plot
plot$data |> plot_significance(p_value = _, var = "dist", dep_var = "wealth_index",
se = "Clustered standard errors.")
}
