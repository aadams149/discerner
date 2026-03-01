\name{run_discerner_app}
\alias{run_discerner_app}
\title{Launch the discerner Shiny app}
\usage{
run_discerner_app(...)
}
\arguments{
\item{...}{Additional arguments passed to \code{\link[shiny]{runApp}}, such as
\code{port}, \code{host}, or \code{launch.browser}.}
}
\value{
This function does not return a value; it launches a Shiny app.
}
\description{
Opens an interactive Shiny application for ranking items by preference.
Items are entered in a text area (one per line or comma-separated), then
sorted through pairwise "battle" comparisons presented as clickable buttons.
}
\examples{
if (interactive()) {
  run_discerner_app()
}
}
