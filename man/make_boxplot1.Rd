% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/see_num_vars.R
\name{make_boxplot1}
\alias{make_boxplot1}
\title{Boxplot for Numeric Grouped by Categorical}
\usage{
make_boxplot1(data, num_var, cat_var, plot_title, vjust = 1.5)
}
\arguments{
\item{data}{dataset that contains the variables}

\item{num_var}{name of the numeric variable as a string}

\item{cat_var}{name of the categorical variable as a string}

\item{plot_title}{title for the plot as a string}

\item{vjust}{position of the median labels}
}
\value{
none
}
\description{
Create a boxplot to display the distribution of a numeric variable
grouped by a categorical variable. Good for categorical variable that has ten
or less categories. When the maximum observations for a category is < 200, each
observation will be plotted and outliers will be hidden, vice versa for
observations >= 200. Median for each category is labeled in the box.
}
\examples{
make_boxplot1(ggplot2::diamonds, "price", "cut", "Price of Diamonds by Cuts")
}
