% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loop_and_display.r
\name{loop_and_display}
\alias{loop_and_display}
\title{Print output}
\usage{
loop_and_display(
  .tbl,
  .vars,
  .f = pander::pandoc.table,
  sort = FALSE,
  skip_if_nothing = TRUE,
  orig_to_display = list(),
  header_lvl_top = 2,
  header_lvl_max = 6,
  header_lvl_max_repeat = FALSE
)
}
\arguments{
\item{.tbl}{dataframe.
Contains columns with names in \code{.vars}.}

\item{.vars}{character vector.
Names of columns in \code{.tbl} for which
combinations of unique elements within each define a set of
row(s) within \code{.tbl}.
Each such set of row(s) is passed to the function \code{.f}.}

\item{.f}{function.
Takes a dataframe as input, and prints or cat's output.
Default is \code{pander::pandoc.table}.}

\item{sort}{logical.
Whether to sort the unique elements with the last (inner-most) looping
variable. For example, if we loop on the column \code{"V1"}
in the dataframe \code{data.frame(V1 = c(x2, x1, x2, x1), V2 = ...)},
if we do not sort the first header related to V1 will be
the element x2 and the second x1 (i.e. the order
in which they appear in the column). If we sort,
this will be reversed and first x1 would appear.
Similarly, if V1 were a factor,
then sorting would force the elements to be displayed
in the order they are specified in the levels of the factor.
Default is \code{FALSE}.}

\item{skip_if_nothing}{logical.
If \code{TRUE}, then no headings or output from \code{.f}
are printed for combinations of the unique elements in \code{.vars}
for which there are no corresponding entries in \code{.tbl}.}

\item{orig_to_display}{named list.
Used to rename headers as specified by unique levels
of the variables in \code{.tbl} selected by \code{.vars},
e.g. renaming \code{"M"} to \code{"Male"}.
Names correspond to columns in \code{.tbl} selected by \code{.vars}.
Element is either a named character vector or a function.
If the element is a named character vector,
then the header is used to select the element within the vector.
Note that numeric elements are coerced to character.
If the element is a function, then the original header
is passed as the first (non-default) argument to the function and
the output is then taken to be the header.
Default is \code{list()}.}

\item{header_lvl_top}{integer.
The level of the header used for the
highest level looping variable.
Corresponds to the number of
hashes before a Markdown heading.
Default is \code{2}.}

\item{header_lvl_max}{integer.
Maximum header level to print.
Default is \code{6}.}

\item{header_lvl_max_repeat}{logical.
If \code{TRUE}, then the smallest
header level is used for all looping variables
corresponding to header level six or higher.
if \code{FALSE}, then the variables corresponding
to (what would be) header level seven or higher
are plotted in bold with their level number in
brackets next to them.
Default is \code{FALSE}.}
}
\value{
\code{invisible(TRUE)}, if successful.
}
\description{
Loop over combinations of elements
from column in a dataframe, printing the
elements as Markdown headings and outputting
some sort of Markdown-output (such as a plot
or a table).

The key advantage over something like using
\code{dplyr::group_by} with \code{dplyr::do}
is that the headings are printed out without repeats.

The advantage over writing out loops by hand
is that it's less typing and much easier to make
changes to.
}
\examples{

test_tbl <- tibble::tibble(
  x = rep(letters[1:2], each = 3),
  y = purrr::map(1:2, function(i) {
    letters[(3:5) - as.numeric(i)]
  }) \%>\%
    unlist()
)
loop_and_display(
  test_tbl,
  .vars = c("x", "y")
)
loop_and_display(
  test_tbl,
  .vars = c("x", "y"),
  orig_to_display = list(
    "x" = c(
      "a" = "Letter A",
      "b" = "Letter B"
    )
  )
)
}
