% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod09nrt.R
\name{extract_fn_from_path}
\alias{extract_fn_from_path}
\title{Get the filename from a path}
\usage{
extract_fn_from_path(fn_with_path)
}
\arguments{
\item{fn_with_path}{The filename, with partial or full path}
}
\value{
\code{fn} The extracted filename
}
\description{
The filename is split on slashes, and the last item is taken; this should be just
the filename.
}
\examples{
fn_with_path = "/Library/Frameworks/R.framework/Versions/2.15/Resources/library/modiscloud/extdata
/2002raw/MYD35_L2.A2002185.1910.005.2007206043609.hdf"
extract_fn_from_path(fn_with_path)

}
\author{
Nicholas J. Matzke \email{matzke@berkeley.edu}
}
\seealso{
\code{\link[base]{strsplit}}
}

