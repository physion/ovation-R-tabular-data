\name{CollectMeasurements}
\alias{CollectMeasurements}
\title{Collect tabular data from Measurements of collected Epochs.}
\usage{
  CollectMeasurements(epochs)
}
\arguments{
  \item{epochs}{list of Epochs}
}
\value{
  DataFrame from CSV measurements in Epochs
}
\description{
  Creates a single DataFrame object that concatenates rows
  from all Measurements with content type "text/csv" or
  "application/csv" in the provided Epochs.
}

