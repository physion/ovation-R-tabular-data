\name{ImportCSV}
\alias{ImportCSV}
\title{Inserts Ovation Measurements for each row in a DataFrame}
\usage{
  ImportCSV(context, csv.path, start.time, end.time,
    protocol.uri, protocol.parameters, devices,
    device.parameters, container.uri,
    source.label.column = 1, source.id.column = 2)
}
\arguments{
  \item{context}{}

  \item{csv.path}{}

  \item{start.time}{}

  \item{end.time}{}

  \item{protocol.uri}{}

  \item{protocol.parameters}{}

  \item{devices}{}

  \item{device.parameters}{}

  \item{container.uri}{}

  \item{source.label.column}{}

  \item{source.id.column}{}
}
\value{
  newly inserted Epoch
}
\description{
  Inserts Ovation Measurements for each row in a DataFrame
}

