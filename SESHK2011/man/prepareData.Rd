\name{prepareData}
\alias{prepareData}
\title{prepareData}
\usage{
  prepareData(.raw_data,
    .school = getVariableName("school", .raw_data$spec),
    .network,
    .hobby = getVariableName("hobby", .raw_data$spec),
    .network_mode = c("undirected_and", "undirected_or", "directed", "directed_inversed"),
    .process_network = function(x) {     (x > 0) + 0 },
    .drop_by_case_id)
}
\arguments{
  \item{.raw_data}{}

  \item{.school}{}

  \item{.network}{}

  \item{.hobby}{}

  \item{.network_mode}{}

  \item{.process_network}{}

  \item{.drop_by_case_id}{}
}
\value{
  value
}
\description{
  Description prepareDataprepareData
}
\examples{
\dontrun{

}
}
\author{
  TszKin Julian Chan \email{ctszkin@gmail.com}
}

