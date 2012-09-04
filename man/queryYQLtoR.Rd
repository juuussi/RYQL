\name{queryYQLtoR}
\alias{queryYQLtoR}
\title{Query Yahoo Query Language (YQL) webservice and convert the response to an R-object list.}
\usage{
  queryYQLtoR(query, full.response = TRUE,
    yql.url = "http://query.yahooapis.com/v1/public/yql/",
    env = "store://datatables.org/alltableswithkeys",
    quiet = FALSE)
}
\arguments{
  \item{query}{Query to perform.}

  \item{full.response}{If TRUE, include additional details
  about the response in the returned list, else .}

  \item{yql.url}{URL of the webservice.}

  \item{env}{Environment for the query.}

  \item{quiet}{If TRUE, suppress status messages.}
}
\value{
  Response for the query as an R-object list.
}
\description{
  Query Yahoo Query Language (YQL) webservice and convert
  the response to an R-object list.
}
\examples{
queryYQLtoR("SELECT * FROM yahoo.finance.quotes WHERE symbol IN (\\"YHOO\\")")
queryYQLtoR("SELECT * FROM twitter.search WHERE q=\\"#R\\" LIMIT 5")
}
\author{
  Jussi Paananen \email{jussi.paananen@uef.fi}
}
\seealso{
  \code{\link{queryYQL}} for returning raw YQL responses.
}

