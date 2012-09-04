\name{queryYQL}
\alias{queryYQL}
\title{Query Yahoo Query Language (YQL) webservice.}
\usage{
  queryYQL(query, format = "json", file,
    yql.url = "http://query.yahooapis.com/v1/public/yql/",
    env = "store://datatables.org/alltableswithkeys",
    quiet = FALSE)
}
\arguments{
  \item{query}{Query to perform.}

  \item{format}{Format of the response. Either "json" or
  "xml".}

  \item{file}{Output file for saving the response. If no
  file is specified, the function will return the response
  instead of saving it to a file.}

  \item{yql.url}{URL of the webservice.}

  \item{env}{Environment for the query.}

  \item{quiet}{If TRUE, suppress status messages.}
}
\value{
  If file is not specified, return the response for the
  query. If file is specified, response is saved to the
  file and nothing is returned.
}
\description{
  Query Yahoo Query Language (YQL) webservice.
}
\examples{
queryYQL("SELECT * FROM yahoo.finance.quotes WHERE symbol IN (\\"YHOO\\")")
queryYQL("SELECT * FROM twitter.search WHERE q=\\"#R\\" LIMIT 5")
}
\author{
  Jussi Paananen \email{jussi.paananen@uef.fi}
}
\seealso{
  \code{\link{queryYQLtoR}} for returning YQL responses as
  R-object lists.
}

