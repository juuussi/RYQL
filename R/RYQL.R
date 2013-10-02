#' Query Yahoo Query Language (YQL) webservice. 
#'
#' @param query Query to perform.
#' @param format  Format of the response. Either "json" or "xml".
#' @param file  Output file for saving the response. If no file is specified, the function will return the response instead of saving it to a file.
#' @param yql.url URL of the webservice.
#' @param env Environment for the query.
#' @param quiet If TRUE, suppress status messages.
#'
#' @return If file is not specified, return the response for the query. If file is specified, response is saved to the file and nothing is returned.
#' @author Jussi Paananen \email{jussi.paananen@@uef.fi}
#' 
#' @seealso \code{\link{queryYQLtoR}} for returning YQL responses as R-object lists.
#' 
#' @examples
#' queryYQL("SELECT * FROM yahoo.finance.quotes WHERE symbol IN (\"YHOO\")")
#' queryYQL("SELECT * FROM twitter.search WHERE q=\"#R\" LIMIT 5")
#' @export

queryYQL <- function (query, format="json", file, yql.url="http://query.yahooapis.com/v1/public/yql/", env="store://datatables.org/alltableswithkeys", quiet=FALSE) {
  
  # Handle response format, either XML or JSON
  format <- tolower(format)
  if(!format %in% c("json", "xml")) {
    stop("Invalid method. Should be XML or JSON")
  }
  
  if (env == "" | is.null(env)) {
    env.string <- ""
  } else {
    env.string <- paste("&env=", env, sep="")
  }
  
  # Build query URL
  query.url <- URLencode(paste(yql.url, "?q=", query, "&format=", format, env.string, sep=""))

  if (!quiet) {
    cat(paste("Query url: ", query.url, "\n", sep=""))
  }
  
  # If no output file specified, return the response
  if (missing(file)) {
    response <- readLines(con=url(query.url))
    response
  } 
  
  # If output file specified, save response to the file
  else {
    if (!quiet) {
      cat(paste("Downloading to: ", file, sep=""))
    }
    download.file(query.url,destfile=file, quiet=quiet)
  }
}

#' Query Yahoo Query Language (YQL) webservice and convert the response to an R-object list. 
#'
#' @param query Query to perform.
#' @param full.response If TRUE, include additional details about the response in the returned list, else .
#' @param yql.url URL of the webservice.
#' @param env Environment for the query.
#' @param quiet If TRUE, suppress status messages.
#'
#' @return Response for the query as an R-object list.
#' @author Jussi Paananen \email{jussi.paananen@@uef.fi}
#' 
#' @seealso \code{\link{queryYQL}} for returning raw YQL responses.
#' @examples
#' queryYQLtoR("SELECT * FROM yahoo.finance.quotes WHERE symbol IN (\"YHOO\")")
#' queryYQLtoR("SELECT * FROM twitter.search WHERE q=\"#R\" LIMIT 5")
#' @export

queryYQLtoR <- function (query, full.response=TRUE, yql.url="http://query.yahooapis.com/v1/public/yql/", env="store://datatables.org/alltableswithkeys", quiet=FALSE) {

  require(rjson)
  # Perform the query
  temp.response <- queryYQL(query, format="json", yql.url=yql.url, env=env, quiet=quiet) 
  
  # Convert the response from JSON to R-object list
  response <- fromJSON(temp.response)
  
  # Return either full.response or only query results.
  if (full.response) {
    response
  } else {
    response$query$results
  }
}
