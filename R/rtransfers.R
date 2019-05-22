# rtransfers
#
# This is the rtransfers function. It is used to run a journal title
# or issn or other piece of information through the JOURNAL TRANSFERS API
# and output results.
#
# You can learn more about the Journal Transfer API at:
#
#   https://journaltransfer.issn.org/api
#

library(httr)
library(jsonlite)
library(xml2)

rtransfers <- function(path) {
  url <- paste("https://journaltransfer.issn.org/api?query=", path, sep="")

  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = False)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintif(
        "TRANSFER API request has failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "rtransfers"
  )

}


