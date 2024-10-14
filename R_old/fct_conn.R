#' Connection to postgresql database
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @return connection
#' @export
#'
#' @examples
#' db_connection <- db_con()
db_con <- function(){
  con <- DBI::dbConnect(RPostgres::Postgres(),
                 host = Sys.getenv("DBMAPDO_HOST"),
                 port = Sys.getenv("DBMAPDO_PORT"),
                 dbname = Sys.getenv("DBMAPDO_NAME"),
                 user      = Sys.getenv("DBMAPDO_USER"),
                 password  = Sys.getenv("DBMAPDO_PASS"))
  return(con)
}
