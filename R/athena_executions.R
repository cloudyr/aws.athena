#' @rdname executions
#' @title AWS Athena Executions
#' @description Get, Start, and Stop Athena Query Executions
#' @param id A character string containing an Amazon Athena Query Executionn ID. A vector of IDs can also be specified to request multiple query executions.
#' @param n An integer specifying the maximum number of results to return.
#' @param token A character string specifying a continuation token, for pagination.
#' @param \dots Additional arguments passed to \code{\link{athenaHTTP}}
#' @references
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_ListQueryExecutions.html}{API Reference: ListQueryExecutions}
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_GetQueryExecution.html}{API Reference: GetQueryExecution}
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_BatchGetQueryExecution.html}{API Reference: BatchGetQueryExecution}
#' @examples
#' \dontrun{
#' require("aws.s3")
#' b <- aws.s3::put_bucket("aws-athena-r-demo")
#' 
#' # create an Athena database
#' id <- start_athena_execution(
#'   query = "create database DEMO",
#'   output = "s3://aws-athena-r-demo"
#' )
#' 
#' # create a table in the database
#' 
#' }
#' @export
list_athena_executions <-
function(
  n = 50,
  token = NULL,
  ...
) {
    bod <- list()
    bod$MaxResults <- n
    if (!is.null(token)) {
        bod$NextToken <- token
    }
    res <- athenaHTTP("ListQueryExecutions", body = bod, ...)
    res$NamedQueryIds
}

#' @rdname executions
#' @export
get_athena_execution <-
function(
  id,
  ...
) {
    bod <- list()
    if (length(id) > 1L) {
        bod$QueryExecutionIds <- id
        res <- athenaHTTP("BatchGetQueryExecution", body = bod, ...)
    } else {
        bod$QueryExecutionId <- id
        res <- athenaHTTP("GetQueryExecution", body = bod, ...)
    }
    res
}

#' @rdname executions
#' @importFrom aws.s3 get_bucketname
#' @export
start_athena_execution <-
function(
  database = NULL,
  query,
  output,
  encryption = NULL,
  ...
) {
    bod <- list()
    if (!is.null(database)) {
        bod$QueryExecutionContext <- list(Database = database)
    }
    bod$QueryString <- query
    bod$ResultConfiguration <- list(OutputLocation = output)
    if (!is.null(encryption)) {
        bod$ResultConfiguration$EncryptionConfiguration <- encryption
    }
    res <- athenaHTTP("StartQueryExecution", body = bod, ...)
    res
}

#' @rdname executions
#' @export
stop_athena_execution <-
function(
  id,
  ...
) {
    bod <- list()
    bod$QueryExecutionId <- id
    res <- athenaHTTP("StopQueryExecution", body = bod, ...)
    res
}
