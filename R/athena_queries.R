#' @rdname queries
#' @title AWS Athena Queries
#' @description Get, Create, and Delete Athena Queries
#' @param id A character string containing an Amazon Athena Query ID. A vector of IDs can also be specified to request multiple queries.
#' @param database
#' @param name
#' @param query
#' @param description
#' @param n An integer specifying the maximum number of results to return.
#' @param token A character string specifying a continuation token, for pagination.
#' @param \dots Additional arguments passed to \code{\link{athenaHTTP}}
#' @references
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_ListNamedQueries.html}{API Reference: ListNamedQueries}
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_CreateNamedQuery.html}{API Reference: CreateNamedQuery}
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_GetNamedQuery.html}{API Reference: GetNamedQuery}
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_BatchGetNamedQuery.html}{API Reference: BatchGetNamedQuery}
#'  \href{https://docs.aws.amazon.com/athena/latest/APIReference/API_DeleteNamedQuery.html}{API Reference: DeleteNamedQuery}
#' @export
list_athena_queries <-
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
    res <- athenaHTTP("ListNamedQueries", body = bod, ...)
    res
}

#' @rdname queries
#' @export
get_athena_query <-
function(
  id,
  ...
) {
    bod <- list()
    if (length(id) > 1L) {
        bod$NamedQueryIds <- id
        res <- athenaHTTP("BatchGetNamedQuery", body = bod, ...)
    } else {
        bod$NamedQueryId <- id
        res <- athenaHTTP("GetNamedQuery", body = bod, ...)
    }
    res
}

#' @rdname queries
#' @export
create_athena_query <-
function(
  database,
  query,
  name,
  description = NULL,
  ...
) {
    bod <- list()
    bod$Name <- name
    bod$Database <- database
    bod$QueryString <- query
    if (!is.null(description)) {
        bod$Description <- description
    }
    res <- athenaHTTP("CreateNamedQuery", body = bod, ...)
    res
}

#' @rdname queries
#' @export
delete_athena_query <-
function(
  query,
  ...
) {
    bod <- list()
    bod$NamedQueryId <- query
    res <- athenaHTTP("DeleteNamedQuery", body = bod, ...)
    res$NamedQueryIds
}

