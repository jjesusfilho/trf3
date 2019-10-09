#' PostgreSQL full-text search
#'
#' @param con connection
#' @param tbl table
#' @param classes vetor de classes
#' @param processos vetor de processos
#' @param origem vetor de origem
#' @param start data inicial no formato "yyyy-mm-dd"
#' @param end data final no formato "yyyy-mm-dd"
#' @param query palavras a serem buscadas
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' dplyr::copy_to(con, "julgados", df)
#' df <- psql_trf3_query(con, "julgados", "IRPF")
#' }
psql_trf3_query <- function(con, tbl, query = "",classes = NULL, processos = NULL, origem = c("SP","MS"),start = "2009-01-01", end = NULL) {

  target <- "document_tokens"

  origem <- origem %>%
    `[`(1)

  if (is.null(end)) end <- Sys.Date()

  start <- as.Date(start)

  if (!is.null(classes) & !is.null(processos) & !is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.processo IN ({processos*})
                        AND {`tbl`}.origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery('portuguese',{query})", .con = con)

  } else if (is.null(classes) & !is.null(processos) & !is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.processo IN ({processos*})
                        AND {`tbl`}.origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery('portuguese',{query})", .con = con)

  } else if (!is.null(classes) & is.null(processos) & !is.null(origem) & query != "" ){
    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                        AND {`tbl`}.origem IN ({origem*})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery('portuguese',{query})", .con = con)

  } else if (!is.null(classes) & !is.null(processos) & is.null(origem) & query != "" ){

    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.processo IN ({processos*})
                        AND {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})
                       AND {`tbl`}.{`target`} @@ websearch_to_tsquery('portuguese',{query})", .con = con)

  } else if (!is.null(classes) & !is.null(processos) & !is.null(origem) & query == "" ){

    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.processo IN ({processos*})
                        AND {`tbl`}.origem IN ({origem*})
                       AND {`tbl`}.classe IN ({classes*})
                        AND {`tbl`}.data_julgamento BETWEEN ({start}) AND ({end})", .con = con)
  } else

    q <- glue::glue_sql("SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.{`target`} @@ websearch_to_tsquery('portuguese',{query})", .con = con)


  df <- DBI::dbGetQuery(con, q)
}
