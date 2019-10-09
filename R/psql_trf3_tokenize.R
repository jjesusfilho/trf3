#' Cria um index a partir dos julgados
#'
#' @param con conexão
#' @param tbl tabela
#'
#' @return Nenhum objeto R é retornado
#' @export
#'
#' @examples
#' \dontrun{
#' library(carf)
#' con <- dbx::dbxConnect()
#' dplyr::copy_to(con, "julgados", df)
#' psql_trf3_tokenize(con, "julgados")
#' }
psql_trf3_tokenize <- function(con, tbl) {

  target <- "document_tokens"
  julgado <- "julgado"
  idx <- paste0(tbl,"_idx")

  query <- glue::glue_sql("ALTER TABLE {`tbl`} ADD COLUMN {`target`} TSVECTOR", .con = con)

  config="pg_catalog.portuguese"

  res <- DBI::dbSendQuery(con, query)

  DBI::dbClearResult(res)

  query <- glue::glue_sql("UPDATE {`tbl`} SET
                         {`target`} = to_tsvector({`julgado`})", .con = con)

  res <- DBI::dbSendQuery(con, query)
  DBI::dbClearResult(res)

  query <- glue::glue_sql("CREATE INDEX {`idx`} ON {`tbl`} USING GIN ({`target`})", .con = con)

  res <- DBI::dbSendQuery(con, query)
  DBI::dbClearResult(res)
}

