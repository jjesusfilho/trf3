#' Adiciona foreign key
#'
#' @param con conexao
#' @param mtbl tabela principal
#' @param dtbl tabela a receber a chave estrangeiro
#' @param constraint nome do constraint
#' @param key variável de referência
#'
#' @return 0
#' @export
#'
#' @examples
#' \dontrun{
#' psql_add_fkey(con, "dados", "partes",constraint = "dados_parte", key = "ordem")
#' }
psql_add_fkey <- function(con,mtbl,dtbl, constraint, key="processo") {

  query <- glue::glue_sql("ALTER TABLE  {`mtbl`}  ADD  UNIQUE({`key`})",.con = con)

  RPostgres::dbExecute(con,query)

  query <- glue::glue_sql("ALTER TABLE  {`mtbl`}  ALTER  COLUMN  {`key`}  SET  NOT NULL",.con = con)

 RPostgres::dbExecute(con,query)


  query <- glue::glue_sql("ALTER TABLE {`dtbl`} ADD CONSTRAINT {`constraint`}  FOREIGN KEY ({`key`})
                          REFERENCES {`mtbl`} ({`key`})",.con = con)

  RPostgres::dbExecute(con,query)

}
