#' Elimina duplicados
#'
#' @param con conexao
#' @param pkey chave primária
#' @param coluna nome da coluna de referência
#' @param tbl tabela
#' @return nada
#' @export
#'
psql_distinct <- function(con, tbl= NULL, pkey = NULL, coluna = "ordem"){

query<- glue::glue_sql("DELETE
FROM
{`tbl`} a
USING {`tbl`} b
WHERE
a.{`id`} < b.{`id`}
AND a.{`coluna`} = b.{`coluna`}", .con = con)

DBI::dbExecute(con,query)
}
