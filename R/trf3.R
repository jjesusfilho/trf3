#' \code{trf3} package
#'
#' Baixa  e organiza decisões do TRF3
#'
#'
#' @docType package
#' @name trf3
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".","linkdocumento","tipodocumento", "relator",
    "fonte","decisao","ementa", "classe",  "data_publicacao","data_decisao",
    "ordem","no_orig","vara_origem","proc","valor_causa","data_protocolo",
    "tipo_distribuicao","secretaria","data_distribuicao","assunto"))
}
