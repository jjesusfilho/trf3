#' Organiza cjsg do TRF3
#'
#' @param df dados
#' @param julgado coluna onde se encontram os julgados
#'
#' @details Esta função é temporária, seus termos já foram incluídos
#'     na função ler_dados_cjsg_trf3
#'
#' @return mesma tibble acrescida data de julgamento, vara e relator
#' @export
#'
#' @examples
#' \dontrun{
#' df <- organizar_cjsg_trf3(df, julgado)
#' }
organizar__cjsg_trf3 <- function(df,julgado){

  j <- rlang::enquo(julgado)

  j <- df %>%
    dplyr::pull(!!j) %>%
    as.character() %>%
    stringr::str_remove_all("\\<br\\>")

  final <- j %>%
    stringr::str_extract("\\X{250}$") %>%
    stringr::str_squish()

  data <- final %>%
    stringr::str_extract("(?<=Data e Hora: )\\d.+") %>%
    lubridate::parse_date_time("dmyHMS",tz="America/Sao_Paulo")

  relator <- final %>%
    stringr::str_extract("(?<=\\(a\\):\\s).+?(?=Nº)")

  vara <- j %>%
    stringr::str_extract("(?<=\\bVr\\b).+?(?=/)") %>%
    stringr::str_trim()

  df <- df %>%
    tibble::add_column(

      relator = relator,
      vara = vara,
      data_julgamento = data
    )

}
