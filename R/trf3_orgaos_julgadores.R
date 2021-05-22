#' Obtêm lista atualizada dos órgãos julgadores
#'
#' @return tibble
#' @export
#'
trf3_orgaos_julgadores <- function(){

  h1 <- httr::add_headers(
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
    `Accept-Language` = 'en-US,en;q=0.9,pt;q=0.8')


   httr::GET(url = "http://web.trf3.jus.br/base-textual" , h1) %>%
    httr::content() %>%
    xml2::xml_find_all("//option[@title='Corte Especial']") %>%
    xml2::xml_text(trim = TRUE) %>%
    tibble::tibble(orgao_julgador = .)


}
