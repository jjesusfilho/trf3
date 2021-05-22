#' Baixar acórdãos com número de ordem
#'
#' @param processo Número do processo
#' @param diretorio Diretório
#'
#' @return htmls
#' @export
#'
trf3_baixar_acordaos <- function(processo, diretorio = "."){

  uri <- "http://web.trf3.jus.br/acordaos/Acordao/PesquisarDocumento?processo="

  h1 <- httr::add_headers(
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
    `Accept-Language` = 'en-US,en;q=0.9,pt;q=0.8')


  pb <- progress::progress_bar$new(total = length(processo))

  purrr::walk(processo,purrr::possibly(~{

    pb$tick()

    p <- stringr::str_remove_all(.x,"\\D")

    url <- paste0(uri, p)

    resposta <- httr::GET(url,h1)

    links <- resposta %>%
      httr::content() %>%
      xml2::xml_find_all("//a/@href[contains(.,'Buscar')]") %>%
      xml2::xml_text() %>%
      paste0("http://web.trf3.jus.br/",.)


    datas <- resposta %>%
      httr::content() %>%
      xml2::xml_find_all("//a[contains(@href,'Buscar')]") %>%
      xml2::xml_text()

    purrr::walk2(links, datas, purrr::possibly(~{

      d <- stringr::str_replace_all(.y,"\\D","_")

      ordem <- stringr::str_extract(.x,"\\d+$")

      arquivo <- file.path(diretorio, paste0("processo_",p,"_data_",d,"_ordem_",ordem,".html"))

      httr::GET(.x,h1, httr::write_disk(arquivo,overwrite = TRUE))

    }, NULL))

  },NULL))




}
