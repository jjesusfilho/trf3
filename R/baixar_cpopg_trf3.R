#' Baixar processos de primeiro grau do TRF3
#'
#' @param processos Vetor com os números dos dos processos
#' @param diretorio Diretório para onde vão os htmls
#' @param movimentacao Baixar movimentacao completa? Default: TRUE
#'
#' @return htmls
#' @export
#'
#' @examples
#' \dontrun{
#' dir.create("cpopg")
#' baixar_cpopg_trf3(processos = "0014367-81.2016.4.03.6100",diretorio="cpopg")
#'
#' }
baixar_cpopg_trf3 <- function(processos = NULL, movimentacao = TRUE, diretorio = "."){

  processos <- stringr::str_remove_all(processos,"\\D") %>%
    abjutils::build_id()


url <- "http://csp.jfsp.jus.br/csp/consulta/consinternet.csp"


evento<- httr::GET(url) %>%
  httr::content() %>%
  xml2::xml_find_first("//input[@value='Pesquisar']") %>%
  xml2::xml_attr("onclick") %>%
  stringr::str_extract("(?<=\\').+?(?=\\')")


pb <- progress::progress_bar$new(total = length(processos))

purrr::walk(processos,purrr::possibly(~{

  pb$tick()

  body <- list(`WARGC`= 2,
               `WEVENT`= evento,
               WARG_1= .x,
               WARG_2= 61)



  url2 <- "http://csp.jfsp.jus.br/csp/consulta/%25CSP.Broker.cls"
  url3<-"http://csp.jfsp.jus.br/csp/consulta/consinternetpro1a.csp"
  url4 <-"http://csp.jfsp.jus.br/csp/consulta/consinternetpro1d.csp"
  httr::POST(url2,body=body,encode="form")



  arquivo <- paste0("_cpopg_", stringr::str_remove_all(.x,"\\D"), ".html")
  arquivo2 <- paste0("_cpopg_movimentacao_", stringr::str_remove_all(.x,"\\D"), ".html")

  httr::RETRY("GET", url3, httr::timeout(30),
              httr::write_disk(file.path(diretorio, Sys.time() %>%
                                           stringr::str_replace_all("\\D+", "_") %>%
                                           stringr::str_replace("$", arquivo))))


if (movimentacao == TRUE){

  httr::RETRY("GET", url4, httr::timeout(30),
              httr::write_disk(file.path(diretorio, Sys.time() %>%
                                           stringr::str_replace_all("\\D+", "_") %>%
                                           stringr::str_replace("$", arquivo2))))
}

},NULL))


}

