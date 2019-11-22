#' Baixar processos de primeiro grau
#'
#' @param processos Vetor com os números dos dos processos
#' @param diretorio Diretório para onde vão os htmls
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
baixar_cpopg_trf3 <- function(processos = NULL, diretorio = "."){

  processos <- stringr::str_remove_all(processos,"\\D") %>%
    abjutils::build_id()


url <- "http://csp.jfsp.jus.br/csp/consulta/consinternet.csp"


evento<- httr::GET(url) %>%
  httr::content() %>%
  xml2::xml_find_first("//input[@value='Pesquisar']") %>%
  xml2::xml_attr("onclick") %>%
  stringr::str_extract("(?<=\\').+?(?=\\')")




purrr::walk(processos,purrr::possibly(purrrogress::with_progress(~{

  body <- list(`WARGC`= 2,
               `WEVENT`= evento,
               WARG_1= .x,
               WARG_2= 61)



  url2 <- "http://csp.jfsp.jus.br/csp/consulta/%25CSP.Broker.cls"
  url3<-"http://csp.jfsp.jus.br/csp/consulta/consinternetpro1a.csp"

  httr::POST(url2,body=body,encode="form")



  arquivo <- paste0("_cpopg_", stringr::str_remove_all(.x,"\\D"), ".html")

 a<- httr::RETRY("GET", url3, httr::timeout(30),
              httr::write_disk(file.path(diretorio, Sys.time() %>%
                                           stringr::str_replace_all("\\D+", "_") %>%
                                           stringr::str_replace("$", arquivo))))




}),NULL))


}

