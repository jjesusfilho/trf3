#' Baixa decisões de segunda instância do TRF3
#'
#' @param livre busca livre
#' @param aspas TRUE para colocar a busca entre aspas
#' @param data_inicial formato "dd/mm/aaaa"
#' @param data_final formato "dd/mm/aaaa"
#' @param relator Obtenha a lista completa com a função trf3_magistrados
#' @param classe Obtenha a lista completa com a função trf3_classes
#' @param orgao_julgador Obtenha a lista completa com a função trf3_orgao_julgador
#' @param diretorio Default para atual.
#'
#' @return Baixa os htmls nos diretório especificado
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_cjsg_trf3(
#'   livre = "agência nacional",  data_inicial = "10/07/2019",
#'   data_final = "31/07/2019"
#' )
#' }
baixar_cjsg_trf3 <- function(livre = "",
                             aspas = FALSE,
                             data_inicial = "",
                             data_final = "",
                             relator = "0",
                             classe = "0",
                             orgao_julgador = "0",
                             diretorio = "."){

  if (aspas ==  TRUE){

    livre <- deparse(livre)

  }


  h1 <- httr::add_headers(
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
    `Accept-Language` = 'en-US,en;q=0.9,pt;q=0.8')


k <- httr::GET(url = "http://web.trf3.jus.br/base-textual" , h1)

url1 <- "http://web.trf3.jus.br/base-textual/Home/ResultadoTotais"


body <-
  list(
    txtPesquisaLivre = livre,
    chkMostrarLista = "on",
    numero = "",
    magistrado = relator,
    data_inicial = data_inicial,
    data_final = data_final,
    data_tipo = "0",
    classe = classe,
    numclasse = "",
    orgao = orgao_julgador,
    ementa = "",
    indexacao = "",
    legislacao = "",
    chkAcordaos = "on",
    hdnMagistrado = "",
    hdnClasse = "",
    hdnOrgao = "",
    hdnLegislacao = "",
    hdnMostrarListaResumida = ""
  )


h <-   httr::add_headers(
  `Connection` = 'keep-alive',
  Pragma='no-cache',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
  `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
  `Origin` = "http://web.trf3.jus.br",
  `Content-Type` = 'application/x-www-form-urlencoded',
  `Referer` = 'http://web.trf3.jus.br/base-textual',
  `Accept-Language` = 'en-US,en;q=0.9,pt;q=0.8')


paginas <- httr::POST(url1, body = body, encode = "form", h) %>%
  httr::content() %>%
  xml2::xml_find_first("//span/a[@href='/base-textual/Home/ListaResumida/1?np=0']") %>%
  xml2::xml_text(trim=TRUE) %>%
  stringr::str_extract("\\d+") %>%
  as.numeric() %>%
  `/`(10) %>%
  floor() %>%
  seq(0,.,1)

urls <- paste0("http://web.trf3.jus.br/base-textual/Home/ListaResumida/3?np=",paginas)

purrr::walk2(paginas,urls,~{

  arquivo <- paste0("_pagina_", .x, ".html")

 httr::RETRY("GET", .y, httr::timeout(30),h,
              httr::write_disk(file.path(diretorio, Sys.time() %>%
                                           stringr::str_replace_all("\\D+", "_") %>%
                                           stringr::str_replace("$", arquivo))))


})

}

