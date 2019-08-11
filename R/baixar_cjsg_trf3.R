#' Baixa decisões de segunda instância do TRF3
#'
#' @param livre busca livre
#' @param aspas TRUE para colocar a busca entre aspas
#' @param data_inicial formato "dd/mm/aaaa"
#' @param data_final formato "dd/mm/aaaa"
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
baixar_cjsg_trf3 <- function(livre = "", aspas = FALSE, data_inicial = "", data_final = "", diretorio = "."){

  if (aspas ==  TRUE){

    livre <- deparse(livre)

  }

k <- httr::RETRY("GET","http://web.trf3.jus.br/base-textual",httr::timeout(5))

url1<-"http://web.trf3.jus.br/base-textual/Home/ResultadoTotais"


body <-
  list(
    txtPesquisaLivre = livre,
    chkMostrarLista = "on",
    numero = "",
    magistrado = "0",
    data_inicial = data_inicial,
    data_final = data_final,
    data_tipo = "0",
    classe = "0",
    numclasse = "",
    orgao = "0",
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


paginas <- httr::POST(url1, body = body, encode = "form") %>%
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

  httr::RETRY("GET", .y, httr::timeout(30),
              httr::write_disk(file.path(diretorio, Sys.time() %>%
                                           stringr::str_replace_all("\\D+", "_") %>%
                                           stringr::str_replace("$", arquivo))))


})

}

