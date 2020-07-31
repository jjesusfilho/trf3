#' Baixar metadados das decisões de segundo grau
#'
#' @param processos Vetor de processos
#' @param diretorio Diretório onde serão armazenados os processos
#'
#' @return htmls
#' @export
#'
baixar_cposg_trf3 <- function(processos= NULL, diretorio = "."){



  k <- httr::RETRY("GET","http://web.trf3.jus.br/base-textual",httr::timeout(5))

  url1<-"http://web.trf3.jus.br/base-textual/Home/ResultadoTotais"


  pb <- progress::progress_bar$new(total = length(processos))

  purrr::walk(processos,purrr::possibly(~{

    pb$tick()

    p <-  .x %>%
              stringr::str_remove_all("\\D")


      processo <-    abjutils::build_id(p)

    body <-
    list(
      txtPesquisaLivre = "",
      chkMostrarLista = "on",
      numero = processo,
      magistrado = "0",
      data_inicial = "",
      data_final = "",
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
      hdnMostrarListaResumida = "",
      hdnUchkMostrarLista = "on",
      hdnUchkAcordaos = "on",
      hdnUchkSumulas = "off",
      hdnUchkDecisoesMonocraticas = "off",
      hdnUchkTurmasRecursais = "off"
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

    arquivo <- paste0("_pagina_", .x,"_processo_",p, ".html")

  k2<-  httr::RETRY("GET", .y, httr::timeout(30))

    url2<-"http://web.trf3.jus.br/base-textual/Home/ListaColecao/9?np=1"

    httr::RETRY("GET",url2,httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo)),overwrite = T))


  })
  },NULL))
}

