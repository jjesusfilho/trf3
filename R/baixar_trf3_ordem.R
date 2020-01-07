#' Baixar julgados do trf3 por ordem
#'
#' @param ordem vetor com números de ordem
#' @param diretorio diretório
#'
#' @return baixa html no destino designado
#' @export
#'
baixar_trf3_ordem <- function(ordem, diretorio){
purrr::walk(ordem,purrr::possibly(purrrogress::with_progress(~{

    arquivo<-file.path(diretorio,Sys.time() %>%
                         stringr::str_replace_all("\\D+","_") %>%
                         stringr::str_c("_",as.character(.x),".html"))

    url<-paste0("http://web.trf3.jus.br/acordaos/Acordao/BuscarDocumentoGedpro/",.x)

    httr::GET(url,httr::write_disk(arquivo,overwrite = TRUE))

  }),NULL))
}
