#' Lê processos de primeiro grau do TRF3
#'
#' @param arquivos Vetor de arquivos. Se NULL, informar diretório
#' @param diretorio Informar diretório se arquivos = NULL
#'
#' @return tibble
#' @export
#'
ler_cpopg_trf3<-function(arquivos = NULL, diretorio = "."){

if (is.null(arquivos)){

  arquivos <- list.files(diretorio,"cpopg_\\d",full.names=TRUE)


}

  pb <- progress::progress_bar$new(total = length(arquivos))

purrr::map_dfr(arquivos,purrr::possibly(~{

  pb$tick()

  processo <- stringr::str_extract(.x,"\\d{20}")

  x <- xml2::read_html(.x,encoding="latin1")

  variaveis <- x %>%
             xml2::xml_find_all("//td[@width='122']/p/b") %>%
             xml2::xml_text() %>%
             iconv("UTF-8","LATIN1") %>%
             tail(-2)

  valores <- x %>%
    xml2::xml_find_all("//table[1][@width='533']//td[@width='401']/p") %>%
    xml2::xml_text() %>%
    iconv("UTF-8","LATIN1") %>%
    tail(-2) %>%
    head(length(variaveis))



  tibble::tibble(proc=processo,variavel = variaveis,valor = valores) %>%
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id) %>%
    janitor::clean_names()

},NULL))

}
