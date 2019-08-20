#' Lê partes do processo de segunda instância do trf3
#'
#' @param arquivos Vetor de caminhos para os artigos
#' @param diretorio Se arquivos não forem fornecidos, fornecer
#'     diretório
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ler_partes_cjsg_trf3(diretorio ".")
#' }
ler_partes_cjsg_trf3<- function(arquivos = NULL,diretorio = "."){

  `%||%` <- rlang::`%||%`


  arquivos <-  arquivos %||% list.files(path = diretorio, pattern = ".html",
                                        full.names = TRUE)

  purrr::map2_dfr(arquivos,ordem, purrr::possibly(~{


    ordem <- stringr::str_extract(arquivos,"\\d{7,}")

    x <- xml2::read_html(.x)

    classe_processo <-xml2::xml_find_first(x,"//div/b[1]") %>%
      xml2::xml_text(trim=TRUE) %>%
      stringr::str_split("\\s(?=\\d{7})",simplify=TRUE) %>%
      tibble::as_tibble() %>%
      setNames(c("classe","processo")) %>%
      tidyr::separate(processo,c("processo","origem"),sep = "/")

    pessoas <- xml2::xml_find_all(x,"//td[@width='22%']/linha[@estilo='parte']") %>%
      xml2::xml_text()

    pessoas_nomes <- xml2::xml_find_all(x,"//td[@width='75%']/linha[@estilo='parte']") %>%
      xml2::xml_text()

    nomes <- tibble::tibble(pessoas,pessoas_nomes) %>%
      dplyr::distinct() %>%
      dplyr::group_by_at(dplyr::vars(-pessoas_nomes)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = pessoas, value = pessoas_nomes) %>%
      dplyr::select(-row_id) %>%
      tidyr::fill(tidyselect::everything(),.direction="down")



    cbind(ordem = .y, classe_processo, nomes) %>%
      dplyr::mutate_all(list(~iconv(.,"UTF-8","latin1//TRANSLIT")))
  },NULL))
}
