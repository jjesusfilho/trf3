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
#' ler_partes_cjsg_trf3(diretorio = ".")
#' }
ler_partes_cjsg_trf3 <- function (arquivos = NULL, diretorio = ".")
{

  if (is.null(arquivos)) {
    arquivos <- list.files(path = diretorio, pattern = ".html",
                           full.names = TRUE)
  }


  pb <- progress::progress_bar$new(total = length(arquivos))

  partes <- purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    ordem <- stringr::str_extract(.x, "\\d{7,}")

    x <- xml2::read_html(.x)

    classe_processo <- xml2::xml_find_first(x, "//div/b[1]") %>%
      xml2::xml_text(trim = TRUE) %>% stringr::str_split("\\s(?=\\d{7})",
                                                         simplify = TRUE) %>% tibble::as_tibble() %>% setNames(c("classe",
                                                                                                                 "processo")) %>% tidyr::separate(processo, c("processo",
                                                                                                                                                              "origem"), sep = "/")
    nomes <-   x  %>%
      xml2::xml_find_all("//table[@mywidth='13,72']") %>%
      rvest::html_table() %>%
      magrittr::extract2(2) %>%
      dplyr::select(-2) %>%
      dplyr::mutate_all(list(~dplyr::na_if(., ""))) %>%
      tidyr::fill(tidyselect::everything(), .direction = "down") %>%
      dplyr::distinct() %>%
      setNames(c("pessoas","pessoas_nomes")) %>%
      dplyr::group_by_at(dplyr::vars(-pessoas_nomes)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = pessoas, value = pessoas_nomes) %>%
      dplyr::select(-row_id) %>%
      tidyr::fill(tidyselect::everything(), .direction = "down")

    cbind(ordem = ordem, classe_processo, nomes) %>%
      dplyr::mutate_all(list(~iconv(.,
                                    "UTF-8", "latin1//TRANSLIT")))
  }, NULL))

  partes <- partes %>%
    janitor::clean_names() %>%
   # tidyr::separate(no_orig, c("numero_origem", "vara_origem"), sep = "\\d?Vr") %>%
  #  tidyr::separate(vara_origem, c("vara_origem", "uf_origem"), sep = "/") %>%
   # dplyr::mutate_at(dplyr::vars(dplyr::ends_with("origem")), stringr::str_squish) %>%
  #  dplyr::mutate(origem = NULL) %>%
    dplyr::mutate(
      cargo_relator = stringr::str_extract(
        relator,
        "(?i)(Desembargadora? Federal|juiza? federal convocad[oa]|juiza? convocad[oa])"
      ),
      relator = stringr::str_remove(
        relator,
        "(?i)(Desembargadora? Federal|juiza? federal convocad[oa]|juiza? convocad[oa])"
      )
    )


}
