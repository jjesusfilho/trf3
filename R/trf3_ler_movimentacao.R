#' Ler movimentacao do TRF3
#'
#' @param arquivos Vetor de arquivos. Se NULL, informar diretório.
#' @param diretorio Diretório se arquivos não informados.
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- trf3_ler_movimentacao()
#' }
trf3_ler_movimentacao <-function (arquivos = NULL, diretorio = ".")
{
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, "movimentacao", full.names = TRUE)
  }
  pb <- progress::progress_bar$new(total = length(arquivos))
  purrr::map_dfr(arquivos, purrr::possibly(~{
    pb$tick()
    processo <- stringr::str_extract(.x, "\\d{20}")
    x <- xml2::read_html(.x)
    data <- x %>% xml2::xml_find_all("//table[@width='533'][3]//td[@width='84']/p/b") %>%
      xml2::xml_text() %>% tail(-1) %>% lubridate::dmy()
    movimentacao <- x %>% xml2::xml_find_all("//table[@width='533'][3]//td[@width='409']/p") %>%
      xml2::xml_text() %>% tail(-1) %>% iconv("UTF-8",
                                              "LATIN1")
    sequencia <- x %>% xml2::xml_find_all("//table[@width='533'][3]//td[@width='36']/p") %>%
      xml2::xml_text() %>% tail(-1)
    tibble::tibble(processo = processo, sequencia, data,
                   movimentacao) %>%
      dplyr::mutate(secundaria = stringr::str_extract(movimentacao,"\\(?\\S[:lower:].+"),
                    principal = stringr::str_remove(movimentacao,stringr::fixed(secundaria))) %>%
      dplyr::mutate(principal = dplyr::coalesce(principal, movimentacao)) %>%
     dplyr::relocate(principal, .before =5)
  }, NULL))
}
