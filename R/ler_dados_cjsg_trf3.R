#' Lê conteúdo inteiro teor do trf3
#'
#' @param arquivos Vetor de caminhos para os artigos
#' @param diretorio Se arquivos não forem fornecidos, fornecer
#'     diretório
#' @param plano plano de paralelização, consulte `future::plan`
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ler_dados_cjsg_trf3(diretorio ".")
#' }

ler_dados_cjsg_trf3<- function(arquivos = NULL,diretorio = ".", plano = "sequential"){

  if (is.null(arquivos)){

    arquivos <-  list.files(path = diretorio, pattern = ".html",
                            full.names = TRUE)
  }
  ordem <- stringr::str_extract(arquivos,"\\d{7}")

  future::plan(plano)

  df<- furrr::future_map2_dfr(arquivos,ordem, purrr::possibly(~{

    x <- xml2::read_html(.x)

    classe_processo <-xml2::xml_find_first(x,"//div/b[1]") %>%
      xml2::xml_text(trim=TRUE) %>%
      stringr::str_split("\\s(?=\\d{7})",simplify=TRUE) %>%
      tibble::as_tibble() %>%
      setNames(c("classe","processo")) %>%
      tidyr::separate(processo,c("processo","origem"),sep = "/") %>%
      dplyr::mutate(classe = iconv(classe,"UTF-8","latin1//TRANSLITT"))

    inteiro_teor <-  xml2::xml_text(x,trim = TRUE) %>%
      setNames("inteiro_teor") %>%
      stringr::str_replace_all("(?<!\\:)(\r\n)+(?!\\:)","<br><br>") %>%
      iconv("UTF-8","latin1//TRANSLIT")

    dplyr::bind_cols(ordem =.y, classe_processo = classe_processo,inteiro_teor = inteiro_teor)
  }, NULL))

  df$inteiro_teor <- purrr::map(df$inteiro_teor,~htmltools::HTML(.x) %>%
                                  unlist())
  df
}
