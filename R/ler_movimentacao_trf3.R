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
#' df <- ler_movimentacao_trf3()
#' }
ler_movimentacao_trf3 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,"movimentacao",full.names=TRUE)
  }

 purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{

   processo <- stringr::str_extract(.x,"\\d{20}")

   x <- xml2::read_html(.x)

   data <- x %>%
          xml2::xml_find_all("//table[@width='533'][3]//td[@width='84']/p/b") %>%
          xml2::xml_text() %>%
          tail(-1) %>%
          lubridate::dmy()

   movimentacao <- x %>%
     xml2::xml_find_all("//table[@width='533'][3]//td[@width='409']/p") %>%
     xml2::xml_text() %>%
     tail(-1) %>%
     iconv("UTF-8","LATIN1")


   sequencia <- x %>%
     xml2::xml_find_all("//table[@width='533'][3]//td[@width='36']/p") %>%
     xml2::xml_text() %>%
     tail(-1)

  tibble::tibble(processo=processo,sequencia,data,movimentacao) %>%
    tidyr::separate(movimentacao,c("principal","complemento"),sep="(?<=[:upper:])\\s*(?=\\w[:lower:])",remove=FALSE,extra="merge")

 }),NULL))


}
