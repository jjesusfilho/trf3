#' Lê htmls baixados por trf3_baixar_cjsg
#'
#' @param arquivos Se forem informados, diretorio é ignorado.
#' @param diretorio Default para o atual
#'
#' @return tibble com informações processuais
#' @export
#'
#' @examples
#' \dontrun{
#' ler_cjsg_trf3(diretorio = ".")
#' }
trf3_ler_cjsg <- function(arquivos = NULL, diretorio = ".") {

  if (is.null(arquivos)){
    arquivos <- list.files(path = diretorio, pattern = ".html", full.names = TRUE)
  }



    pb <- progress::progress_bar$new(total = length(arquivos))

    df <- purrr::map_dfr(arquivos, ~ {

      pb$tick()

      suppressMessages({

    doc <- xml2::read_html(.x, encoding = "UTF-8")

    variaveis <- doc %>%
      xml2::xml_find_all("//div[@id='blocoesquerdo'][span]") %>%
      purrr::map(~xml2::xml_children(.x) %>% xml2::xml_attr("class"))

    valores <- doc %>%
      xml2::xml_find_all("//div[@id='blocoesquerdo'][span]") %>%
      purrr::map(~xml2::xml_children(.x) %>% xml2::xml_text(trim = TRUE))

    df <- purrr::map2_dfr(variaveis,valores,~{
      t(.y) %>%
        tibble::as_tibble() %>%
        purrr::set_names(.x)

    })

    df$ementa <-  xml2::read_html(.x) %>%
      xml2::xml_find_all("//div[@id='blocodireito']") %>%
      xml2::xml_text(trim = T)

    df
  })

    df <- df %>%   dplyr::select(classe = linkdocumento, orgao_julgador = tipodocumento,
                  relator, fonte,data_decisao = decisao, ementa) %>%
    tidyr::separate(classe,c("codigo_classe","classe"),sep = "-") %>%
    #dplyr::mutate_at(dplyr::vars(2,3),~iconv(.,"utf8","latin1//TRANSLIT")) %>%
    tidyr::separate(fonte,c("fonte","data_publicacao"),sep="DATA:\\s?") %>%
    dplyr::mutate(data_publicacao = lubridate::dmy(data_publicacao),
                  data_decisao = stringr::str_extract(data_decisao,"\\d.+") %>%
                    lubridate::dmy()) %>%
    dplyr::mutate_if(is.character,list(~stringr::str_squish(.))) %>%
    dplyr::mutate(ementa = stringr::str_remove(ementa,"(?i)^e\\s?m\\s?e\\s?n\\s?t\\s?a")) %>%
    dplyr::mutate(cargo_relator = stringr::str_extract(relator,"(?i)(Desembargadora? Federal|juiza? federal convocad[oa]|juiza? convocad[oa])"),
                  relator = stringr::str_remove(relator,"(?i)(Desembargadora? Federal|juiza? federal convocad[oa]|juiza? convocad[oa])")) %>%
    dplyr::select(c("codigo_classe", "classe", "orgao_julgador","cargo_relator" ,"relator", "fonte",
                    "data_publicacao", "data_decisao", "ementa"))

})
    }
