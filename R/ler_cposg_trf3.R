#' Lê metadados processuais
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informados os arquivos
#'
#' @return tibble
#' @export
#'
ler_cposg_trf3<-
   function (arquivos = NULL, diretorio = ".")
   {
      if (is.null(arquivos)) {
         arquivos <- list.files(diretorio, "html", full.names = T)
      }
      purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
         x <- xml2::read_html(.x, encoding = "UTF-8")
         classe_processo <- x %>% xml2::xml_find_first("//p[@class=\"docTexto\"]") %>%
            xml2::xml_text(trim = T)
         sigla_classe <- stringr::str_extract(classe_processo,
                                              "\\w+")
         classe <- stringr::str_extract(classe_processo, "(?<=\\-\\s?).+?(?=\\-)") %>%
            stringr::str_trim()
         numero <- stringr::str_extract(classe_processo, "\\d+")
         origem <- stringr::str_extract(classe_processo, "(?<=/\\s?)\\w+")
         processo <- stringr::str_extract(classe_processo, "\\s\\S+$") %>%
            stringr::str_remove_all("\\D")
         relator <- x %>% xml2::xml_find_first("//div[contains(.,'Relator(a)')]/following-sibling::div") %>%
            xml2::xml_text(trim = T)
         orgao_julgador <- x %>% xml2::xml_find_first("//div[contains(.,'Órgão Julgador')]/following-sibling::div") %>%
            xml2::xml_text(trim = T)
         data_julgamento <- x %>% xml2::xml_find_first("//div[contains(.,'Data do Julgamento')]/following-sibling::div") %>%
            xml2::xml_text(trim = T) %>% lubridate::dmy()
         data_publicacao <- x %>% xml2::xml_find_first("//div[contains(.,'Data da Publicação/Fonte')]/following-sibling::div") %>%
            xml2::xml_text(trim = T) %>% stringr::str_extract("(?<=\\:).+") %>%
            lubridate::dmy()
         ementa <- x %>% xml2::xml_find_first("//div[contains(.,'Ementa')]/following-sibling::div") %>%
            xml2::xml_text(trim = T)
         dispositivo <- x %>% xml2::xml_find_first("//div[@class='docTitulo'][contains(.,'Acórdao')]/following-sibling::div") %>%
            xml2::xml_text(trim = T)
         resumo_estruturado <- x %>% xml2::xml_find_first("//div[@class='docTitulo'][contains(.,'Resumo Estruturado')]/following-sibling::div") %>%
            xml2::xml_text(trim = T)
         tibble::tibble(processo, classe, numero, origem, relator,
                        orgao_julgador, data_julgamento, data_publicacao,
                        ementa, dispositivo, resumo_estruturado)
      }), NULL))
   }
