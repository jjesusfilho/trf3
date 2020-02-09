#' Cria tibbles dados e partes organizados
#'
#' @param df Lido com ler_cpopg_trf3
#'
#' @return Dois tibbles: dados_cpopg_trf3 e partes_cpopg_trf3
#' @export
#'
organizar_cpopg_trf3 <- function(df){

  nomes <-  c("proc", "adv", "assunto", "autor", "classe", "data_protocolo",
              "localizacao", "processo", "reu", "secretaria", "situacao", "tipo_distribuicao",
              "valor_causa", "volume_s", "embargado", "embargante", "num_antiga",
              "executado", "exequente")

  nomes_dados <- c("proc","assunto","classe","data_protocolo","localizacao",
                   "processo","secretaria","situacao","tipo_distribuicao","valor_causa",
                   "volume_s","num_antiga")

  nomes_partes <- setdiff(nomes,nomes_dados) %>% c("proc",.)


  dados <- df %>%
    dplyr::select(tidyselect::all_of(nomes_dados)) %>%
    dplyr::distinct(proc,.keep_all=TRUE) %>%
    dplyr::mutate(valor_causa=numero(valor_causa)) %>%
    dplyr::mutate(data_protocolo=lubridate::dmy(data_protocolo)) %>%
    tidyr::separate(classe,c("classe_numero","classe"),sep = "\\s\\.\\s") %>%
    tidyr::separate(tipo_distribuicao,
                    c("tipo_distribuicao","data_distribuicao"),
                    sep="\\sem\\s") %>%
    dplyr::mutate(data_distribuicao = lubridate::dmy(data_distribuicao)) %>%
    tidyr::separate(secretaria,c("vara","secretaria"),sep = "\\s/\\s") %>%
    tidyr::separate(secretaria,c("uf","secao_judiciaria"),sep="\\s-\\s")


  partes <- df %>%
    dplyr::select(tidyselect::all_of(nomes_partes))

  partes<-partes %>%
    tidyr::pivot_longer(-proc) %>%
    stats::na.omit()

  lista <- list(dados_cpopg_trf3 = dados,partes_cpopg_trf3=partes)

  list2env(x = lista, envir = .GlobalEnv, parent = .GlobalEnv)

}
