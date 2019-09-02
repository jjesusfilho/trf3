#' Função usada para selecionar apenas variáveis com algum valor
#' @param x variaveis
is_na <- function(x) {
  !is.na(x) %>% sum() == length(x)
  }
