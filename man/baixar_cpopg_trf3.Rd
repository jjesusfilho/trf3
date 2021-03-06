% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baixar_cpopg_trf3.R
\name{baixar_cpopg_trf3}
\alias{baixar_cpopg_trf3}
\title{Baixar processos de primeiro grau do TRF3}
\usage{
baixar_cpopg_trf3(processos = NULL, movimentacao = TRUE, diretorio = ".")
}
\arguments{
\item{processos}{Vetor com os números dos dos processos}

\item{movimentacao}{Baixar movimentacao completa? Default: TRUE}

\item{diretorio}{Diretório para onde vão os htmls}
}
\value{
htmls
}
\description{
Baixar processos de primeiro grau do TRF3
}
\examples{
\dontrun{
dir.create("cpopg")
baixar_cpopg_trf3(processos = "0014367-81.2016.4.03.6100",diretorio="cpopg")

}
}
