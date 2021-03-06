% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/functions_targetHub.R
\name{miRNA_target_interactions}
\alias{miRNA_target_interactions}
\title{targetHub is a database of miRNA-mRNA interactions. The interaction data is
obtained various external data sources and in some cases computed in-house
by algorithms implemented for miRNA target prediction.
Note: the include_docs option is always TRUE}
\usage{
miRNA_target_interactions(query_alt, search_option, gene, data_source,
  atleast = FALSE)
}
\arguments{
\item{query_alt}{gene: Query by gene
stem-loop miRNA: Query by stem-loop miRNA
mature miRNA: Query by mature miRNA}

\item{search_option}{evidence count: Evidence Count
specific method: Specific Methods}

\item{gene}{geneID character \emph{hsa-miR-212-3p} or numeric \emph{672}}

\item{data_source}{character \emph{miranda+mirtarbase+pictar4+targetscan} or numeric \emph{3}}

\item{atleast}{TRUE or FALSE document for each gene}
}
\value{
targetHub object.
}
\description{
targetHub is a database of miRNA-mRNA interactions. The interaction data is
obtained various external data sources and in some cases computed in-house
by algorithms implemented for miRNA target prediction.
Note: the include_docs option is always TRUE
}
\examples{
r = miRNA_target_interactions("gene", "evidence count", 672,  3)
r = miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212", 4)
r = miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p", 4)
r = miRNA_target_interactions("gene", "Specific Method", 672,  "miranda+pictar4+targetscan")
}

