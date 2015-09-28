# require library:
# httr
# XML
# jsonlite

# Check function:
parse_response <- function(req) {
	status = status_code(req)
	stop_for_status(status)

	if (status < 200){
		print("Wait for more information!")
	}else if (status < 300){
		print("Success!")
	}

	possible_encode = stringi::stri_enc_detect(content(req, "raw"))
  	text <- content(req, as = "parsed", encoding = possible_encode[[1]]$Encoding[1])
  	if (identical(text, "")) stop("No output to parse", call. = FALSE)
  	return(text)
}

#' targetHub is a database of miRNA-mRNA interactions. The interaction data is
#' obtained various external data sources and in some cases computed in-house
#' by algorithms implemented for miRNA target prediction.
#'
#' @param query_alt gene: Query by gene
#'					stem-loop miRNA: Query by stem-loop miRNA
#'					mature miRNA: Query by mature miRNA
#' @param search_option evidence count: Evidence Count
#'						specific method: Specific Methods
#' @param gene geneID character \emph{hsa-miR-212-3p} or numeric \emph{672}
#' @param data_source character \emph{miranda+mirtarbase+pictar4+targetscan} or numeric \emph{3}
#' @param include_docs TRUE or FALSE document for each gene
#' @return targetHub object.
#' @examples
#' tmp = miRNA_target_interactions("gene", "evidence count", 672,  3)
#' tmp = miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212", 4)
#' tmp = miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p", 4)
#' tmp = miRNA_target_interactions("gene", "Specific Method", 672,  "miranda+pictar4+targetscan")
#' # supported by exactly three data sources/methods
#' x = tmp$extract()
#' # supported by atleast three data sources/methods
#' x = tmp$atleast()
miRNA_target_interactions <- function(query_alt, search_option, gene, data_source, include_docs=FALSE) {
	## parse url into GET query
  	base_url <- "http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/"
  	alt <- "NA"
  	query_alt = tolower(query_alt)
  	if (query_alt == "gene") alt <- "by_geneID"
  	if (query_alt == "stem-loop mirna") alt <- "by_miRNAID"
  	if (query_alt == "mature mirna") alt <- "by_matureMIR"
  	stopifnot (!alt == "NA")

	option <- "NA"
	search_option = tolower(search_option)
	if (search_option == "evidence count") option <- "count"
	if (search_option == "specific method") option <- "method"
	stopifnot (!option == "NA")

	if (class(data_source) == "numeric"){
		query <- paste('?key=["', gene, '",', data_source, ']', sep='')
	}else
		query <- paste('?key=["', gene, '","', data_source, '"]', sep='')

	docs_str = ''
	if (isTRUE(include_docs))
		docs_str = '&include_docs=true'
  	url <- paste(base_url, alt, option, query, sep="")
	# print(url)

	if (class(gene) == "character")
		gene = tolower(gene)
	if (class(data_source) == "character")
		data_source = tolower(data_source)

	## create result
	ret <- list()
	ret$url = url
	class(ret) = "targetHub"

	ret$extract = function(){
	  	url = paste(url, docs_str, sep="")
	  	print(url)
		req = GET(url)
		res = parse_response(req)
	}

	ret$atleast = function(){
		stopifnot (class(data_source) == "numeric")

		url = paste(url,'&endkey=["', gene, '",{}]', docs_str, sep="")
		print(url)
		req = GET(url)
		res = parse_response(req)
	}
  	return(ret)
}

tmp = miRNA_target_interactions("gene", "evidence count", 672,  3)
x = tmp$extract()
x
x$rows[[1]]$value

tmp = miRNA_target_interactions("gene", "evidence count", 672,  3, TRUE)
x = tmp$atleast()
print(length(x$rows))
print(x$rows[[1]]$value)
# [1] "hsa-miR-132-3p"
print(x$rows[[2]]$value)
# [1] "hsa-miR-212-3p"

# tmp = miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212", 4)
# x = tmp$extract()
# tmp = miRNA_target_interactions("mature miRNA", "evidence count", "HSA-miR-212-3p", 4)
# x = tmp$extract()
# tmp = miRNA_target_interactions("gene", "Specific Method", 672,  "miranda+pictar4+TargeTscan")
# x = tmp$extract()
