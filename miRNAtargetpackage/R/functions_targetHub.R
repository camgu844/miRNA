# Required libraries:

# library(httr)
#library(XML)
# library(jsonlite)
# library(stringi)



#' Function that checks the status and parses the response.
#' @param req json object
#' @param ... other arguments

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
#' Note: the include_docs option is always TRUE
#' @param query_alt gene: Query by gene
#'					stem-loop miRNA: Query by stem-loop miRNA
#'					mature miRNA: Query by mature miRNA
#' @param search_option evidence count: Evidence Count
#'						specific method: Specific Methods
#' @param gene geneID character \emph{hsa-miR-212-3p} or numeric \emph{672}
#' @param data_source character \emph{miranda+mirtarbase+pictar4+targetscan} or numeric \emph{3}
#' @param atleast TRUE or FALSE document for each gene
#' @return targetHub object.
#' @examples
#' r = miRNA_target_interactions("gene", "evidence count", 672,  3)
#' r = miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212", 4)
#' r = miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p", 4)
#' r = miRNA_target_interactions("gene", "Specific Method", 672,  "miranda+pictar4+targetscan")

miRNA_target_interactions <- function(query_alt, search_option, gene, data_source, atleast=FALSE) {
	## preprocessing
  	query_alt = tolower(query_alt)
	search_option = tolower(search_option)

  	## parse url into GET query
  	base_url <- "http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/"
  	alt <- "NA"
  	if (query_alt == "gene") alt <- "by_geneID"
  	if (query_alt == "stem-loop mirna") alt <- "by_miRNAID"
  	if (query_alt == "mature mirna") alt <- "by_matureMIR"
  	stopifnot (!alt == "NA")

	option <- "NA"
	if (search_option == "evidence count") option <- "count"
	if (search_option == "specific method") option <- "method"
	stopifnot (!option == "NA")

	if (class(data_source) == "numeric"){
		query <- paste('["', gene, '",', data_source, ']', sep='')
	}else
		query <- paste('["', gene, '","', data_source, '"]', sep='')

	docs_str = '&include_docs=true'

	## create result
	ret <- list()
	ret$query_alt = query_alt
	ret$search_option = search_option
	ret$gene = gene
	ret$data_source = data_source
	class(ret) = "targetHub"

	if (isTRUE(atleast)){
		# stopifnot (class(data_source) == "numeric")
  		url <- paste(base_url, alt, option,'?startkey=', query, '&endkey=["', gene, '",{}]', docs_str, sep="")
		print(url)
		req = GET(url)
		ret$response = parse_response(req)
	}else {
  		url <- paste(base_url, alt, option,'?key=', query, docs_str, sep="")
		req = GET(url)
		ret$response = parse_response(req)
	}

	ret$url = url
	ret$json = jsonlite::prettify(jsonlite::toJSON(ret$response))

  	return(ret)
}

#' Function that prints the interactions.
#' @param x targetHub object
#' @param ... other arguments

print.targetHub = function(x, ...){
	res = x$response
	if (class(x$data_source) == "numeric") {
	  le <- length(res$rows)
	  if (le > 0){
	  for (i in 1:le) {
	  	print(paste("Interactions:", res$rows[[i]]$id,'-', res$rows[[i]]$value))
	    }
	  }
	}
	else {
		rows = res$rows
		le <- length(rows)
		if (le >0) {
		for (i in 1:le){
			r = rows[[i]]
			print(paste("Interaction:",r$id,'-',r$value))
		  }
		}
	}
}


#' Function that creates a list from a targetHub object.
#' @param x targetHub object
#' @param ... other arguments
#' @return list

as.list.targetHub = function(x,...){
  out <- list()
  res = x$response
  if (class(x$data_source) == "numeric") {
    le <- length(res$rows)
    if (le > 0){
    for (i in 1:le) {
      out[i]<-paste("Interactions:", res$rows[[i]]$id,'-', res$rows[[i]]$value)
      }
    }
  }
  else {
    rows = res$rows
    le <- length(rows)
    if(le>0){
    for (i in 1:length(rows)){
      r = rows[[i]]
      out[i]<-paste("Interaction:",r$id,'-',r$value)
      }
    }
  }
  return(out)
}

# Gene
if (FALSE){
	print('1-------------------')
	r = miRNA_target_interactions("gene", "evidence count", 672,  3, TRUE)
	print(r$url)
	print(as.list(r))

	print('2-------------------')
	r = miRNA_target_interactions("gene", "specific method", 672,  3, TRUE)
	print(r$url)
	print(as.list(r))

	print('3-------------------')
	r = miRNA_target_interactions("gene", "specific method", 672,  "mirtarbase", TRUE)
	print(r$url)
	print(as.list(r))

	print('4-------------------')
	r = miRNA_target_interactions("gene", "specific method", 672,  "mirtarbase", FALSE)
	print(r$url)
	print(as.list(r))
}

#  stem-loop miRNA
if (FALSE){
	print('5-------------------')
	r = miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212",  4)
	print(r$url)
	print(as.list(r))

	print('6-------------------')
	r = miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212",  4, TRUE)
	print(r$url)
	print(as.list(r))

	print('7-------------------')
	r = miRNA_target_interactions("stem-loop miRNA", "specific method", "hsa-mir-212", "pictar5")
	print(r$url)
	print(as.list(r))

	print('8-------------------')
	r = miRNA_target_interactions("stem-loop miRNA", "specific method", "hsa-mir-212", "miranda+mirtarbase+pictar4+targetscan", TRUE)
	print(r$url)
	print(as.list(r))
}

#   mature miRNA
if (FALSE){
	print('9-------------------')
	r = miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p",  4)
	print(r$url)
	print(as.list(r))

	print('10-------------------')
	r = miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p",  4, TRUE)
	print(r$url)
	print(as.list(r))

	print('11-------------------')
	r = miRNA_target_interactions("mature miRNA", "specific method", "hsa-miR-212-3p", "targetscan")
	print(r$url)
	print(as.list(r))

	print('12-------------------')
	r = miRNA_target_interactions("mature miRNA", "specific method", "hsa-miR-212-3p", "miranda+targetscan", TRUE)
	print(r$url)
	print(as.list(r))
}
