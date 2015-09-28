library(httr)
library(jsonlite)

# A function that converts the user's request into a http call.

request <- function(query_alt, gene, search_option, data_source) {
  base_url <- "http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/"
  alt <- "NA"
  if (query_alt == "by gene") alt <- "by_geneID"
  if (query_alt == "by stem-loop miRNA") alt <- "by_miRNAID"
  if (query_alt == "by mature miRNA") alt <- "by_matureMIR"
  stopifnot (!alt == "NA")
  
  option <- "NA"
  if (search_option == "evidence count") option <- "count"
  if (search_option == "specific method") option <- "method"
  stopifnot (!option == "NA")
  
  stopifnot (class(gene) == "numeric")
  
  query <- paste('?key=["', gene, '",', data_source, ']', sep='')
  url <- paste(base_url, alt, option, query, sep="")
  url
  return(url)
  
}


#For example:
request("by gene", 672, "evidence count", 3)


# Function to execute GET                                                                                                                                               

targetHub_GET <- function(url, ...) {
  req <- GET(url, ...)
  targetHub_check(req)
  req
}


# Check function:

targetHub_check <- function(req) {
  if (req$status_code < 400) return (invisible())
  else {
    message <- targetHub_parse(req)$message
    stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
  }
}


# Parse function:

targetHub_parse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector=FALSE)
}

# A function to do all steps (not all steps in yet):

miRNA_target_interactions <- function (query_alt, gene, search_option, data_source) {
  url <- request(query_alt, gene, search_option, data_source)
  get_info <- targetHub_GET(url)
  get_info
}

#For example:
miRNA_target_interactions("by gene", 672, "evidence count", 3)

#results in:
#Response [http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/by_geneIDcount?key=["672",3]]
#Date: 2015-09-28 10:02
#Status: 200
#Content-Type: application/json
#Size: 119 B
#{"total_rows":4621385,"offset":3499033,"rows":[
#  {"id":"hsa-mir-132:672","key":["672",3],"value":"hsa-miR-132-3p"}
#  ]}
