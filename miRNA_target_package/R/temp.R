library(httr)
library(jsonlite)

# In the end of the file are some useful functions.

# Commands from the targetHub API documentation home page

doc <- GET("http://app1.bioinformatics.mdanderson.org/tarhub/hsa-mir-212:672")

doc

#Response [http://app1.bioinformatics.mdanderson.org/tarhub/hsa-mir-212:672]
#Date: 2015-09-25 07:25
#Status: 200
#Content-Type: application/json
#Size: 4.94 kB
#{"_id":"hsa-mir-212:672","_rev":"34-7a981fb95a1024aed9d704e0af0bcc3e","Gene_Symbol":"BRCA1","miR_EntrezID":"406994","Gene_EntrezID":"...


basic_design_doc <- GET("http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic")

basic_design_doc
#Response [http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic]
#Date: 2015-09-25 07:30
#Status: 200
#Content-Type: application/json
#Size: 121 kB
#{"_id":"_design/basic","_rev":"492-fb5f0fcc1023d7e8404b4c9b6625b946","vendor":{"couchapp":{"evently":{"profile":{"profileReady":{"aft...

#When we do a function using httr include:
#warn_for_status()
#stop_for_status()

#To access the body of the request use content().

#(To figure out what the correct encoding should be use:
#stringi::stri_enc_detect(content(doc, "raw"))
#[[1]]
#[[1]]$Encoding
#[1] "ISO-8859-1" "ISO-8859-2" "UTF-8"      "UTF-16BE"   "UTF-16LE"   "Shift_JIS"  "GB18030"    "EUC-JP"     "EUC-KR"     "Big5"      
#[11] "ISO-8859-9"

#[[1]]$Language
#[1] "en" "pl" ""   ""   ""   "ja" "zh" "ja" "ko" "zh" "tr"

#[[1]]$Confidence
#[1] 0.32 0.17 0.15 0.10 0.10 0.10 0.10 0.10 0.10 0.10 0.05
#)

#JSON automatically parsed into named list. Write:
str(content(doc, "parsed"))

#Truncated result:
#
#List of 7
#$ _id          : chr "hsa-mir-212:672"
#$ _rev         : chr "34-7a981fb95a1024aed9d704e0af0bcc3e"
#$ Gene_Symbol  : chr "BRCA1"
#$ miR_EntrezID : chr "406994"
#$ Gene_EntrezID: chr "672"
#$ miRNA        : chr "hsa-mir-212"
#$ mat_miRNA    :List of 2
#..$ hsa-miR-212-3p:List of 4
#.. ..$ targetScan:List of 5
#.. .. ..$ NM_007297:List of 12
#.. .. .. ..$ totalConSites     : chr "1"
#.. .. .. ..$ nonCons7mer1Asites: chr "0"
#.. .. .. ..$ cons7mer1Asites   : chr "0"


str(content(basic_design_doc, "parsed"))

#Truncated result:
  
#  List of 11
#$ _id         : chr "_design/basic"
#$ _rev        : chr "492-fb5f0fcc1023d7e8404b4c9b6625b946"
#$ vendor      :List of 1
#..$ couchapp:List of 3
#.. ..$ evently :List of 3
#.. .. ..$ profile:List of 4
#.. .. .. ..$ profileReady:List of 3
#.. .. .. .. ..$ after   : chr "function(e, p) {\r\n  $$(this).profile = p;\r\n};"
#.. .. .. .. ..$ data    : chr "function(e, p) {\r\n  return p\r\n}"
#.. .. .. .. ..$ mustache: chr "<div class=\"avatar\">\r\n  {{#gravatar_url}}<img src=\"{{gravatar_url}}\"/>{{/gravatar_url}}\r\n  <div class=\"name\">\r\n    "| __truncated__
#.. .. .. ..$ noProfile   :List of 3
#.. .. .. .. ..$ data     : chr "function(e, userCtx) {\r\n  return userCtx;\r\n}"
#.. .. .. .. ..$ mustache : chr "<form>\r\n  <p>Hello {{name}}, Please setup your user profile.</p>\r\n  <label for=\"nickname\">Nickname \r\n    <input type=\""| __truncated__
#.. .. .. .. ..$ selectors:List of 1
#..$ name           : chr "tarHub: A RESTful resource for miRNA targets"
#..$ objects        : Named list()
#..$ description    : chr "A relaxed way to get information about miRNA targeting genes"
#..$ manifest       :List of 103

#To use the API, each of the "HTTP calls" should be preceeded by:
  
#http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/
  
  
#Get error message when running this code, why?:
  
#  Q_gene <- GET("http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/by_geneIDcount?key=["672",3]")
#Error: unexpected numeric constant in "Q_gene <- GET("http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/by_geneIDcount?key=["672"
       

                                                                                                                                               
                                                                                                                                               
######  Functions   ##############                                                                                                                                               
                                                                                                                                               
                                                                                                                                                                                                                                                                                             
# Function to execute GET                                                                                                                                               

targetHub_GET <- function("http://app1.bioinformatics.mdanderson.org/tarhub/_design/basic/_view/", path, ...) {
  res <- GET(path, ...)
  targetHub_check(res)
  res
}
  
# Check function:

targetHub_check <- function(res) {
  if (res$status_code < 400) return (invisible())
  else {
    message <- targetHub_parse(res)$message
  stop("HTTP failure: ", res$status_code, "\n", message, call. = FALSE)
  }
}


# Parse function:

targetHub_parse <- function(res) {
  text <- content(res, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector=FALSE)
}

                                                                                                                                                                                                                                                                                       
