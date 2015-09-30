## ----include=FALSE-------------------------------------------------------
library(miRNAtargetpackage)
library(jsonlite)
library(httr)

## ------------------------------------------------------------------------
miRNA_target_interactions("gene", "evidence count", 672,  3, TRUE)

## ------------------------------------------------------------------------
	miRNA_target_interactions("gene", "specific method", 672,  "mirtarbase", TRUE)
	
	miRNA_target_interactions("gene", "specific method", 672,  "mirtarbase", FALSE)

## ------------------------------------------------------------------------
miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212",  4)
	
miRNA_target_interactions("stem-loop miRNA", "evidence count", "hsa-mir-212",  4, TRUE)

## ------------------------------------------------------------------------
miRNA_target_interactions("stem-loop miRNA", "specific method", "hsa-mir-212", "pictar5")

## ------------------------------------------------------------------------
miRNA_target_interactions("stem-loop miRNA", "specific method", "hsa-mir-212", "miranda+mirtarbase+pictar4+targetscan", TRUE)

## ------------------------------------------------------------------------
miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p",  4)
	
miRNA_target_interactions("mature miRNA", "evidence count", "hsa-miR-212-3p",  4, TRUE)

## ------------------------------------------------------------------------
miRNA_target_interactions("mature miRNA", "specific method", "hsa-miR-212-3p", "targetscan")

## ------------------------------------------------------------------------
miRNA_target_interactions("mature miRNA", "specific method", "hsa-miR-212-3p", "miranda+targetscan", TRUE)

