# @knitr prep_files
library(alfresco)
domain <- "Statewide" # "Noatak" # Two options for domains.
run.name <- "fmo00s00i_historical_CRU32"
owner <- "paul.duffy@neptuneinc.org"
outputs_as_inputs(domain, run.name, 1949, owner)
outputs_as_inputs(domain, run.name, 2013, owner)
