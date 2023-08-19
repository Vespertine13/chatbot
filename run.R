# for the elisp function
args <- commandArgs(trailingOnly = TRUE)
PATH_BASE <- args[2]
input <- args[1]


# PATH_BASE <- "c:/Users/eivin/AppData/Roaming"
# input <- "what is your name?"

source(paste0(PATH_BASE, "/parla/R/main.R"))
run_parla(input)
