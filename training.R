cwd <- getwd()
PATH_BASE <- substr(1, nchar(cwd)-6, x = cwd)
source("R/main.R")
run_training()
