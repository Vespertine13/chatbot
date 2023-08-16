setwd("c:/Users/ekb/AppData/Roaming/parla")
source("R/main.R")

score_matrix <- unname(as.matrix(arrow::read_parquet(paste0(PATH, "chatbot_matrix.parquet"))))
phrases <- as.character(unlist(read.csv(paste0(PATH, "chatbot_phrases.csv"))[-1]))

simple_run <- function(input = "hello"){
    if(sample(c(T, rep(F, 99)), 1)){
        output <- new_phrase()
    }else if(sample(c(T, rep(F, 99)), 1)){
        output < select_from_all(input)
    }else if(sum(score_matrix[phrases == input, ]>1)>0){
        output <- advance_select(input)
    }else{
        output <- jaccard_select(input)   
    }
    if(answers_q_with_q(input, output)){
        output <- select_from_all(input)
    }
    if(bot_repeats(output, input)){
        output <- select_from_all(input)
    }
    print(output)
}

simple_run()
