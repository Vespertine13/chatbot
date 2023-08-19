# remember the arrow package
# get config
PATH <- paste0(PATH_BASE, "/mega/data/parla/")
source(paste0(PATH_BASE, "/parla/config.R"))

# check if file exists and create if not
if(!file.exists(paste0(PATH, "chatbot_matrix.parquet")) | !file.exists(paste0(PATH, "chatbot_matrix.parquet"))){
    phrases <<- c("talk to me!", "yes", "no")
    score_matrix <<- matrix(1, 3, 3)
    write.csv(phrases, paste0(PATH, "chatbot_phrases.csv"))
    arrow::write_parquet(as.data.frame(score_matrix), paste0(PATH, "chatbot_matrix.parquet"))
}


# test data
run_stats <- function(){
    print("mean score:")
    print(mean(score_matrix))
    print("nrow score_matrix:")
    print(nrow(score_matrix))
    print("ncol score_matrix")
    print(ncol(score_matrix))
    print("Number of phrases:")
    print(length(phrases))
}

# plot score distribution
plot_score_matrix <- function(){
    options(scipen=999)
    plot(table(as.vector(score_matrix)), ylab="Occurences", xlab="Score")
}

# rows refers to input
# columns refer to output
add_new_input <- function(char){
    score_matrix <<- rbind(score_matrix, 1)
    score_matrix <<- cbind(score_matrix, 1)
    phrases <<- c(phrases, char)}

# delete phrase
remove_phrase <- function(char){
    idx <- which(phrases==char)
    score_matrix <<- score_matrix[-idx, -idx]
    phrases <<- phrases[-idx]
}

# give feedback
give_feedback <- function(input, output){
    # add +1 to the response written by the user
    score_matrix[phrases == output, phrases == input] <<- score_matrix[phrases == output, phrases == input] + 1
}

# creates a new phrase
new_phrase <- function(){
    return(paste(sample(unique(unlist(strsplit(
        phrases, split=" "))), sample(1:10, 1)), collapse=" "))
}


# selects a responce from the matrix
# select from all over 0
select_from_all <- function(input){   
    score <- score_matrix[phrases == input, ]
    score[score<0] <- 0
    sample_lst <- c()
    for(i in 1:length(score)){
        sample_lst <- c(sample_lst, rep(phrases[i],score[i]))
    }
    return(sample(sample_lst, 1))
}

# select from all over 1
advance_select <- function(input){
    score <- score_matrix[phrases == input, ]
    score[score<2] <- 0
    sample_lst <- c()
    for(i in 1:length(score)){
        sample_lst <- c(sample_lst, rep(phrases[i],score[i]))
    }
    return(sample(sample_lst, 1))
}

# Jaccard similarity
jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
}

# selects the largest jaccard similarity that is not identical as input
jaccard_select <- function(input){
    splitted_input <- unlist(strsplit(input," "))
    results <- sapply(strsplit(phrases," "), jaccard, splitted_input)
    results[results == max(results)] <- -1
    jaccard_input <<- sample(phrases[which(results == max(results))], 1)
    score <- score_matrix[phrases == jaccard_input, ]
    if(sum(score>1)>0){
        jaccard_output <<- advance_select(jaccard_input)
        if(jaccard_output == input){
            return(jaccard_input)
        }        
        else{
            return(jaccard_output)
        }
    }
    else{
        return(jaccard_input)
    }
}

# check if the bot repeats itself
bot_repeats <- function(output, last_output){
    if(output == last_output){
        return(TRUE)
    }else{
        return(FALSE)
    }
}

# dont answer a question with a question
answers_q_with_q <- function(input, output){
    if(grepl("?", input, fixed = TRUE) & grepl("?", output, fixed = TRUE)){
        return(TRUE)
    }else{
        return(FALSE)
    }
}


# command mode
command_mode <- function(){
    cmd <<- "start"
    while(cmd != "0"){
        cmd <<- readline(prompt = "cmd: ")
        if(cmd == "0"){print("command mode ended")}
        else if(cmd == "1"){
            print("0: stop command mode")
            print("1: list codes")
            print("2: delete last output")
            print("3: get stats")
            print("4: check if phrase is saved")
            print("5: check likely output of input")
            print("6: check possible input for output")
            print("7: delete specific phrase")
            print("8: print current output")
            print("9: print 10 newest phrases")
            print("10: print 10 last selection types")
        }
        else if(cmd == "2"){remove_phrase(output)
            print(paste("Removed", output))
            output <<- ""
        }
        else if(cmd == "3"){run_stats()}
        else if(cmd == "4"){check_phrase  <- readline(prompt = "type phrase: ")
            if(check_phrase %in% phrases){
                print(paste("Phrase is in position", which(phrases==check_phrase)))
            }
            else{print("Phrase is not saved")}
        }
        else if(cmd == "5"){check_phrase  <- readline(prompt = "type phrase: ")
            if(check_phrase %in% phrases){
                score_vector <- score_matrix[phrases == check_phrase,]
                print(rbind(phrases[score_vector > 1],score_vector[score_vector > 1]))
            }
            else{print("Phrase is not saved")}
        }
        else if(cmd == "6"){check_phrase  <- readline(prompt = "type phrase: ")
            if(check_phrase %in% phrases){
                score_vector <- score_matrix[,phrases == check_phrase]
                print(rbind(phrases[score_vector > 1],score_vector[score_vector > 1]))
            }
            else{print("Phrase is not saved")}
        }
        else if(cmd == "7"){phrase_to_remove <<- readline(prompt = "type phrase: ")
            if(phrase_to_remove %in% phrases){
                remove_phrase(phrase_to_remove)
                print(paste("Removed", phrase_to_remove))
                output <<- ""
            }
            else{print("Phrase is not saved")}
        }
        else if(cmd == "8"){print(output)}
        else if(cmd == "9"){print(tail(phrases, 10))}
        else if(cmd == "10"){print(tail(selection_log))}
        else{print("command unknown")}
    }
}



# chatbot function
run_training <- function(){
    # load data 
    score_matrix <<- unname(as.matrix(arrow::read_parquet(paste0(PATH, "chatbot_matrix.parquet"))))
    phrases <<- as.character(unlist(read.csv(paste0(PATH, "chatbot_phrases.csv"))[-1]))
    selection_log <<- c("start_log")
    output_log <<- c("start_log")
    # set start input and output values
    input <<- "none"
    output <<- "talk to me!"
    print(output)
    while(!grepl("bye", input)){
        input <<- readline(prompt = "Write something: ")
        input <<- tolower(input)

        # command mode
        if(input == "command mode"){
            command_mode()
            input <<- readline(prompt = "Write something: ")
            input <<- tolower(input)
        }

        # adds the word if it isn't already in phrases
        if(!(input %in% phrases)){add_new_input(input)}
        give_feedback(input, output)

        # adds a new random phrase, 1% chance
        if(sample(c(T, rep(F, 99)), 1)){
            current_selection <<- "random phrase"
            output <<- new_phrase()
        }
        
        # selects from all phrases over value 0, 1% chance
        else if(sample(c(T, rep(F, 99)), 1)){
            current_selection <<-  "all over 0 phrases"
            output <<- select_from_all(input)
        }

        # selects from all phrases over value 1
        else if(sum(score_matrix[phrases == input, ]>1)>0){
                current_selection <<- "all over 1 phrases"
                output <<- advance_select(input)
        }
        
        # selects with Jaccard similarity
        else{
                    output <<- jaccard_select(input)
                    current_selection <<- "Jaccard selection"            
                }
        if(answers_q_with_q(input, output)){
            current_selection <<- "all over 0 phrases"
            output <<- select_from_all(input)
        }
        
        # checks if bot repeats you
        if(bot_repeats(output, input)){
            current_selection <<- "all over 0 phrases"
            output <<- select_from_all(input)
        }
        # checks if bot repeats herself
        if(bot_repeats(output, output_log[length(output_log)])){
            current_selection <<- "all over 0 phrases"
            output <<- select_from_all(input)
        }

        
        selection_log <<- c(selection_log, current_selection)
        if(!(output %in% phrases)){add_new_input(output)}
        print(output)
        output_log <<- c(output_log, output)
    }
    print("Saving data...")
    # save data
    arrow::write_parquet(as.data.frame(score_matrix), paste0(PATH, "chatbot_matrix.parquet"))
    write.csv(phrases, paste0(PATH, "chatbot_phrases.csv"))
    print("Chatbot left")
}

# run parla for use with elisp function 
# it is similar to run_training()
run_parla <- function(input){
    input <- tolower(input)
    output <<- readLines(paste0(PATH, "output.txt"))
    if (length(output) == 0){output <- ""}
    output_last <- output
    score_matrix <<- unname(as.matrix(arrow::read_parquet(paste0(PATH, "chatbot_matrix.parquet"))))
    phrases <<- as.character(unlist(read.csv(paste0(PATH, "chatbot_phrases.csv"))[-1]))
    if(!(input %in% phrases)){add_new_input(input)}
    give_feedback(input, output)

    if(sample(c(T, rep(F, 99)), 1)){
        output <- new_phrase()
    }
    else if(sample(c(T, rep(F, 99)), 1)){
        output <- select_from_all(input)
    }
    else if(sum(score_matrix[phrases == input, ]>1)>0){
        output <- advance_select(input)
    }
    else{
        output <- jaccard_select(input)
    }
    if(answers_q_with_q(input, output)){ #check is bot answers a question with a question (should be rare but not impossible)
        output <- select_from_all(input)
    }
    if(bot_repeats(output, output_last)){
        output <- select_from_all(input)
        }
    if(bot_repeats(output, input)){ #check is bot repeats what you said (should be rare but not impossible)
        output <- select_from_all(input)
    }
    arrow::write_parquet(as.data.frame(score_matrix), paste0(PATH, "chatbot_matrix.parquet"))
    write.csv(phrases, paste0(PATH, "chatbot_phrases.csv"))
    writeLines(output,con=paste0(PATH, "output.txt"))
    return(output)
}
