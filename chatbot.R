# reset
# phrases <<- c("talk to me!", "yes", "no")
# score_matrix <<- matrix(1, 3, 3)

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
    print("score_matrix table:")
    print(table(as.vector(score_matrix)))
    print("score_matrix size:")
    print(object.size(score_matrix), units = "auto")
    print("phrases size:")
    print(object.size(phrases), units = "auto")
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

# selects the largest jaccard similarity that is not identical
jaccard_select <- function(input){
    splitted_input <- unlist(strsplit(input," "))
    results <- sapply(strsplit(phrases," "), jaccard, splitted_input)
    results[results == max(results)] <- -1
    return(sample(phrases[which(results == max(results))], 1))
}

# TODO create logg of selection process


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
            print("6: check input for likely output")
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
run_chatbot <- function(){
    # load data 
    score_matrix <<- unname(as.matrix(read.csv("data/chatbot_matrix.csv")[-1]))
    phrases <<- as.character(unlist(read.csv("data/chatbot_phrases.csv")[-1]))
    selection_log <<- c()
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
            selection_log <<- c(selection_log, "random phrase")
            output <<- new_phrase()
            if(!(output %in% phrases)){add_new_input(output)}
        }
        
        # selects from all phrases over value 0, 1% chance
        else if(sample(c(T, rep(F, 99)), 1)){
            selection_log <<- c(selection_log, "all over 0 phrases")
            output <<- select_from_all(input)}

        # selects from all phrases over value 1
        else if(sum(score_matrix[phrases == input, ]>1)>0){
            selection_log <<- c(selection_log, "all over 1 phrases")
            output <<- advance_select(input)}
                

        # selects with Jaccard similarity
        else{output <<- jaccard_select(input)
          selection_log <<- c(selection_log, "Jaccard selection")}
                

    print(output)
    }
    print("Chatbot left")
    # save data
    write.csv(score_matrix, "data/chatbot_matrix.csv")
    write.csv(phrases, "data/chatbot_phrases.csv")
}

#run_chatbot()
