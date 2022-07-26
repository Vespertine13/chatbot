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

# 

# rows refers to input
# columns refer to output
add_new_input <- function(char){
    score_matrix <<- rbind(score_matrix, 1)
    score_matrix <<- cbind(score_matrix, 1)
    phrases <<- c(phrases, char)}

# auto feedback
auto_feedback <- function(input, output){
    score <- sample(c(1, 10, 100, 1000), 1)
    # add to response
    score_matrix[phrases == output, phrases == input] <<- score_matrix[phrases == output, phrases == input] + score
    # penalize random unused response
    select_penalty <- sample(1:ncol(score_matrix), 1)
    score_matrix[phrases == output, select_penalty] <<- score_matrix[phrases == output, select_penalty] - score
}

# creates a new phrase
new_phrase <- function(){
    return(paste(sample(unique(unlist(strsplit(
        phrases, split=" "))), sample(1:10, 1)), collapse=" "))
}


# selects a responce from the matrix
# select from all over 0
simple_select <- function(input){   
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



# chatbot function
run_chatbot <- function(){
    # load data 
    score_matrix <<- unname(as.matrix(read.csv("data/chatbot_matrix.csv")[-1]))
    phrases <<- as.character(unlist(read.csv("data/chatbot_phrases.csv")[-1]))
    # set start input and output values
    input <- "none"
    output <- "talk to me!"
    print(output)
    while(!grepl("bye", input)){
        input <- readline(prompt = "Write something: ")
        input <- tolower(input)

        # adds the word if it isn't already in phrases
        if(!(input %in% phrases)){add_new_input(input)}
        auto_feedback(input, output)

        # very rarely adds a new random phrase, 1% chance
        if(sample(c(T, rep(F, 99)), 1)){
            output <- new_phrase()
            add_new_input(output)
        }

        # selects from all phrases over value 0, 10% chance
        else if(sample(c(T, rep(F, 9)), 1)){output <- simple_select(input)}

        # selects from all phrases over value 1
        else if(sum(score_matrix[phrases == input, ]>1)>0){output <- advance_select(input)}

        # selects from all phrases over value 0
        else{output <- simple_select(input)}
        print(output)
    }
    print("Chatbot left")
    # save data
    write.csv(score_matrix, "data/chatbot_matrix.csv")
    write.csv(phrases, "data/chatbot_phrases.csv")
}

#run_chatbot()
