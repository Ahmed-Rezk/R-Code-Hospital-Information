rankhospital <- function(state, outcome, num){
        outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome_clean <- na.omit(outcome_df)
        outcome_list <- c("heart attack","heart failure","pneumonia")
        if (outcome %in% outcome_list){
                print("vaild outcome")
        } else{
                stop("invalid outcome")
        }
        if (state %in% outcome_clean[ ,7]){
                print("vaild state")
        } else{
                stop("invalid state)")
        }
        outcome_clean <- outcome_clean[outcome_clean$State == state, ]
        if (outcome == "heart attack"){
                if(num == "best"){
                        num <- 1
                } else if(num == "worst"){
                        num <- max(order(as.numeric(outcome_clean[ ,11],na.rm=TRUE),outcome_clean[ ,2]))
                }
                row_rank <- order(as.numeric(outcome_clean[ ,11],na.rm=TRUE),outcome_clean[ ,2])[num]
        } else if(outcome == "heart failure"){
                if(num == "best"){
                        num <- 1
                } else if(num == "worst"){
                        num <- max(order(as.numeric(outcome_clean[ ,17],na.rm=TRUE),outcome_clean[ ,2]))
                }
                row_rank <- order(as.numeric(outcome_clean[ ,17],na.rm=TRUE),outcome_clean[ ,2])[num]
        } else if(outcome == "pneumonia"){
                if(num == "best"){
                        num <- 1
                } else if(num == "worst"){
                        num <- max(order(as.numeric(outcome_clean[ ,23],na.rm=TRUE),outcome_clean[ ,2]))
                }
                row_rank <- order(as.numeric(outcome_clean[ ,23],na.rm=TRUE),outcome_clean[ ,2])[num]
        }
        hospital_rank <- outcome_clean[row_rank,2]
        hospital_rank
}

rankhospital("NJ", "pneumonia", "worst")