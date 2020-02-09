best <- function(state, outcome) {
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
                row_best <- which(as.numeric(outcome_clean[ ,11],na.rm=TRUE) == min(as.numeric(outcome_clean[ ,11]),na.rm=TRUE))
        } else if(outcome == "heart failure"){
                row_best <- which(as.numeric(outcome_clean[ ,17],na.rm=TRUE) == min(as.numeric(outcome_clean[ ,17]),na.rm=TRUE))
        } else if(outcome == "pneumonia"){
                row_best <- which(as.numeric(outcome_clean[ ,23],na.rm=TRUE) == min(as.numeric(outcome_clean[ ,23]),na.rm=TRUE))
        }
        hospital_best <- outcome_clean[row_best,2]
        hospital_best
}

best("AK", "pneumonia")