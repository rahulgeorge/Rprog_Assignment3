#Begining Assigment 3 on 1 July 2021l

best <- function(state = character(), outcome = character()) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #Reading in the data
        data[,7] <- as.factor(data[,7])
        state_lvl <- levels(as.factor(data[,7]))
        if(!(state %in% state_lvl)) return(message("State not recognised"))
        stateData <- data[data$State == state,] #Filtering out data for just the state specified
        
        t <- NULL
        if(outcome == "heart attack") t <- 11
        else if(outcome == "heart failure") t <- 17
        else if(outcome == "pneumonia") t <- 23
        else return(message("Outcome not recognised"))
        mor <- stateData[,c(2,t)] #Reducing data down to just the outcome specified and hospital name
       
        mor[[2]] <- as.numeric(mor[[2]])
        good <- complete.cases(mor[[2]])
        cleanMor <- mor[good,] #Clean data generated. NA values removed
        
        min <- min(cleanMor[[2]]) #Finding the minimum
        count <- 0
        hosp <- character()
        for(i in 1:length(cleanMor[[2]])) { #Finding the hospital name for the minimum value
                if(cleanMor[[2]][i] == min) {
                        hosp <- append(hosp,cleanMor[[1]][i])
                        count <- count + 1
                }
        }
        hosp <- sort(hosp) #Finding the first hospital alphabetically
        print(paste0("Hospitals: ", hosp))
        print(paste0("Minimum rate: ", min))
        hosp[1]
}





