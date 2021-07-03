#Final Assignment 3 July

rankall <- function(outcome = character(), num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #Reading in the data
        #data[,7] <- as.factor(data[,7])
        #state_lvl <- levels(as.factor(data[,7]))
        # if(!(state %in% state_lvl)) return(message("State not recognised"))
        # stateData <- data[data$State == state,] #Filtering out data for just the state specified
        
        t <- NULL
        if(outcome == "heart attack") t <- 11
        else if(outcome == "heart failure") t <- 17
        else if(outcome == "pneumonia") t <- 23
        else return(message("Outcome not recognised"))
        allHosp <- data[,c(2,7,t)] #Reducing data down to just the outcome specified and hospital name
        
        allHosp[[3]] <- as.numeric(allHosp[[3]])
        good <- complete.cases(allHosp[[3]])
        cleanHosp <- allHosp[good,] #Clean data generated. NA values removed
        cleanHosp <- cleanHosp[order(cleanHosp[,2],cleanHosp[,3],cleanHosp[,1]),] #Sorts all values by the state, rate and then hosp name
       
        if(num == "best") num <- 1 #Handling values for num
        else if(num != "worst" && num > length(cleanHosp$Hospital.Name)) return(NA)
        
        statesComplete <- character() #Expecting to receive all states evaluated
        rankedHosp <- data.frame(hospital = character(), state = character()) #Preparing to receive the final output
        runningRank <- 1 #keeping track of required rank
        for (i in 1:length(cleanHosp$Hospital.Name)) { #For all hospital records
                state <- as.character(cleanHosp$State[i]) #current state being worked on
                if(is.na(statesComplete[1])) statesComplete <- append(statesComplete, state) #Checking for first run of loop
                runningState <- statesComplete[length(statesComplete)] #State in previous run checked to handle rank
                
                if(state == runningState && i > 1 && i != length(cleanHosp$Hospital.Name)) runningRank <- runningRank + 1 #Initial run setup as well as necessary conditions to offset further ifelse
                else if(state != runningState) { #Resetting runningRank when state changes as well as worst rank handling when state changes
                        runningRank <- 1
                        statesComplete[length(statesComplete)+1] <- state #Capturing the completed state
                        if(num == "worst") rankedHosp[length(statesComplete),] <- cleanHosp[i-1,1:2] #Handling case for worst of a state
                }
                else if(i == length(cleanHosp$Hospital.Name) && num == "worst") rankedHosp[length(statesComplete)+1,] <- cleanHosp[i,1:2] #Special condition at the end of the loop to handle worst
                
                if(runningRank == num) rankedHosp[length(statesComplete),] <- cleanHosp[i,1:2] #Capturing hosp of required rank
        }
        rankedHosp
}
