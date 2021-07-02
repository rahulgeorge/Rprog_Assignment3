rankhospital <- function(state = character(), outcome = character(), num) {
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
        cleanMor <- cleanMor[order(cleanMor[,2],cleanMor[,1]),] #Ordering by ascending values of mortality rate
       
        if(num == "best") num <- 1      #Handling values for num
        else if(num == "worst") num <- length(cleanMor$Hospital.Name)
        else if(num > length(cleanMor$Hospital.Name)) return(NA)
        
        # val <- as.numeric(cleanMor[num,][2]) #Calculates the value of the return hospital
        # hosp <- character() #Planned for receiving the list of hospitals with same value as original target position
        # pos <- numeric() #Planned for receiving the position of all hospitals collected in the original list
        # for(i in 1:length(cleanMor$Hospital.Name)) { #Collecting all hospitals with the same target value
        #         tempVal <- as.numeric(cleanMor[i,][2])
        #         if(val == tempVal) {
        #                 pos <- append(pos,i)
        #                 hosp <- append(hosp, as.character(cleanMor$Hospital.Name[i]))
        #         }
        # }
        
        #hosp <- sort(hosp) #Sorts the hospitals
        #if(length(pos) > 1 ) realPos <- (num - pos[1])+1 #If multiple positions are recorded, selecting the actual position of the rank in the new hospital vector
        #hosp[realPos]
        
        hosp <- as.character(cleanMor[num,][1])
        hosp
}




