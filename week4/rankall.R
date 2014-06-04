rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        #Initialize the database
        data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        hospital<-read.csv("hospital-data.csv", colClasses="character")
        
        data$State<-as.factor(data$State)
        data[,11]<-as.numeric(data[,11])
        data[,17]<-as.numeric(data[,17])#Heart failure 30-day Death Rate
        data[,23]<-as.numeric(data[,23])#Pneumonia 30-day Death Rate 
        
        #Initialize the output value
        answer<-data.frame(hospital=integer(),state=integer())
        answer$hospital<-as.character(answer$hospital)
        answer$state<-as.character(answer$state)
        colnames(answer)<-c("hospital","state")
        
        if(num=="best"){
                if(outcome=="heart attack"){ 
                        
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                w<-range(data2[,11],na.rm=TRUE)
                                q<-which.min(data2[,11])
                                answer[i,1]<-as.character(data2$Hospital.Name[q])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer) }
                
                else if(outcome=="heart failure"){
                        
                        
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                w<-range(data2[,17],na.rm=TRUE)
                                q<-which.min(data2[,17])
                                answer[i,1]<-as.character(data2$Hospital.Name[q])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer)}
                
                else if(outcome=="pneumonia"){
                        
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                w<-range(data2[,23],na.rm=TRUE)
                                q<-which.min(data2[,23])
                                answer[i,1]<-as.character(data2$Hospital.Name[q])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer) }
                
                else{stop("invalid outcome")}
                
        }
        
        if(num=="worst"){ 
                if(outcome=="heart attack"){ 
                        
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                
                                m<-max(data2[,11],na.rm=TRUE)
                                data2_2<-subset(data2,data2[,11]==m)
                                sort.data2_2 <- data2_2[order(data2_2$Hospital.Name) , ]
                                
                                answer[i,1]<-as.character(sort.data2_2$Hospital.Name[1])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer)}
                
                else if(outcome=="heart failure"){
                        
                        
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                m<-max(data2[,17],na.rm=TRUE)
                                data2_2<-subset(data2,data2[,17]==m)
                                sort.data2_2 <- data2_2[order(data2_2$Hospital.Name) , ]
                                
                                answer[i,1]<-as.character(sort.data2_2$Hospital.Name[1])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer) }
                
                else if(outcome=="pneumonia"){
                        
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                m<-max(data2[,23],na.rm=TRUE)
                                data2_2<-subset(data2,data2[,23]==m)
                                sort.data2_2 <- data2_2[order(data2_2$Hospital.Name) , ]
                                
                                answer[i,1]<-as.character(sort.data2_2$Hospital.Name[1])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer) }
                
                else{stop("invalid outcome")}
        }
        
        if(is.numeric(num)==TRUE){
                
                
                if(outcome=="heart attack"){
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                v<-order(data2[,11])
                                
                                data2_2<-subset(data2,data2[,11]==data2[,11][v[num]])
                                sort.data2_2 <- data2_2[order(data2_2$Hospital.Name) , ]
                                
                                answer[i,1]<-as.character(sort.data2_2$Hospital.Name[1])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer)
                }
                
                else if(outcome=="heart failure"){
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                v<-order(data2[,17])
                                
                                data2_2<-subset(data2,data2[,17]==data2[,17][v[num]])
                                sort.data2_2 <- data2_2[order(data2_2$Hospital.Name) , ]
                                
                                answer[i,1]<-as.character(sort.data2_2$Hospital.Name[1])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer)
                        
                }
                
                else if(outcome=="pneumonia"){
                        
                        for(i in 1:length(levels(data$State))){
                                data2<-subset(data,data$State==levels(data$State)[i])
                                data2$Hospital.Name<-as.factor(data2$Hospital.Name)
                                v<-order(data2[,23])
                                
                                data2_2<-subset(data2,data2[,23]==data2[,23][v[num]])
                                sort.data2_2 <- data2_2[order(data2_2$Hospital.Name) , ]
                                
                                answer[i,1]<-as.character(sort.data2_2$Hospital.Name[1])
                                answer[i,2]<-as.character(levels(data$State)[i])
                                
                        }
                        rownames(answer)<-levels(data$State)
                        print(answer)
                        
                }
                
        }
        rownames(answer)<-levels(data$State)
        
        return(answer) 
        
}