rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        source('~/InternetCourses/Computing_for_Data_Analysis/ProgAssignment2-data/worst.R')
        #Initialize the database
        out_come<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        hospital<-read.csv("hospital-data.csv", colClasses="character")
        out_come.hospital<-merge(out_come,hospital,by="Provider.Number")
        out_come[,11]<-as.numeric(out_come[,11])
        out_come[,17]<-as.numeric(out_come[,17])#Heart failure 30-day Death Rate
        out_come[,23]<-as.numeric(out_come[,23])#Pneumonia 30-day Death Rate
        #out_come$Hospital.Name<-as.factor(out_come$Hospital.Name)
        kapa<-out_come$State[out_come$State %in% state]
        lk=length(kapa)
        if(lk==0) stop("invalid state") # ERROR for invalid state
        
        
        if(num=="best"){
                ch<-as.character(best(state,outcome))
                ch
        }
        
        else if(num=="worst"){
                source('~/InternetCourses/Computing_for_Data_Analysis/ProgAssignment2-data/worst.R', echo=TRUE)
                ch<-as.character(worst(state,outcome))
                ch
        }
        
        
        else if(is.numeric(num)==TRUE){
                if(outcome=="heart attack"){
                        out_come2<-subset(out_come,out_come$State==state)
                        out_come2$Hospital.Name<-as.factor(out_come2$Hospital.Name)
                        sortout<-out_come2[order(out_come2[,11],out_come2[,2]),]
                        ch<-as.character(sortout$Hospital.Name[num]) 
                        ch
                }                      
                
                else if(outcome=="heart failure"){
                        out_come2<-subset(out_come,out_come$State==state)
                        out_come2$Hospital.Name<-as.factor(out_come2$Hospital.Name)
                        sortout<-out_come2[order(out_come2[,17],out_come2[,2]),]
                        ch<-as.character(sortout$Hospital.Name[num]) 
                        ch
                }
                
                else if(outcome=="pneumonia"){
                        out_come2<-subset(out_come,out_come$State==state)
                        out_come2$Hospital.Name<-as.factor(out_come2$Hospital.Name)
                        sortout<-out_come2[order(out_come2[,23],out_come2[,2]),]
                        ch<-as.character(sortout$Hospital.Name[num]) 
                        ch
                }
                
                else{stop("invalid outcome")}
                
        }
        
        
        
        
        
        
        
        
}