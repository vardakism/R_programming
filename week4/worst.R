worst <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with highest 30-day death
        ## rate
        
        
        #Initialize the database
        out_come<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        hospital<-read.csv("hospital-data.csv", colClasses="character")
        out_come.hospital<-merge(out_come,hospital,by="Provider.Number")
        out_come[,11]<-as.numeric(out_come[,11])#Heart attack 30-day Death Rate
        out_come[,17]<-as.numeric(out_come[,17])#Heart failure 30-day Death Rate
        out_come[,23]<-as.numeric(out_come[,23])#Pneumonia 30-day Death Rate
        #out_come$Hospital.Name<-as.factor(out_come$Hospital.Name)
        kapa<-out_come$State[out_come$State %in% state]
        lk=length(kapa)
        if(lk==0) stop("invalid state")
        
        if(outcome=="heart attack"){ 
                out_come2<-subset(out_come,out_come$State==state)
                out_come2$Hospital.Name<-as.factor(out_come2$Hospital.Name)                 
                tapply(out_come2[,11],out_come2$Hospital.Name,max,na.rm=TRUE)         
                range(out_come2[,11],na.rm=TRUE)
                which.max(out_come2[,11])
                a<-out_come2$Hospital.Name[which.max(out_come2[,11])]
                ch<-as.character(a)
                ch
        }
        
        else if(outcome=="heart failure"){
                out_come2<-subset(out_come,out_come$State==state)
                out_come2$Hospital.Name<-as.factor(out_come2$Hospital.Name)                 
                tapply(out_come2[,17],out_come2$Hospital.Name,max,na.rm=TRUE)         
                range(out_come2[,17],na.rm=TRUE)
                which.max(out_come2[,17])
                a<-out_come2$Hospital.Name[which.max(out_come2[,17])]
                ch<-as.character(a)
                ch
        }
        
        else if(outcome=="pneumonia"){
                out_come2<-subset(out_come,out_come$State==state)
                out_come2$Hospital.Name<-as.factor(out_come2$Hospital.Name)                 
                tapply(out_come2[,23],out_come2$Hospital.Name,max,na.rm=TRUE)         
                range(out_come2[,23],na.rm=TRUE)
                which.max(out_come2[,23])
                a<-out_come2$Hospital.Name[which.max(out_come2[,23])]
                ch<-as.character(a)
                ch
        }
        
        else{stop("invalid outcome")}
        
}