complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        obs<-matrix(1:length(id),length(id),2)
        labels<-c("id","nobs")
        colnames(obs)<-labels
        
        readmyFile <- function(id, directory) {
                
                
                id<-as.numeric(id)
                idsfx<-paste(directory,"/",sprintf("%03.0f",id),".csv",sep="")
                data<- read.csv(idsfx,header=TRUE)
                data
        }
        
        for(i in 1:length(id)) {
                obs[[i,2]]=NROW(na.omit(readmyFile(id[[i]],directory)))
                obs[[i,1]]=id[[i]]
        }
        
        
        obs<-as.data.frame(obs)
        obs
        
}