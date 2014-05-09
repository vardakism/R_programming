corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        readmyFile <- function(id, directory) {
                
                
                id<-as.numeric(id)
                idsfx<-paste(directory,"/",sprintf("%03.0f",id),".csv",sep="")
                data<- read.csv(idsfx,header=TRUE)
                data
        }
        
        idx<- complete(directory)
        if(threshold<max(idx$nobs)){
                
                id<-subset(idx,idx$nobs>threshold)
                obs<-(1:NROW(id))
                
                for(i in 1:NROW(id)) {
                        data=readmyFile(id[[i,1]],directory)
                        obs[[i]]=cor(data$sulfate,data$nitrate,use="complete.obs")
                        
                }
                
                #obs<-round(obs,5)
                obs
        }
        
        else{obs<-as.numeric(vector())
             obs
        }
       
}