pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        if (!is.numeric(id)){ 
                message(" Warning: id is not numeric but convert it to one")
                }
        
        id<-as.numeric(id)
        
        idsfx<-paste(directory,"/",sprintf("%03.0f",id[1]),".csv",sep="")
        prdata<- read.csv(idsfx,header=TRUE)
        data<- prdata
        
        if(length(id)==1){
                data<-data
        } else {
                for (i in 2:length(id)){ 
                        idsfx<-paste(directory,"/",sprintf("%03.0f",id[i]),".csv",sep="")
                        prdata<- read.csv(idsfx,header=TRUE)
                        data<-rbind(data,prdata)
        }
        }
        
#  
#                 readmyFile <- function(id, directory) {
#         
#         
#                 id<-as.numeric(id)
#                 idsfx<-paste(directory,"/",sprintf("%03.0f",id),".csv",sep="")
#                 data<- read.csv(idsfx,header=TRUE)
#                 data
#         }
      m<- mean(eval(parse(text=paste("data$",pollutant,sep=""))),na.rm=T)
      m<-round(m,3)  
      m
      
        
}