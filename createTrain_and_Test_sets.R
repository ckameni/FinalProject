
path<-"/home/nkra/Capstone_Ds/final/en_US"
files<-dir(path)#[1]
name<-gsub("en_US.|.txt","",files)#[1]

mysample1<-function(path,file,name){
    #data<-readLines(sprintf("%s/%s.%s",files,names,"txt"))
    data<-readLines(sprintf("%s/%s",path,file))
    
    #select a random 5% of the lines
    long<-as.integer(length(data)*0.05)
    set.seed(123)
    #dat<- list(NULL)
    for(i in 1:12){
        inTrain <-sample(1:length(data),long, replace=F)
        dat <- data[inTrain]
        con<-sprintf("%s/%s/%s%s.%s", "/home/nkra/Capstone_Ds/Sample",
                     name,name,i,"txt")
        writeLines(dat, con)
        
        data<-data[-inTrain]
    }
    con<-sprintf("%s/%s%s.%s", "/home/nkra/Capstone_Ds/Sample/test",
                      name,"Test","txt")
    writeLines(data, con)

}

for(j in 1:3) mysample1(path,files[j],name[j])



