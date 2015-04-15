#init
download.file("http://www.dit.ie/media/computing/spam.csv",destfile="/spam/spam.csv",method="curl")

?download.file
# Set Options
library(tm)
options(header=F, stringsAsFactors=F, fileEncoding="latin1")

# Set parameters
pathname<-"./textMining"

# read the data
#data<-read.csv("spam.csv")
corpus<-VCorpus(VectorSource(twitter))


## clean up
cleanset<-tm_map(corpus,removeWords,stopwords("english"))
cleanset<-tm_map(corpus,stripWhitespace)


# Build the term document matrix
dtm<-DocumentTermMatrix(cleanset)

#TF-IDF
dtm_tfxdf<-weightTfIdf(dtm)


# CLustering
m<- as.matrix(dtm_tfxdf)
rownames(m)<-1:nrow(m)
norm.eucl<-function(m){
    m/apply(m,1,function(x) sum(x^2)^.5)
}
m.norm<-norm.eucl(m)
results<-kmeans(m.norm,12,30)

cluster<-1:12
for(i in clusters){
    cat("Cluster",i,":",finFreqTerns(dtm_tfxdf[results¼cluster=i],2),\n\n)
}
