
# inti

libs<-c("tm", "plyr", "class")
lapply(libs, require,character.only=T)

# set options
options(stringsAsfactors=F)

# Set parameters

candidates<-c("bush", "weiss")
pathname<-"./textMining"
# Clean text

cleanCorpus<-function(corpus1){
    corpus.tm <- tm_map(corpus1, removePunctuation)
    corpus.tm <- tm_map(corpus1, stripWhitespace)
    corpus.tm <- tm_map(corpus1, tolower)
    corpus.tm <- tm_map(corpus1, removeWords,stopwords("english"))
    return(corpus.tm)
}

s.dir
# build tmd
generateTDM<-function(cand,path){
    s.dir<-sprintf("%s/%s", path,cand) # concatenate paht with directory name
    s.cor<-Corpus(DirSource(directory=s.dir,encoding="UTF-8"))
    s.cor.clean<-cleanCorpus(s.cor)
    s.tdm<-TermDocumentMatrix(s.cor.clean)
    
    s.tdm<- removeSparseTerms(s.tdm,0.7)
    result<-list(name=cand,tdm=s.tdm)
}


tdm<-lapply(candidates,generateTDM,path=pathname)


# clean up

bindCandidateTOTDM<-function(tdm){
    s.mat<-t(data.matrix(tdm[["tdm"]]))
    s.df<-as.data.frame(s.mat,stringsAsfactors=F)
    #atthis stage we get a df where ech speech is in a row and
    # each  term is in a column and 
    
    # put the name of the cadidate who set the speech at the last column
     s.df<-cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
    colnames(s.df)[ncol(s.df)]<-"targetcandidate"
    return(s.df)
}

candTDM<-lapply(tdm,bindCandidateTOTDM)
class(candTDM)

#  stack
 tdm.stack<-do.call(rbind.fill,candTDM)
 tdm.stack[is.na(tdm.stack)]<-0
 # at this point every row represents a speech
 head(tdm.stack,2)
# hold-ou
 train.idx<-sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)*0.6))
 test.idx<-(1:nrow(tdm.stack))[-train.idx]
        


# model -KNN
tdm.cand <-tdm.stack[,"targetcandidate"]
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in%  "targetcandidate"] 


knn.prediction<-knn(tdm.stack.nl[train.idx,], tdm.stack.nl[test.idx,],tdm.cand[train.idx])

#accuracy

conf.mat<- table("predictions"= knn.prediction,Actual=tdm.cand[test.idx])
conf.mat
 
(accuracy<-sum(diag(conf.mat))/length(test.idx)*100)
