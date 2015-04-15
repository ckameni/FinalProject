library(tm) # framework for text mining
library(SnowballC) # provides wordStem() for stemming
library(Rgraphviz) # provides term correlation plots
library(tidyr) # assists in cleaning & preparing data
library(dplyr) # assists in data manipulation, transformation, & summarization
library(RWeka) # tokenzation

########dfs <- data.frame(text=unlist(sapply(blogs,'[',"content")),stringsAsFactors=F)


#path<-"/home/nkra/Capstone_Ds/final/en_US"

path<-"/home/nkra/Capstone_Ds/Sample"
files<-dir(path)#[1]
name<-gsub("en_US.|.txt","",files)#[1]


##=================================================================================
# this is the function that makes Corpus
myCorpus<-function(data){
    corp<- Corpus(VectorSource(data), readerControl = list(language="en_US"))
    corp<-Corpus(VectorSource(data))
    delete <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    #corp<- tm_map(corp, removeWords, stopwords("english"))
    corp <- tm_map(corp, delete, "[^[:alnum:]]")
    corp <- tm_map(corp, removeNumbers)
    corp <- tm_map(corp, removePunctuation)
    # transform words to lower case
    corp <- tm_map(corp, tolower)
    # remove white space
    corp <- tm_map(corp , stripWhitespace)
    
    # reformat documents to PlainTextDocument to allow for DocumentTermMatrix and TermDocumentMatrix formats
    corp <- tm_map(corp, PlainTextDocument)
    return(corp)
    
}


storeCorpus<-function(path,name){  
#tramsform each of the 12 .txt files in a corpus an store them as  .RData   
    for(i in 1:12){
        con<-sprintf("%s/%s/%s%s.%s", path,
                     name,name,i,"txt")
        assign("blog",readLines(con))
        blog<-myCorpus(blog) # create corpus
        save(blog,file=sprintf("%s%s%s.%s", name ,"Samp", i,"RData"))
        
    }
   
}



for(j in 1:3)storeCorpus(path,name[j])

                                                                                                                                                                                                                                                load("blogsSamp3.RData")
head(blog,2)
s#s<-blog
sa#sa<-blog

head(inspect(s),2)
summary(s)
s[[1]]
sa[[1]]
for(j in 1:3) mysample1(path,files[j],name[j])















getwd()
libs<-c("tm","RWeka","SnowballC","tidyr","dplyr")
lapply(libs,require,character.only=T)



save(blogs, file="blogs.RData")
save(news, file="news.RData")
save(twitter, file="twitter.RData")


path<-"/home/nkra/Capstone_Ds/Sample"
files<-dir(path)#[1]
name<-gsub("en_US.|.txt","",files)#[1]

createTdm<-function(name,path){
    for(i in 2:12){
        # read only the first file in the directory
        data<- read.csv(sprintf("%s/%s/%s%s.%s", path,name,name,i,"csv"))
        corpus<- Corpus(VectorSource(data), readerControl = list(language="en_US"))
        
        #corpus<-Corpus(DirSource(directory=s.dir,encoding="UTF-8"))
        corpus<-myCorpus(corpus)
        tdm<-TermDocumentMatrix(corpus)
        
        tdm<- removeSparseTerms(tdm,0.7)
        #result<-list(name=cand,tdm=tdm)
        matrix<-as.matrix(tdm)
        write.csv(matrix, sprintf("%s/%s%s%s.%s", path,name,"Tdm_",i,"csv"))
    }
}


createTdm(name[1],path)

inspect(tdm)
######tdm<-lapply(candidates,generateTDM,path=pathname)

data<- read.csv("/home/nkra/Capstone_Ds/Sample/tdm/blogsTdm_6.csv")

dat<-read.csv("/home/nkra/Capstone_Ds/Sample/blogs/blogs6.csv")
sa<-grep("[^[:alnum:]]",dat,value=T) 
length(sa)
class(sa)
dat<-Corpus(VectorSource(dat))
dat<-TermDocumentMatrix(data)    
dim(dat)

dfs <- data.frame(text=unlist(sapply(dat,'[',"content")),stringsAsFactors=F)

dim(dfs)
nrow(dat)
inspect(dat)
str(data)
head(data)
dat[grep("[^[:alnum:]]",dat),]

head(data)
concatenateTdm<-function(){
    createTdm<-function(name,path){
        for(i in 2:12){
            # read only the first file in the directory
            data<- read.csv(sprintf("%s/%s/%s%s.%s", path,name,name,i,"csv"))
            corpus<- Corpus(VectorSource(data), readerControl = list(language="en_US"))
            
            #corpus<-Corpus(DirSource(directory=s.dir,encoding="UTF-8"))
            corpus<-myCorpus(corpus)
            tdm<-TermDocumentMatrix(corpus)
            
            tdm<- removeSparseTerms(tdm,0.7)
            #result<-list(name=cand,tdm=tdm)
            matrix<-as.matrix(tdm)
            write.csv(matrix, sprintf("%s/%s%s%s.%s", path,name,"Tdm_",i,"csv"))
            
            #create 12 samples with each 5 percent of blog-data
            
            
            
            ####twitter <- twitter[rbinom(length(twitter)*.10, length(twitter), .5)]
            ####write.csv(twitter, file = "./Sample/twitter.sample.csv", row.names = FALSE, col.names = FALSE)
            
            # clean up global environment
            ####rm(blogs, news, twitter)
            
            # combine samples datasets into a Corpus
            #full <- Corpus(VectorSource(c(blogs,news,twitter)), readerControl = list(language="en_US"))
            
            ####full <- Corpus(DirSource("./Sample"), readerControl = list(language="en_US"))
            full <- Corpus(VectorSource(c(blogs,news,twitter)), readerControl = list(language="en_US"))
            rm(blogs, news, twitter)
            length(full)
            summary(full)
            meta(full[[3]])
            
            length(full[[1]]$content)
            length(full[[2]]$content)
            length(full[[3]]$content)
            
            library(tm)
            filesDir<-dir("/home/nkra/Capstone_Ds/Sample")
            data<-read.csv("/home/nkra/Capstone_Ds/Sample/blogs1.csv")
            inspect(cor)
            #Pre-processing the Corpus
            # create & apply function to remove special characters
            myCorpus<-function(data){
                corp<-Corpus(VectorSource(data))
                toSp <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
                corp<- tm_map(corp, removeWords, stopwords("english"))
                corp <- tm_map(corp, toSp, "[^[:alnum:]]")
                corp <- tm_map(corp, removeNumbers)
                corp <- tm_map(corp, removePunctuation)
                # transform words to lower case
                corp <- tm_map(corp, tolower)
                # remove white space
                corp <- tm_map(corp , stripWhitespace)
                
                # reformat documents to PlainTextDocument to allow for DocumentTermMatrix and TermDocumentMatrix formats
                corp <- tm_map(corp, PlainTextDocument)
                return(corp)
            }
            
            inspect(corp)
            # make the  corpus a df
            dfs <- data.frame(text=unlist(sapply(corp,'[',"content")),stringsAsFactors=F)
            #write the df as table in the computer
            write.table(dfs,file = "/home/nkra/Capstone_Ds/mycorpus.txt", row.names = FALSE, col.names = FALSE)
            # read from the 
            a<-read.table("/home/nkra/Capstone_Ds/mycorpus.txt", col.names = FALSE)
            c<-read.csv("/home/nkra/Capstone_Ds/Sample/blogs1.csv", col.names = FALSE)
            b<-read.csv("/home/nkra/Capstone_Ds/Sample/blogs2.csv", col.names = FALSE)
            
            head(b)
            head(c)
            # visualize the df
            str(a)
            summary(a)
            head(levels(a[,1]),20)
            head(levels(a[2,]),20)
            myCorpus(data)
            b<- Corpus(VectorSource(a))#, PlainTextDocument)
            inspect(b)
            inspect(corp)
            
            a[2,1]
            a[1,1]
            m<-matrix(corp)
            str(m)
            a[1,1]
            # remove numbers as they are likely not going to provide added benefit to my analysis
            
            
            # for my initial analysis I will remove punctuation
            
            )
# remove profanity words
profanity <- read.csv("~/Desktop/Personal/Education & Training/Coursera/Capstone/Capstone/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/en", header=FALSE, stringsAsFactors=FALSE)
profanity <- profanity$V1
docs <- tm_map(docs, removeWords, profanity)




# perform stemming to remove complexity introduced by multiple suffixes and get word radicals
docs_context <- tm_map(docs_context, stemDocument, language = "english")







Interesting Findings

The following are a few interesting items I discovered and will need to address in my final predictive model. Some of these items can be addressed in initial pre-processing, some may require coding that finds similar words as in the case of miss-spellings, and others may not be addressed adequately at all.

Several foreign words have been identified in the sample Blog data, and interestingly enough, they are mixed in with english words. This results in n-grams that contains phrases such as “πνευματικον which means”
The sample Twitter data contains the following “” 61 times
48 terms that are over 20 characters in length were used 2,147 times (1432 times in Twitter data: 67%, 454 times in Blog data: 21%, and 261 times in News data: 12%). These terms includes URLs (ie: wwwchrisandamandswonderyearswordpresscom), combined words that may represent hashtags (ie: funniestthingiheardtoday), and non-words (ie: plsplsplsplsplsplsplsplsplspls).
Miss-spelled and/or abbreviated words to include aaaand, yourre, yrs, txt, spf, etc.
It appears that line numbers may have been incorporated into some of the lines in the sample News data. Several words in the form of “\u0096” made it through my pre-processing. This resulted in a few instances of n-grams such as *“\u0097” before adversity“*.

Plans for Final Project

I am still developing my plans for my final project; however, I have started to scope out the basic process. The following is a very generalized overview of my planned approach.
Step 1: Develop a larger training set

I found that using the tm and RWeka libraries has its limitations. The processing time and memory requirements to analyze more than 10% of the three datasets in one corpora is unrealistic. As a result, I recently created a new process in developing a training set much larger than the 10% I analyzed above. First, I take 10% of the primary dataset as sample #1. Second, I another 10% sub sample of the remaining datasets. Third, I take another 10% sub sample of the remaining dataset and so forth for 10 sub samples. Each training set will be its own corpora in which I will clean and pre-process each one independently. The final training set will equate to approximately 65% of each of the Blog, News, and Twitter data set versus just 10% as analyzed above.
Step 2: Processing the multiple training sets

For each of the sub sample corporas, I will perform similar data processing as above in which I will remove punctuations, numbers, convert to lowercase, etc. One area to note is I have already resolved items 1, 2, and 3 from the Interesting Findings section above. In my pre-processing above I created a function to remove the “/”, “@”, and “\” characters; however, later I found that using “[^[:alnum:]]” in my docs <- tm_map(full, toSpace, "[^[:alnum:]]") function resolved these issues. Each corpora will be processed individually.

Next, I will mine each corpora to develop 2-, 3-, 4-, and 5-gram lists. I will then combine the multiple n-gram lists from each corpora together, which creates an n-gram list for my entire training set. As an example, when I produced the 3-gram list above, I identified a total of 237,394 3-grams for my 10% training sample. However, when I developed my 3-gram list for my 10 training sub samples and then combined them into one master list, I identified 2,026,622 unique 3-grams. This creates far more power in my algorithms predictive capabilities.
Step 3: Creating a prediction algorithm

In my shiny app I will have a text box where the user provides an input phrase. For instance, this phrase could be “Johns Hopkins team, thanks for the great” From the desired phrase I will take the final 2, 3, or 4 words (depending on length of user input) as my initial user phrase. In my example phrase (“Johns Hopkins team, thanks for the great”) the input is 7 words. Since my longest n-gram will be 5-grams I would take the last four words (“team, thanks for the great”). I will then process this text to remove any punctuation, turn to lowercase, etc. I will then search for this phrase in my 5-gram list and identify the 5-gram that has the highest probability (using the summation of log probabilities approach to minize underflow) with this phrase. Finally, I will select the fifth word in this 5-gram as the predicted word to display.
Step 4: Issues to overcome

So far I have performed most of steps 1-3 with success. I used this process to successfully answer the Quiz 2 questions; however, there are several issues that I need to learn about and address in my algorithm. These issues include:
    
    Misspellings: I need to identify a process that includes identifying similar words to the users input in case they misspelled a word. This will likely leverage the agrep() function.
Synonyms: I need to identify a process that will incorporate synonyms. If a user inputs a certain word that is not in my data set such as “focus”, I will need to be able to substitute a synonym such as “target, center, core, etc” that will also be searched for in my data set.
No Matches: If the user inputs a phrase that has no matches (even after I incorporate a process that uses synonyms and approximate matches with agrep()), I will need to have a sufficient process in place to deal with this situation. This may include creating interaction with the user in which I request another similiar phrase or, if the user input a long phrase I could take n-words from the beginning part of the phrase and perform a search to try identify relationships.

Conclusion

As an analyst that has traditionally focused on numerical analyses my entire career, text mining is a brand new analytic technique that I have been unfamiliar with. I have been spending a lot of time familiarizing myself with the basics, understanding the limitations of current text mining libraries, and learning to create new approaches without libraries to overcome some of these limitations. Although my work with this project may not be cutting edge, I am just greatful for being able to add this new skill set to my analytic capabilities.

















library(RWeka)

oneGram <- NGramTokenizer(fCorpus$terms, Weka_control(min = 1, max = 1));
head(oneGram)
twoGram <- NGramTokenizer(df, Weka_control(min = 2, max = 2))
threeGram <- NGramTokenizer(df, Weka_control(min = 3, max = 3))
fourGram <- NGramTokenizer(df, Weka_control(min = 4, max = 4))


#==============================================================
top1<-as.data.frame(table(oneGram))
top1final<-(head(top1[order(-top1$Freq),],5))
print(top1final)

one<- data.frame(table(oneGram))
one <- one_gram_dt[order(oneGram$Freq,decreasing = TRUE),]
head(one)