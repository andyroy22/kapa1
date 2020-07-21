#
# KAPA Survey - Response encoding
# June 2020
#
# source("meaningcloud.R")
library(data.table)
library(ggplot2)

source("topicfinder.R")

fn  <- "kapa survey responses 2020-06-29.csv"
dat <- fread(fn)
readTopics("kapa phrases.csv")

## Hand Coded classification
hand <- fread("kapa survey - hand coded.csv")
hand <- hand[,.(id,code)]
names(hand)[2]<-"inconsistent"


# save rows into alldf (global)
save_rows <- function(id,tg,df,txt1="") {
   # save rows to global alldf
   temp <- cbind(data.frame(
                  id=rep(id,nrow(df)),
                  tag=rep(tg,nrow(df)),
                  text=txt1,stringsAsFactors = F),
               df)
   if (is.null(alldf)) 
      alldf <<- as.data.table(temp)
   else 
      alldf <<- rbind(alldf,temp)

   # message("save_rows id: ",id," alldf:",nrow(alldf))
}

## Topic Analysis by counting incidence of key phrases
alldf <- NULL
kapa <- NULL
for (i in 1:nrow(dat)) {

   # split each response (positive/negative) into sentences
   txt1 <- gsub("\xcd","'",paste(dat[i,.(positive,negative)],collapse=" "))
   txt1 <- trimws(unlist(strsplit(txt1,"\\.")))

   # identify presence of kapa phrases
   flags <- flagTopics2(txt1)
   message("id: ",dat$id[i]," txt1: ",length(txt1)," flags: ",nrow(flags))

   # flags.sum <- round(colSums(flags)/nrow(flags),2) # normalize by sentence count
   flags.sum <- colSums(flags)
   lev <- c(rep(0,nrow(flags)),1)
   flags <- rbind(flags,flags.sum)
   flags <- cbind(flags,data.frame(lev=lev))

   save_rows(dat$id[i],nrow(flags),flags,c(txt1,""))
}
# fix column name
names(alldf)[which(names(alldf)=="wh3n")] <- "when"


## PCA and clusters / classification
kapa_colors <- rainbow(12,alpha=0.5)[2:10]
ramp<-colorRamp(c("lightgray","white"))
kapa_colors<-rgb(ramp(seq(0,1,length=6)),max=255)

plot_pca_clusters <- function(xy,nc=4,ti="pca",labs=NULL,factors="") {
   # xy: x,y coord
   # nc: # of clusters
   # ti: title text
   
   subtitle <- paste(factors,collapse=" ")
   col1 <- kapa_colors[1:nc]
   plot(xy,col="blue",type="n",pch=19,cex=.5,
        main=ti,xlab=NA,ylab=NA,cex.sub=0.8,sub=subtitle)
   fit <- kmeans(xy,nc)
   points(xy,col=col1[fit$cluster],pch=19,cex=1.9)
   if (!is.null(labs)) text(xy,labels = labs,cex=0.6)
   points(fit$centers,col="red",pch=3,cex=1.2)
   legend("topleft", legend=1:nc, bty="n", lty=1, lwd=2, col=col1, cex=0.4)
   
   return(fit)
}

## Analysis
# PCA
topics <- c("LDMTLD","often","when")
topics <- c("LDMTLD",topics)
topics <- c("PCREFp","PCVERBp","SMINTEp","DRGERUND","DRINF","PCTEMPp")
topics <- c("because","when","often")
topics <- c("when","often","WRDADJ")
topics <- c("when","often","LDMTLD","CNCAll") # ***
topics <- c("when","often","because")

# init kapa
kapa <- alldf[lev==1,-"text"]

# kapa <- kapa[coh,,on="id"] # join coh-metrix
kapa <- kapa[hand,,on="id"]

# PCA
pca <- prcomp(kapa[,..topics], scale=T)
xy <- data.frame(x = pca$x[,1], y = pca$x[,2])
kapa <- cbind(kapa,xy)

fit <- plot_pca_clusters(xy,4,"Kapa",labs=kapa$id,factors=topics)
kapa$group <- fit$cluster
table(fit$cluster) # how many in each cluster

# favorites <- which(kapa$id %in% c(21,25,29,30))
# cn <- c("id","group",topics)
# kapa[favorites,..cn]

# hand coded
favorites <- which(kapa$code == 1)
points(xy[favorites,],col="cyan",pch=1,cex=2,lwd=2)

favorites <- which(kapa$code == 2)
points(xy[favorites,],col="green",pch=1,cex=2,lwd=2)

favorites <- which(kapa$code > 2)
points(xy[favorites,],col="red",pch=1,cex=2,lwd=2)


# Cross Tabulations

cross_tab <- function(v1,v2) {

   nmx <- deparse(substitute(v1))
   nmx <- strsplit(nmx,"\\$")[[1]][2]; nmx <- gsub("[^A-z]","",nmx)
   nmy <- deparse(substitute(v2))
   nmy <- strsplit(nmy,"\\$")[[1]][2]; nmy <- gsub("[^A-z]","",nmy)
   par(mar=c(6,2,1,1))
   
   if (class(v2)!="logical") {
      med <- summary(v1)[3]
      brks <- summary(v2)[c(1,2,4,5,6)]
      if (length(unique(brks))<5) brks[1]<-brks[1]-0.01
      
      tb <- table(as.factor(v1>med),
                  cut(v2,breaks=brks,labels=c("min","low","med","high"),
                      include.lowest=T,ordered_result = T),
                  dnn=c(nmx,nmy))
      xtrue  <- sum(tb[2,3:4]); xfalse <- sum(tb[1,3:4])
      likely <- round(100*(xtrue-xfalse)/xfalse,1)
   } 
   else {
      tb <- table(as.factor(v1), as.factor(v2), dnn=c(nmx,nmy))
      pt <- prop.table(tb, margin=1)
      likely <- round(100*(pt[2,2]/pt[1,2]-1),1)
   }

   plot(tb,col=topo.colors(5)[2:5],main=paste0(nmx," => ",nmy),
        sub=paste0("\nmore likely: ",likely,"%"),cex.axis=0.6)
   return(tb)
}

cross_tab(kapa$when>1.5,kapa$inconsistent>0)
cross_tab(kapa$often>1,kapa$inconsistent>0)
cross_tab(kapa$because,kapa$inconsistent)


### EXTRA / LEFTOVERS

p1 <- xy[c(4,15),] # two points the form a line
m1 <- (p1$y[2]-p1$y[1])/(p1$x[2]-p1$x[1])
abline(a=0,b=m1) # parallel line

line_side <- function(xy,p1) {
   # xy: point to be tested
   # p1: two points forming line
   # output: 1 or 2
   # formula: d=(x−x1)(y2−y1)−(y−y1)(x2−x1)

   d <- (xy$x-p1$x[1])*(p1$y[2]-p1$y[1]) - (xy$y-p1$y[1])*(p1$x[2]-p1$x[1])
   if (d<0) return(1)
   return(2)
}

x1 <- xy[which(kapa$id==11),]
line_side(x1,p1)


# look at only longer responses
kapa <- kapa[tag>10,]
# >> run the analysis <<
# >> choose group with most favorites <<

kapa <- kapa[group==3,]
dat.show <- dat[kapa,,on="id"]
write.csv(dat.show,"kapa survey subset-15JUL.csv",row.names = F)

# cnk <- c("tag","myself","when","often","worse","group","because","id")

# barplot(flags.sum,main = paste("Respondent",dat$id[i],collapse = " "))

# flags <- flagTopics()
# 
# flags <- as.data.table(flags)
# flags.sum <- flags[,lapply(.SD,sum)]
# flags.sum

# create an example to work with
fsum <- flags.sum[rep(1,10),]
fsum <- cbind(id=1:10,fsum)
fsum$myself<-round(runif(10,1,8))
fsum$time<-round(runif(10,1,8))
fsum$freq<-round(runif(10,1,8))
fsum$extreme<-round(runif(10,1,8))

# convert from wide to long
fsum.long <- melt(fsum, id.vars = c("id"))
fsum.long$id<-factor(fsum.long$id)
# dcast(fsum.long,id~variable+value)

ggdat <- fsum.long[id %in% 1:5]
ggplot(ggdat,aes(x=variable,y=value,group=id)) + 
   geom_line(aes(color=id)) + geom_point() + labs(x=NULL,y=NULL)

ggplot(ggdat,aes(x=variable,y=value,group=id)) + 
   geom_line(aes(color=id)) + geom_point() + labs(x=NULL,y=NULL) + facet_grid(rows = ggdat$id)

ggplot(ggdat,aes(x=variable,y=value,fill=id)) + 
   geom_bar(stat="identity") + facet_grid(rows = ggdat$id) +
   labs(x=NULL,y=NULL) + theme(legend.position = "none")


####
# https://grammar.yourdictionary.com/style-and-usage/words-that-describe-personality-traits.html
# https://grammar.yourdictionary.com/parts-of-speech/adjectives/personal-adjective.html
# https://learnersdictionary.com/3000-words/topic/personality-types


### TEXT MINING

library(tm)
library(tidytext)

alltext <- data.table(id=integer(0),
                      text=character(0))
for (i in 1:nrow(dat)) {
   
   # split each response (positive/negative) into sentences
   txt1 <- gsub("\xcd","'",paste(dat[i,.(positive,negative)],collapse=" "))
   txt1 <- trimws(unlist(strsplit(txt1,"\\.")))

   alltext <- rbind(alltext,
                    data.table(
                     id=dat$id[i],
                     text=txt1
                     ))
}
alltext <- alltext[text!="",]


# Prepare Term-Document Matrix
corpus <- Corpus(VectorSource(as.vector(alltext$text)))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus  # check corpus

# td.mat <- as.matrix(TermDocumentMatrix(corpus))

txt <- NULL
for (i in 1:length(corpus)) {
   txt[i] <- corpus[[i]]$content
}
rm(corpus)

textdf <- data.frame(text=txt,stringsAsFactors=F)
rm(txt)

tokens1 <- unnest_tokens(textdf, word, text)

ngrams <- data.table(ngram=integer(0),
                     phrase=character(0),
                     count=integer(0))

for (i in 3:5) {
   ng <- unnest_tokens(tokens1, phrase, word, token="ngrams", n=i) ;ng<-as.data.table(ng)
   ng <- ng[,.(count=.N),phrase][order(-count),]
   ngrams <- rbind(ngrams,
                     data.table(ngram=i,
                        phrase=ng$phrase,
                        count=ng$count))
}
rm(ng)

phrases_iam <- ngrams[grep("^i am",phrase),phrase]
phrases_iam <- gsub("i am ","",phrases_iam)

grep("hard",phrases_iam,value=T)

#  [1] "i am hardworking"             "i am hard"                    "i am a hard"                 
#  [4] "i am hard working"            "i am very hardworking"        "i am hardworking i"          
#  [7] "i am hardworking person"      "i am very hard"               "i am a hard worker"          
# [10] "i am a hard and"              "i am a very hardworking"      "i am caring its hard"        
# [13] "i am hard working i"          "i am very hardworking for"    "i am hardworking i am"       
# [16] "i am hardworking person also" "i am very hard working"      

# https://ipip.ori.org/newIndexofScaleLabels.htm


doc <- tags$html(
   tags$head(
      tags$title('My first page')
   ),
   tags$body(
      h1('My first heading'),
      p('My first paragraph, with some ',
        span('highlighted',style='background-color:yellow;'),
        ' text.'),
      div(id='myDiv', class='simpleDiv',
          'Here is a div with some attributes.')
   )
)
cat(as.character(doc))

tempDir <- tempfile()
dir.create(tempDir)

htmlFile <- file.path(tempDir, "test.html")
writeLines(as.character(doc), htmlFile)
rstudioapi::viewer(htmlFile)

##
## Coh-Metrix data import
##
coh <- fread("cohmetrix-POSNEGcombined.csv")
coh.desc <- as.character(coh[1,])
names(coh.desc) <- names(coh)
write.csv(coh.desc,"cohmetrix-fields.csv")

coh <- coh[-1,] # eliminate descriptions
names(coh)[1] <- "id" # rename "Numbers" to "id"
write.csv(coh,"coh-metrix kapa.csv",row.names = F)
coh <- fread("coh-metrix kapa.csv") # reading back in changes chr columns to numeric

cat(grep("cnc",names(coh.desc),ignore=T,value=T))

# lexical diversity, any connective measures (start with CNC), and 
# referential cohesion (PCREFp)
# 
# also the following categories might be useful:
# 
# verb cohesion (PCVERBp)- which might also be expected logically
# Intentional verb incidence (SMINTEp)
# Gerund density (DRGERUND)
# Infinitive density (DRINF)
# 
# MAYBE temporality (PCTEMPp)... to pick up some of the "hedging

## from DAN
# -- qualities that contradict (hard working / lazy being the most common)
# -- qualifier statements – or one could simply say “contextualizing statements,” 
# i.e., some people write about other people, places, events, challenges 
# when asked to describe themselves, 
# whereas others describe isolated in-the-head attributes
# -- connector statements:  “x makes me y” (e.g., my depression makes me shy); 
# “X causes my y”. 
# 
# Running beyond our data per se, some people seem automatically to draw upon a kind of story-schematic sense of person attributes, whereas others self-describe in a set of seemingly independent attribute terms.

### AWS Comprehend

library(aws.comprehend)
getwd()
setwd("/Users/anindaroy/Google Drive/RCode")
x<-read.csv("aws_credentials.csv",stringsAsFactors = F)
Sys.setenv(AWS_ACCESS_KEY_ID=x$Access.key.ID)
Sys.setenv(AWS_SECRET_ACCESS_KEY=x$Secret.access.key)

aws1 <- detect_entities(text=alldf$text[1],region='us-east-2')
aws1 <- detect_phrases(text=txt,region='us-east-2')
aws2 <- detect_syntax(txt)
aws2 <- as.data.table(aws2)
aws2$Text<-tolower(aws2$Text)

aws2[PartOfSpeech.Tag=="ADP",]
aws2[Text=="when",]
names(aws2)
names(aws2)[5]<-"pos"
w1 <- which(aws2$pos=="CONJ")
sapply(w1, function(x) paste(aws2[(x-2):(x+2),Text],collapse = " ") )

aws2[pos=="PUNCT",.N]
aws2<-aws2[pos!="PUNCT",]
aws2.pairs <- paste(c("",aws2$Text),c(aws2$Text,""))

grep("i am",aws2.pairs)
sapply(grep("i am",aws2.pairs), function(x) paste(aws2[(x-1):(x+4),Text],collapse = " ") )
# [1] "i am a hard and diligent"        "i am honest and will tell"      
# [3] "i am a good listener and"        "i am strong and have proven"    
# [5] "i am independent but am willing" "i am sociable and enjoy talking"
