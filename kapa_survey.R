#
# KAPA Survey - Response encoding
# June 2020
#
# source("meaningcloud.R")
library(data.table)
library(ggplot2)
library(cluster)

# text classification
obj <- mcTextClass(s)
obj$category_list$label

#############

simplify_text <- function(s) {
   
   # topic extraction
   obj <- mcTopics(s); last_obj <<- obj
   
   obj$concept_list$sementity$type
   message("entity list: ",length(obj$entity_list))
   
   subj <- sapply(obj$relation_list$subject$lemma_list,function(x) ifelse(is.null(x[1]),"(none)",x[1]))
   if (length(subj)==0) subj <- "(none)"
   verbs <- sapply(obj$relation_list$verb$lemma_list,function(x) ifelse(is.null(x[1]),"(none)",x[1]))
   if (length(verbs)==0) verbs <- "(none)"

   statements <- data.frame(subject=subj,verbs=verbs,stringsAsFactors = F)
   
   i_do  <- statements$verbs[statements$subject=="I" & statements$verbs!="be"]
   i_do  <- unique(i_do); if (length(i_do)==0) i_do <- "(none)"
   message("i_do: ",paste(i_do,collapse = '\n'))
   
   it_be <- statements$subject[statements$verbs=="be" & statements$subject!="I"]
   it_be <- unique(it_be); if (length(it_be)==0) it_be <- "(none)"
   message("it_be: ",paste(it_be, collapse = '\n'))

   complements <- NULL
   for (i in 1:length(obj$relation_list$complement_list)) {
      df <- as.data.frame(obj$relation_list$complement_list[[i]])
      if (is.null(complements)) complements <- df else complements <- rbind(complements,df)
   }
   
   attribs <- complements$form[complements$type=="isAttribute"]
   attribs <- unique(attribs); if (length(attribs)==0) attribs <- "(none)"
   message("attribs: ",paste(attribs,collapse = '\n'))

   # build table   
   df <- data.frame(type="do",text=i_do,stringsAsFactors = F)
   
   df <- rbind(df,
               data.frame(type="be",text=it_be,stringsAsFactors = F))
   
   df <- rbind(df,
               data.frame(type="attr",text=attribs,stringsAsFactors = F))

   message("simplify_text: ",nrow(df))
   return(df)
}

fn  <- "kapa survey responses 2020-06-29.csv"
dat <- fread(fn)

# save rows into alldf (global)
save_rows <- function(id,tg,df) {
   # save rows to global alldf
   temp <- cbind(data.frame(
                  id=rep(id,nrow(df)),
                  tag=rep(tg,nrow(df)),stringsAsFactors = F),
               df)
   if (is.null(alldf)) 
      alldf <<- as.data.table(temp)
   else 
      alldf <<- rbind(alldf,temp)

   # message("save_rows id: ",id," alldf:",nrow(alldf))
}

# main loop
alldf <- NULL
kapa <- NULL
for (i in 1:nrow(dat)) {

   # split each response (positive/negative) into sentences
   txt1 <- gsub("\xcd","'",paste(dat[i,.(positive,negative)],collapse=" "))
   txt1 <- trimws(unlist(strsplit(txt1,"\\.")))

   # identify presence of kapa phrases
   flags <- flagTopics2(txt1)
   message("id: ",dat$id[i]," txt1: ",length(txt1)," flags: ",nrow(flags))

   flags.sum <- round(colSums(flags)/nrow(flags),2)
   lev <- c(rep(0,nrow(flags)),1)
   flags <- rbind(flags,flags.sum)
   flags <- cbind(flags,data.frame(lev=lev))

   save_rows(dat$id[i],nrow(flags),flags)

}

topics <- search.topics$topics[1][,1]
cn <- c("id","tag",topics)
kapa <- alldf[lev==1,..cn]

pca <- prcomp(kapa[,..topics], scale=T)
xy <- data.frame(x = pca$x[,1], y = pca$x[,2])
kapa <- cbind(kapa,xy)

plot_pca_clusters <- function(xy,nc=4,ti="pca") {
   # xy: x,y coord
   # nc: # of clusters
   # ti: title text

   col1 <- rainbow(nc)

   plot(xy,col="blue",type="p",pch=19,cex=.5,main=ti,xlab=NA,ylab=NA)
   fit <- kmeans(xy,nc)
   for (i in 1:nc) {
      points(xy[which(fit$cluster==i),],col=col1[i],lwd=2)
   }
   points(fit$centers,col="red",pch=3,cex=1.2)
   legend("topleft", legend=1:nc, bty="n", lty=1, lwd=2, col=col1, cex=0.4)
   
   return(fit)
}

fit <- plot_pca_clusters(xy,6,"Kapa")
kapa$group <- fit$cluster
table(fit$cluster) # how many in each cluster

cnk <- c("tag","trait","cond","freq","most","group","id")
write.csv(cbind(dat,kapa[,..cnk]),"kapa survey grouped-new.csv",row.names = F)


dat$positive[kapa$group==1]

clusplot(xy,fit$cluster)


# barplot(flags.sum,main = paste("Respondent",dat$id[i],collapse = " "))

readTopics("kapa phrases.csv")

# flags <- flagTopics()
# 
# flags <- as.data.table(flags)
# flags.sum <- flags[,lapply(.SD,sum)]
# flags.sum

# create an example to work with
fsum <- flags.sum[rep(1,10),]
fsum <- cbind(id=1:10,fsum)
fsum$trait<-round(runif(10,1,8))
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

## OLD version
# for (i in 1:nrow(dat)) {
for (i in 15:15) {
   message("id: ",dat$id[i])
   
   txt1 <- trimws(unlist(strsplit(dat$positive[i],"\\.")))
   for (j in 1:length(txt1)) {
      df <- simplify_text(txt1[j])
      df <- df[df$text!="(none)",]
      if (nrow(df)>0) save_rows(dat$id[i],"pos",df)
   }
   
   df <- simplify_text(dat$negative[i])
   save_rows(dat$id[i],"pos",df)
   
   message("loop, alldf: ", nrow(alldf))
}

write.csv(alldf,"kapa1-3.csv",row.names = F)

####
# https://grammar.yourdictionary.com/style-and-usage/words-that-describe-personality-traits.html
# https://grammar.yourdictionary.com/parts-of-speech/adjectives/personal-adjective.html
# https://learnersdictionary.com/3000-words/topic/personality-types


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


