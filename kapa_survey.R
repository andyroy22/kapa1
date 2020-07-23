#
# KAPA Survey - Response encoding
# June-July 2020
#
library(data.table)
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
# topics <- c("LDMTLD","often","when")
# topics <- c("LDMTLD",topics)
# topics <- c("PCREFp","PCVERBp","SMINTEp","DRGERUND","DRINF","PCTEMPp")
# topics <- c("because","when","often")
# topics <- c("when","often","WRDADJ")
# topics <- c("when","often","LDMTLD","CNCAll") # ***
topics <- c("when","often","because")

# init kapa
kapa <- alldf[lev==1,-"text"]

# kapa <- kapa[coh,,on="id"] # join coh-metrix
kapa <- kapa[hand,,on="id"]

# PCA
pca <- prcomp(kapa[,..topics], scale=T)
xy <- data.frame(x = pca$x[,1], y = pca$x[,2])
# kapa <- cbind(kapa,xy)

fit <- plot_pca_clusters(xy,4,"Kapa",labs=kapa$id,factors=topics)
# kapa$group <- fit$cluster
table(fit$cluster) # how many in each cluster

# favorites <- which(kapa$id %in% c(21,25,29,30))
# cn <- c("id","group",topics)
# kapa[favorites,..cn]

# hand coded
# favorites <- which(kapa$code == 1)
# points(xy[favorites,],col="cyan",pch=1,cex=2,lwd=2)
# 
# favorites <- which(kapa$code == 2)
# points(xy[favorites,],col="green",pch=1,cex=2,lwd=2)
# 
# favorites <- which(kapa$code > 2)
# points(xy[favorites,],col="red",pch=1,cex=2,lwd=2)


# Cross Tabulations

cross_tab <- function(v1,v2) {
   nmx <- deparse(substitute(v1))
   nmy <- deparse(substitute(v2))
   nmx <- strsplit(nmx,"\\$")[[1]][2]
   nmy <- strsplit(nmy,"\\$")[[1]][2]
   title <- paste0(nmx,"  :  ",nmy)
   
   nmx <- strsplit(nmx,">")[[1]][1]
   nmy <- strsplit(nmy,">")[[1]][1]
   
   par(mar=c(3,3,4,1),cex.sub=0.9)
   
   if (class(v2)[1] !="logical") {
      med  <- summary(v1)[3]
      med2 <- summary(v2)[3]

      tb <- table(as.factor(v1>med), as.factor(v2>med2), dnn=c(nmx,nmy))
   } 
   else {
      tb <- table(as.factor(v1), as.factor(v2), dnn=c(nmx,nmy))
   }

   pt <- prop.table(tb, margin=1)
   likely <- round(100*(pt[2,2]/pt[1,2]-1))
   
   plot(tb,cex.axis=0.7,main="",col=c("lightgray","darkgray"),
        sub=paste0("more likely: ",likely,"%"))
   # col=topo.colors(5)[2:5],
   title(main=title,cex.main=0.9)

   return(tb)
}

cross_tab(kapa$when>1.5,kapa$inconsistent>0)
cross_tab(kapa$often>1,kapa$inconsistent>0)
cross_tab(kapa$because>0,kapa$inconsistent>0)

#kapa$inconsistent
kapa$inconsistent <- cut(kapa$inconsistent,
         breaks=c(0,1,2,5),
         labels=c("zero","one","two+"),
         ordered_result = T, include.lowest = T, right = F)

table_ <- table(kapa$when,kapa$inconsistent)
table_
chisq.test(table_)
kapa[,list(ave.when=mean(.SD$when)),inconsistent]

# cross_tab(kapa$when,x)

library(jmv)

# descriptives(kapa, vars = vars(when, often, because, inconsistent), freq = TRUE)

# kapa$when_gt_1 <- (kapa$when > 1)

# descriptives(formula = inconsistent ~ when_gt_1, kapa, 
#        median=T, min=F, max=F, n=T, freq = T)

# contTables(kapa,inconsistent,when_gt_1)

# kapa$when_gt_1 <- (kapa$when > 1)

# contTables(kapa,when_gt_1,inconsistent,pcCol = T)
