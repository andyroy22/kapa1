#
# KAPA Survey - Response encoding
# July 23, 2020
#
library(data.table)

source("topicfinder.R")

fn  <- "data/kapa survey responses 2020-06-29.csv"
dat <- fread(fn)
topics <- readTopics("kapa phrases.csv")

## Hand Coded classification
hand <- fread("data/kapa survey - hand coded.csv")
hand <- hand[,.(id,code)]
names(hand)[2]<-"inconsistent"

# save rows into alldf (global) - helper function
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

   # flags.sum <- round(colSums(flags)/nrow(flags),2) # normalize by sentence count
   flags.sum <- colSums(flags)
   lev <- c(rep(0,nrow(flags)),1)
   flags <- rbind(flags,flags.sum)
   flags <- cbind(flags,data.frame(lev=lev))

   save_rows(dat$id[i],nrow(flags),flags,c(txt1,""))
}

kapa_colors <- rainbow(12,alpha=0.5)[2:10]
# ramp<-colorRamp(c("darkgray","white"))
# kapa_colors<-rgb(ramp(seq(0,1,length=6)),max=255)

# init kapa
w1 <- setdiff(names(alldf),c("text","lev")) # don't keep lev & text column
kapa <- alldf[lev==1,..w1]

# kapa <- kapa[coh,,on="id"] # join coh-metrix
kapa <- kapa[hand,,on="id"]

# re-join with original data file
kapa <- kapa[dat,,on="id"]

write.csv(kapa,"kapa survey word counts.csv",row.names = F)

## Cross Tabulations
cross_tab <- function(v1,v2) {
   nmx <- deparse(substitute(v1))
   nmy <- deparse(substitute(v2))
   nmx <- strsplit(nmx,"\\$")[[1]][2]
   nmy <- strsplit(nmy,"\\$")[[1]][2]
   title <- paste0(nmx,"  :  ",nmy)
   
   nmx <- strsplit(nmx,">")[[1]][1]
   nmy <- strsplit(nmy,">")[[1]][1]
   
   par(mar=c(3,3,4,1),cex.sub=0.9)
   
   if (class(v1)[1] !="logical") {
      med  <- summary(v1)[3]
      med2 <- summary(v2)[3]

      tb <- table(as.factor(v1>med), as.factor(v2>med2), dnn=c(nmx,nmy))
      pt <- prop.table(tb, margin=1)
      likely <- round(100*(pt[2,2]/pt[1,2]-1))
   } 
   else {
      tb <- table(as.factor(v1), as.factor(v2), dnn=c(nmx,nmy))
      pt <- prop.table(tb, margin=1)
      likely <- round(100*(((1-pt[2,1])/(1-pt[1,1]))-1))
   }

   col1=topo.colors(5)[2:5]
   
   plot(tb,cex.axis=0.7,main="",col=col1,
        sub=paste0("more likely: ",likely,"%"))
   
   title(main=title,cex.main=0.9)

   print(tb)
   print(chisq.test(tb))

}

kapa$inconsistent <- cut(kapa$inconsistent,
                         breaks=c(0,1,2,5),
                         labels=c("zero","one","two+"),
                         ordered_result = T, include.lowest = T, right = F)

cross_tab(kapa$when>1,kapa$inconsistent)
cross_tab(kapa$often>=2,kapa$inconsistent)
cross_tab(kapa$because>0,kapa$inconsistent)

