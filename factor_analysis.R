#
# Correspondence Analysis - R Example using CA library
#
library(ca)
library(openxlsx)

# read in data (Excel)
dat <- read.xlsx("category_data.xlsx")
# alternate: csv
dat <- read.csv("category_data.csv", stringsAsFactors=F)

# convert values to ordered Factors
dat$degree <- factor(dat$degree, levels=c("D1","D2","D3"),ordered=T)
dat$biz    <- factor(dat$biz,    levels=c("N","Y"),ordered=T)
dat$change <- factor(dat$change, levels=c("N","Y"),ordered=T)
dat$public <- factor(dat$public, levels=c("N","Y"),ordered=T)
dat$wealth <- factor(dat$wealth, levels=c("L","M","H"),ordered=T)

# visualize data abstractly (first 10 rows)
par(mar=c(1,1,1,1),mfrow=c(10,1))
for(i in 1:10) 
   barplot(sapply(2:ncol(dat),function(x) which(levels(dat[i,x])==dat[i,x])),
           ylim=c(0,4),border=NA,col="blue")

# if wealth is numeric
# dat$wealth <- cut(dat$wealth, 
#                 breaks=c(0,50,150,1000),
#                 labels=c("low","med","high"),ordered_result = T)

# Plot Categorical Relationships
# examples of factor Y vs factor X as a barplot
plot(dat$public ~ dat$degree, col=rainbow(4))
plot(dat$public ~ dat$biz, col=rainbow(4))
plot(dat$wealth ~ dat$biz, col=rainbow(4))
plot(dat$wealth ~ dat$degree, col=rainbow(4), main="wealth~degree")

# create multiple correspondence analysis
# (leave out name of row)
mca  <- mjca(dat[,-1])
xy   <- data.frame(x = mca$rowcoord[,1], y = mca$rowcoord[,2])

# standard MCA plot
plot(mca)
fit<- plot_clusters(xy,nc=4,labs=dat$case) # ,mca=mca


# alternate way of distributing cases
# rowcoord columns 1 & 2 correspond to x & y coord of plot
plot(xy,xlab=NA,ylab=NA,pch=3,cex=0.5,col="yellow")
text(xy, labels=dat$case, 
     col=ifelse(dat$degree=="D3","purple","orange"))

# another example
plot(xy,xlab=NA,ylab=NA,pch=3,cex=0.5,col="yellow")
text(xy,cex=0.9,
     labels=paste(dat$case,dat$degree,sep=":\ndeg="), 
     col=ifelse(dat$wealth=="H","red","green"))

# third example
plot(mca$rowcoord[,1],mca$rowcoord[,2],xlab=NA,ylab=NA,pch=3,cex=0.5,col="yellow")
text(mca$rowcoord[,1],mca$rowcoord[,2],cex=0.8,
     labels=paste(dat$case,dat$wealth,sep=":\nw="), 
     col=ifelse(dat$wealth=="H","red","green"))


plot_clusters <- function(xy,nc=4,ti="Clusters",labs=NULL,mca=NULL) {
   # xy: x,y coord
   # nc: # of clusters
   # ti: title text
   # labs: point labels
   
   col1 <- rainbow(nc)
   xy.lim<-c(min(xy$x),max(xy$x),min(xy$y),max(xy$y))*1.2
   
   plot(xy,col="blue",type="n",main=ti,
        xlab=NA,ylab=NA,xlim=xy.lim[1:2],ylim=xy.lim[3:4])
   
   fit <- kmeans(xy,nc)
   points(xy,col=col1[fit$cluster],pch=19,cex=1.3)
   points(fit$centers,col="gray",pch=3,cex=1.2) # cross-hairs in center
   
   if (!is.null(labs)) text(xy,cex=0.9,pos=4,offset=0.5,labels=labs)
   if (!is.null(mca))  text(mca$colcoord[,1]*1.5,mca$colcoord[,2]*1.5,
                            labels=mca$levelnames,cex=0.7,col="red")
   
   legend("topleft", legend=1:nc, bty="n", lty=1, lwd=2, col=col1, cex=0.4)
   
   return(fit)
}
