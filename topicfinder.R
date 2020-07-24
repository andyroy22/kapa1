#
# Topic Finder - revised
#

# read topic phrases file & store in global: search.topics
readTopics <- function(csv_file=NA) {
   if (!exists("search.topics")) {
      search.topics <<- list()
      default_topics <<- 
         "topic,w1,w2,w3,w4,w5,w6,w7,w8
      parts,batter,spark,valve,belt,wiper,,,
      inventory,carry,do you sell,do they,,,,,
      price,how much,expens,too high,cuanto,price,cost,bucks,	
      service,customer,service,rude,professional,helpful,above and,friendly,
      carmakes,Nissan,Honda,Toyota,Ford,,,,"
   }
   if (is.na(csv_file)) {
      search.topics$topics <<- 
         read.table(text=default_topics,sep=",",header=T,stringsAsFactors=F)
      search.topics$topics[search.topics$topics==""] <<- NA
   }
   else {
      search.topics$topics <<- read.csv(csv_file,stringsAsFactors=F)
      search.topics$topics[search.topics$topics==""] <<- NA
      search.topics$topics <<- search.topics$topics[!is.na(search.topics$topics$topic),]
   }
   
   search.topics$maxwords  <<- length(search.topics$topics[1,])
   search.topics$count     <<- nrow(search.topics$topics)

   return(gsub("\\#","",search.topics$topics[,1]))
}

# input is a vector of character strings (sentences)
# output is a row of flags per string
# this flags the # of occurences (not just 1 for present, 0 for not present)
flagTopics2 <- function(text) {
   # input is a character vector
   text.count <- length(text)
   if (length(search.topics)==0) stop("missing topics")
   
   flags <- NULL
   for (j in 1:search.topics$count) {
      flagName <- gsub("\\#","",search.topics$topics[j,1])
      if (is.null(flags)) {
         flags <- data.frame(rep(0,text.count))
         colnames(flags) <- flagName
      } else {
         flags[,flagName] <- 0
      }
      
      for (i in 1:search.topics$maxwords) {
         term <- search.topics$topics[j,i]
         if (!is.na(term)) {
            matches <- gregexpr(term, text, ignore.case=T)
            for (z in 1:length(matches)) {
               match.count <- length(matches[[z]])
               if (matches[[z]][1] == -1) match.count <- 0
               flags[z,flagName] <- flags[z,flagName] + match.count
            }
            # message("term: ",term," count: ",match.count)
         }
      }
   }
   
   return(flags)
}

