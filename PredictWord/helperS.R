##
## helper1.R - algorithm - stupid backoff
##
library(quanteda)
library(dplyr)
library(data.table)

## process initial string - remove strange chars, tolower, remove punctuation
## if necessary remove bad words (such as swear words)
## without stop words
prep<-function(str,stop_words) {
  inStr <- iconv(str, "latin1", "ASCII", sub="");
  ## stop_words == TRUE
  str <- tokens(str, ngrams = 1) %>%
    tokens_tolower() %>%
    tokens(remove_punct=TRUE)
  
  if (!stop_words)
    str <- tokens_select(str, stopwords('en'), selection = 'remove')
  str
}

## backoff - get substring of n words from the end
# n -length of ngram; returns higher number of in dfm
strbo<-function(s,n) {
  
  len <- length(s) # len number of words
  res <- "^"
  if (n>1) {
    res <- s[len - n + 2]
    #print(sprintf("in strbo len: %d, s:%s res:%s",len, s,res))
    if (n > 2) { # if only 2 words - no paste
      for(i in (len - n + 3) : len)
        res <- paste(res,s[i],sep=" ")
    }
    res <- paste0("^",res)
  }
  #res <- paste0(res," *")
  #print(sprintf("in strbo res:%s",res))
  res
}

## try to select substring from dt 
## if substring exists us it 
select_from_dt<-function(sx,i) {
  #print(sprintf("select_from_dfm i:%d, dfms%d, sx:%s",i,i,sx))
  switch(as.character(i),
         "6" = dtx <- dt6[name %>% like(sx)],     
         "5" = dtx <- dt5[name %>% like(sx)],
         "4" = dtx <- dt4[name %>% like(sx)],
         "3" = dtx <- dt3[name %>% like(sx)],
         "2" = dtx <- dt2[name %>% like(sx)],
         "1" = dtx <- dt1[name %>% like(sx)],
         print(sprintf("i not found: %d %s",i,as.character(i)))
         
  )
}

## small helper functins
##
## small helpers
##
empty_reply<-function(stupid) {
  if (stupid)
    return(data.table(word=as.character(),prob=as.double()))
  return(data.table(word=as.character(),count=as.integer()))
}
dosort<-function(dtx,stupid){
  if (stupid)
    dtx <- dtx[order(-p)]
  else
    dtx <- dtx[order(-count)]
  dtx
}
getcols<-function(dtx1,stupid) {
  if (stupid) {
    dtx1 <- select(dtx1,-count)
    names(dtx1)<-c("word","prob")
  }
  else {
    dtx1 <- select(dtx1,-p)
    names(dtx1)<-c("word","count")
  }
  return(dtx1)
}


## 1. PREDICT WORD

stupid_predict_word<- function(str,stupid=TRUE) {
  if (nchar(str)==0) 
    return( empty_reply(stupid))
  savestr <- str
  for(i in seq(6,1,by=-1)) {
    str <- savestr
    str <- prep(str,TRUE)[[1]]
    if(length(str) <= (i-2))
      next
    sx <- strbo(str,i)
    if (i > 1) 
      sx <- paste0(sx," ")
    #print(sx)
    dtx <- select_from_dt(sx,i)
    nrow <-nrow(dtx)
    #print(sprintf("i:%d,nrow:%d sx:%s",i,nrow,sx))
    if (nrow==0)
      next
    dtx<-dosort(dtx,stupid)
    #print(dtx)
    nwords <- min(5,nrow)
    dtx <- dtx[1:nwords]
    dtx1 <- dtx
    dtx1$name <- substring(dtx$name,regexpr("\ [^\ ]*$", dtx$name)+1)
    #print(dtx1)
    return(getcols(dtx1,stupid))
  } 
  return( empty_reply(stupid))
}


## 3. STUPID PREDICT CHAR-BY-CHAR


stupid_char_by_char<-function(str,chars_str,stupid=TRUE) { ## bchars if this is first
  if (nchar(str)==0)
    return( empty_reply(stupid))
  if (nchar(chars_str)==0)
    return (stupid_predict_word(str,stupid))
  str1 <- sprintf("%s %s",str,chars_str) 
  savestr <- str1
  for(i in seq(6,1,by=-1)) {
    str <- savestr
    str <- prep(str,TRUE)[[1]]
    if(length(str) <= (i-2))
      next
    sx <- strbo(str,i+1)
    dtx <- select_from_dt(sx,i)
    nrow <-nrow(dtx)
    #print(sprintf("i:%d,nrow:%d sx:%s",i,nrow,sx))
    if (nrow==0)
      next
    #print(dtx)
    dtx<-dosort(dtx,stupid)
    #print(dtx)
    nwords <- min(5,nrow)
    dtx <- dtx[1:nwords]
    dtx1 <- dtx
    dtx1$name <- substring(dtx$name,regexpr("\ [^\ ]*$", dtx$name)+1)
    #print(dtx1)
    return(getcols(dtx1,stupid))
  }
  return( empty_reply(stupid))
}



## 2. FIND THE BEST HINT (QUIZ)

## try to find if words exists in dmf 
## if yes find the most probable
stupid_find_answer<-function(sx,answer,dtf,i,stupid=TRUE) {
  
  nrow <- nrow(dtf)
  #print(sprintf("1. st_find answer dtf nrow:%d",nrow))
  if (nrow == 0 )
    return( empty_reply(stupid))
  found<-FALSE
  for(j in 1:length(answer)) { ## scan all words in test and find one with top position
    sans <- sprintf("%s %s$",sx,answer[j])
    dtt <- dtf[name %>% like(sans)] 
    #print(sprintf("2. st_find answer dtt nrow:%d  answer[j]$:%s",nrow(dtt),sans))
    if (nrow(dtt) == 0)
      next
    if(!found)
      dtx <- dtt
    else
      dtx<- rbind(dtx,dtt[1,])
    
    found<-TRUE  
  }   
  if (found) 
    dtx<-dosort(dtx,stupid)
  #print(dtx)
  else 
    return( empty_reply(stupid))
  dtx
}

stupid_predict_quiz<-function(str,answer_str,stupid=TRUE) {
  if (nchar(str)==0 || nchar(answer_str) == 0)
    return( empty_reply(stupid))
  ans <- tokens(answer_str, ngrams = 1) %>%
    tokens_tolower() %>%
    tokens(remove_punct=TRUE)
  answer<- ans[[1]]
  #print(sprintf("phrase:%s",str))
  #print(sprintf("possible answers:%s",answer_str))
  savestr <- str
  # check aginst dfm with stop words present
  
  for(i in seq(6,1,by=-1)) {
    str <- savestr
    str <- prep(str,TRUE)[[1]] ## str array of words from string
    #print(sprintf("in predict quiz: savestr: %s,str:%s",savestr,str))
    if(length(str) <= (i-2))
      next
    sx <- strbo(str,i)
    #print(sx)
    dtf <- select_from_dt(sx,i)
    nrow <-nrow(dtf)
    #print(sprintf("i:%d,dtf nrow:%d sx:%s",i,nrow,sx))
    if (nrow==0)
      next
    dtx <- stupid_find_answer(sx,answer,dtf,i,stupid)
    #print(sprintf("i:%d,dtx nrow:%d sx:%s",i,nrow,sx))
    if(nrow(dtx)==0)
      next
    #print(sprintf("result found in dt%d nrows:%d, topprob:%6.5f top word:%s",
    #       i, nrow(dtx),dtx[1]$p,dtx[1]$name))
    dtx1 <- dtx
    dtx1$name <- substring(dtx$name,regexpr("\ [^\ ]*$", dtx$name)+1)
    return(getcols(dtx1,stupid))
    
  }
  return( empty_reply(stupid))
  
}
