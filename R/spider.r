
library(RCurl)  
library(XML)  
library(tm) 
library(wordcloud2)
library(SnowballC)

myurl <- "https://www.amazon.com/Sony-Cancelling-Bluetooth-Headphone-MDR1000X/product-reviews/B01KHZ4ZYY/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

myheader <- c("User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",  
           "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",  
           "Accept-Language"="en-us",  
           "Connection"="keep-alive",  
           "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")

my_comment <- c()
for ( pageNumber in 1:24) {
	spider_url <- paste0(myurl,pageNumber)
	print(spider_url)
	webpage <- getURL(url=spider_url,httpheader=myheader,.encoding='utf-8')  
	pagetree <- htmlParse(webpage,encoding='utf-8')  
	comment  <-  xpathSApply(pagetree,"//span[@class='a-size-base review-text']",xmlValue)
	my_comment <- c(my_comment,comment)
}

mydf<- data.frame(comment=my_comment)

# tm
x <- VCorpus(DataframeSource(mydf))
x <- tm_map(x, stripWhitespace)
x <- tm_map(x, stemDocument)
dtm <- DocumentTermMatrix(x,control = list(removeNumbers = TRUE,stemming = TRUE,stopwords = TRUE,removePunctuation = TRUE))
dtm <- removeSparseTerms(dtm, sparse=0.99)
inspect(dtm)
rlt <- as.matrix(dtm)


# plot
word_freq <- data.frame(word=colnames(rlt),value=colSums(rlt))
wordcloud2(word_freq, size = 1, shape='star',color = 'random-dark',  backgroundColor = "white") 



# Find Associations 
findAssocs(dtm,terms = 'price',corlimit = 0.2)




