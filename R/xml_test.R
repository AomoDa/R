







stateNames = read.table(url("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/stateNames.txt"), header = TRUE)
stateNames = stateNames[-c(2),] 
require(XML)




num_value <- function(x){ 
	require(stringr)
	if(!is.na(x)){
	a = xmlValue(x)
	rt =  as.numeric(str_replace(a,'[,]',''))
   } else{
   	rt = NA
   }
	return(rt)
}


dat =data.frame(stringsAsFactors=F)

for (i in (1:length(stateNames))) {
  eResult2012List = paste("http://www.stat.berkeley.edu/users/nolan/data/voteProject/countyVotes2012/", stateNames[i], ".xml", sep = "")
  # eResult2012tmp = try(xmlParse(eResult2012List))
  # eResult2012tmp = try(xmlRoot(eResult2012tmp))
  # eResult2012[[i]] = eResult2012tmp
 infDoc = xmlParse(eResult2012List)
 infRoot = xmlRoot(infDoc)
 county=xpathSApply(infRoot, "//table/tbody/tr/th[@class='results-county']/text()",xmlValue)

 # county = xpathSApply(eResult2012[[1]], "//table/tbody/tr/th[@class='results-county']",xmlValue)
 # county = substr(county, 1, nchar(county)-17)
 # m_state = xpathSApply(infRoot,'//table/tbody/tr[./th[@class="results-candidate"]/text()="M. Romney"]/th[@class="results-county"]/text()',xmlValue)
 # b_state = xpathSApply(infRoot,'//table/tbody/tr[./th[@class="results-candidate"]/text()="B. Obama (i)"]/th[@class="results-county"]/text()',xmlValue)
 RomneyVote=xpathSApply(infRoot,'//table/tbody/tr[./th[@class="results-candidate"]/text()="M. Romney"]/td[@class="results-popular"]',num_value)
 ObamaVote=xpathSApply(infRoot,'//table/tbody/tr[./th[@class="results-candidate"]/text() = "B. Obama (i)"]/td[@class="results-popular"]',num_value)
 mydat = data.frame(State=stateNames[i],  county=county,RomneyVote=RomneyVote, ObamaVote=ObamaVote,stringsAsFactors=F)
 dat = rbind(dat,mydat)
 print(paste(i,' / ', length(stateNames),' finshed',sep='')) # print i 

}


