

##7
rand.play <- function(K){
teams <- sample.int(13, 2, replace = FALSE)
 i <- teams[1]
 j <- teams[2]
 Ri <- true[i]
 Rj <- true[j]
 pi <- 1/(1+exp(Rj-Ri))
 pj <- 1/(1+exp(Ri-Rj))
 iwins <- game(Ri,Rj)
if(iwins==0){
  Ri.new <- Ri-K*pi
  Rj.new <- Rj+K*(1-pj)
}
 if(iwins==1){
  Ri.new <- Ri + K(1-pi)
  Rj.new <- Rj - K*pj
}
  return(list(iwins=iwins,Ri.new=Ri.new,Rj.new=Rj.new,i=i,j=j))
}


##8

game.play <- function(K) {
 for (i in 1:10000) {
    a <- rand.play(K)
    ai <- a$i
    aj <- a$j
    #update ture
    true[ai] <- a$Ri.new
    true[aj] <- a$Rj.new
    #deposit the ratings over time
    ot.rate[ai,i] <- a$Ri.new
    ot.rate[aj,i] <- a$Rj.new
 }
return(ot.rate)
}
