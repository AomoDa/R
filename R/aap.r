

#two points euclidean distance
euc <- function(x1,x2,y1,y2){
	dis <- sqrt((x1-x2)^2+(y1-y2)^2)
	return(dis)
}

# global tour 
tour <- function(tour_coordinate){
	tour_coordinate <- as.data.frame(tour_coordinate)
    rn <- nrow(tour_coordinate)
    tour_coordinate$x2 <- c(tour_coordinate$x1[rn],tour_coordinate$x1[-rn])
    tour_coordinate$y2 <- c(tour_coordinate$y1[rn],tour_coordinate$y1[-rn])
    g_tour <- apply(tour_coordinate,MARGIN = 1,function(x)euc(x[1],x[3],x[2],x[4]))
    return(sum(g_tour))
}


#travel change
travel_change <- function(old_tour_coordinate){
	old_tour_coordinate <- as.data.frame(old_tour_coordinate)
	ind <- sample(x = nrow(old_tour_coordinate),size = 2,replace = F)
	a <- old_tour_coordinate[ind[1],]
	b <- old_tour_coordinate[ind[2],]
	old_tour_coordinate[ind[1],] <- b
	old_tour_coordinate[ind[2],] <- a
	new_tour_coordinate <- old_tour_coordinate
	return(old_tour_coordinate)
}


# Anneal Arithmetic  Probability

# init_T 初始温度
# rate 降温速率 [0-1]

aap <- function(delta,init_T=1e5,rate=0.9,times=1){
    T <- init_T*rate^times
	prob <- exp(-delta / T)
	reject <- 1 - prob
	aap <- sample(x=c(TRUE,FALSE),size = 1,replace = F,prob = c(prob,reject))
	return(aap)
}


# Traveling Salesman

tsp_tour <- function(city_coordinate,loop_times=2e4 ){
 city_coordinate <- as.data.frame(city_coordinate)
 init_dis <- tour(tour_coordinate=city_coordinate)
 #Initialization
 travel_coordinate <- city_coordinate
 old_dis <- init_dis
 reject_num <- 0 
 for (i in 1:loop_times) {
 new_travel <- travel_change(travel_coordinate)
 new_dis <- tour(new_travel)
 diff_dis <- new_dis - old_dis

 if(diff_dis <=0){
 	travel_coordinate <- new_travel
 	old_dis <- new_dis
 	reject_num <- 0 
 	#debug
 	#print(c(diff_dis,new_dis))	
 }else if(aap(delta=diff_dis,init_T=1e5,rate=0.99,times=i)){
 	travel_coordinate <- new_travel
 	old_dis <- new_dis
 	reject_num <- 0 
 	#debug
 	#print(c(diff_dis,new_dis))	
 	}else{
 	reject_num <- reject_num +1
 	#debug
 	#print(reject_num)
 	}
 }
 rt <- list(min_distance=new_dis,min_distance_order_coordinate=travel_coordinate$id)
 return(rt)
}



#Qinit_T=10000


sample_data <- data.frame(id=1:100,x1=as.vector(sapply(1:10,function(x)rep(x,10))) ,y1=1:10)
#solution
tsp_tour(sample_data)




#Q2
# x,y coordinates of 100 cities in [0,100]^2
X <- c(
  52.66, 73.66,  1.61, 77.43, 21.43,  6.56, 73.89, 10.93, 18.51, 46.27, 77.37, 36.66,  3.77, 46.85, 20.74, 13.45, 68.91, 82.00, 22.25, 33.95, 64.54, 10.25, 92.69,
  90.03, 80.38, 15.22, 55.17, 13.26, 78.31, 19.78, 26.19, 28.42, 65.11, 12.26, 39.28, 32.89, 89.84, 91.60, 57.80, 73.23, 90.49, 37.16, 27.86, 69.33, 20.84, 52.13,
  98.31, 34.37, 37.21, 76.44, 21.91, 39.38, 92.25, 73.46, 68.68, 94.26, 62.01,  5.81, 45.38, 88.66, 59.30, 78.44, 23.95, 79.90, 54.18, 53.31, 78.72, 57.96, 55.27,
  26.39, 87.18, 93.64, 49.71, 78.19, 65.28, 65.21, 90.16,  4.00, 27.00, 74.13, 48.02, 36.45, 97.46, 12.01, 22.48, 53.41, 15.35, 70.07, 73.73, 68.31, 79.39, 45.11,
  61.25, 15.13, 58.57, 95.27, 30.35, 73.41, 10.75, 49.38
  )
Y <- c(
 10.19, 20.85, 36.42, 52.25, 43.03, 49.96, 92.10, 13.05, 66.13, 46.12, 60.46, 66.46, 25.04, 93.12, 84.86, 21.73, 69.88, 68.92, 84.26, 76.77, 68.78, 17.24, 81.18,
 60.23, 67.46,  5.83, 86.22, 86.13, 88.53, 59.33, 39.16, 94.00, 71.81, 91.32, 92.97,  6.15, 68.23, 59.24, 62.12, 71.25, 57.92, 59.14, 84.49, 76.80, 59.22, 26.71,
  7.79, 47.96,  0.88, 64.86, 21.72, 40.48, 80.03, 29.75, 62.99, 56.54, 24.46, 25.54, 44.33, 58.43, 96.62, 73.42, 63.87, 16.95, 89.10, 63.72, 26.75, 47.48, 77.68,
  2.84, 96.70, 77.40, 77.52, 83.08, 20.63, 64.58, 83.80, 55.40, 12.18, 11.27, 70.16, 67.60, 45.33, 92.50, 59.50, 49.87, 68.56,  6.44, 96.95, 68.14, 99.79, 28.05,
 45.24, 79.19, 37.63, 80.05,  6.20, 62.56, 43.35, 32.76
 )

city <- data.frame(id=1:100,x1=X,y1=Y)

#solution
tsp_tour(city)
