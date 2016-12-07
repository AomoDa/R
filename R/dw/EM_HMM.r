



set.seed(100)
x <- matrix(sample(10,size = 200,replace = T),nrow = 1)
len.x=length(x)


#设定初始隐藏状态
z1<-runif(200,min=1,max=3) 
z<-t(as.matrix(sort(round(z1))))

len.z=length(z)

k<-3  #隐藏状态个数设定
p<-matrix(c(.5,0,0.2,.5,.4,0,0,.6,0.8),3,3)

pi<-array(,dim<-c(1,k))
#隐藏状态的初始概率初始值
pi=c(1,0,0)


#E步
#已知隐藏状态序列，求变点时间序列T
function_T1<-function(z,len.z,k){
  # 初始化矩阵
T1<-matrix(NA,nrow = 1,ncol = k-1)
   j=1
   for(i in 1:(len.z-1)){ 
      if(z[1,i]!=z[1,i+1]){
         T1[1,j]=i 
         j=j+1  
      }       
   }
return(T1)
}

function_T1(z,len.z,k)



#已知变点序列，求参数(三个参数，K=3)
function_lamuda<-function(T1){
   lamuda<-array(,dim<-c(1,k))
   Tk<-array(,dim<-c(1,k))
   for(i in 1:k-1){ #通过变点找到每一段失效的总时间
       Tk[1,i]=T1[1,i]
   }
   Tk[1,k]=len.x-T1[1,k-1]
   sum.x=0
   for(i in 1:T1[1,1]){
       sum.x=sum.x+x[i]
   }
   lamuda[1,1]=Tk[1,1]/sum.x
   sum.x=0
   for(i in (T1[1,1]+1):T1[1,2]){
      sum.x=sum.x+x[i]
   }
   lamuda[1,2]=Tk[1,2]/sum.x
   sum.x=0
   for(i in (T1[1,2]+1):len.x){
     sum.x=sum.x+x[i]
  }
  lamuda[1,3]=Tk[1,3]/sum.x
  
return(lamuda)
}

T1=function_T1(z,len.z,k)
len.T1=length(T1)
function_lamuda(T1)


#混淆矩阵赋值
function_hunxiao<-function(z,x,lamuda)
{  #从状态1开始

    for(t in 1:len.x){
       for(j in 1:k)
      B[j,t]=lamuda[j]*exp(-lamuda[1,j]*x[t])
   }
   return(B)
}
lamuda=function_lamuda(T1)
B<-array(,dim<-c(k,len.x))    #混淆矩阵，隐藏状态表现为观察状态的概率
B=function_hunxiao(z,x,lamuda)*10

function_Forward<-function(pi,B,p,k){
  alpha=array(,dim<-c(k,len.x))
  #前向向量
   alpha[,1]=pi*B[,1]
   sum=0
   for(t in 2:len.x){ #t>=2时
      for(i in 1:k){  
          for(j in 1:k){    
          sum=alpha[j,t-1]*p[j,i]*B[i,t]+sum
          }
          alpha[i,t]=sum
          sum=0
      }
   } 
 return(alpha)
}
alpha=function_Forward(pi,B,p,k)
head(alpha)


#后向向量
   
function_Backward<-function(beta,B){
  beta=array(,dim<-c(k,len.x))  #后向向量
   beta[,len.x]=c(1)
   sum=0
   t=(len.x-1)
   while(t>0){
      for(i in 1:k){
          for(j in 1:k){
             sum=beta[j,t+1]*p[i,j]*B[j,t+1]+sum
          }
          beta[i,t]=sum
          sum=0
      }
         t=t-1
   }
return(beta)
}
beta=function_Backward(beta,B)

#转移矩阵重估函数 
function_kesi<-function(k,alpha,p,beta,B){
  kesi<-array(,dim<-c(k,k,(len.x-1))) 
   for(t in 1:(len.x-1)){  #每一个t对应的kesi值
      for(i in 1:k){
         for(j in 1:k){
            kesi[i,j,t]=alpha[i,t]*p[i,j]*beta[j,t+1]*B[j,t+1]
         }
      }   
   }
 return(kesi)
}
kesi=function_kesi(k,alpha,p,beta,B) #kesi分子

function_kesi.new<-function(kesi,k){
   sum=array(,dim=c(1,(len.x-1))) #分母
   n=0
   for(t in 1:(len.x-1)){
      for(i in 1:k){
        for(j in 1:k){
           n=n+kesi[i,j,t]
        }
      }
      sum[1,t]=n
      n=0
   }

   kesi.new=array(,dim<-c(k,k,len.x-1))
    for(t in 1:(len.x-1)){  #获得kesi值
      for(i in 1:k){
         for(j in 1:k){
            kesi.new[i,j,t]=kesi[i,j,t]/sum[1,t]
         }
      }   
   }
return(kesi.new)
}
kesi.new=function_kesi.new(kesi,k)


function_gama<-function(kesi,alpha,beta,k){
   gama<-array(,dim<-c(k,len.x))
   gama.n<-array(,dim<-c(k,len.x))  #分子
   gama.d<-array(,dim<-c(1,len.x))  #分母
   sum=0
   for(t in 1:(len.x)){
      for(i in 1:k){
         gama.n[i,t]=alpha[i,t]*beta[i,t]
      }  
   }
   for(t in 1:len.x){
      for(i in 1:k){
         sum=sum+alpha[i,t]*beta[i,t]
      }
    gama.d[1,t]<-sum
   }

   for(t in 1:len.x){
      for(j in 1:k){
         i=1
         gama[j,t]=gama.n[j,t]/gama.d[i,t]
      }
      i=i+1
   }
return(gama)
}
gama=function_gama(kesi,alpha,beta,k)

function_p.new<-function(kesi.new,gama,k){ #重估公式
  p.new<-array(,dim<-c(k,k))
   sum1=0
   sum.kesi=array(,dim<-c(k,k))
   sum.gama=array(,dim<-c(1,k))
      for(i in 1:k){
         for(j in 1:k){
             for(t in 1:(len.x-1)){
            sum1=kesi[i,j,t]+sum1  
      }
    sum.kesi[i,j]=sum1
       sum1=0
 }
}
sum1=0
   for(i in 1:k){
      for(t in 1:(len.x-1)){
         sum1=sum1+gama[i,t]
      }
      sum.gama[1,i]=sum1
      sum1=0
   }

    for(i in 1:k){
      for(j in 1:k){
         p.new[i,j]=sum.kesi[i,j]/sum.gama[i]
      }
   }

return(p.new)
}
p.new=function_p.new(kesi.new,gama,k)


function_p.end<-function(p.new,k){#转移矩阵归一化
    p.end=array(,array<-c(k,k))
   p.sum=array(apply(p.new,1,sum),dim<-c(1,k))
   for(i in 1:k){
      for(j in 1:k){
         p.end[i,j]=p.new[i,j]/p.sum[1,i]
      }
   }
return(p.end)
}

p=function_p.end(p.new,k)


function_viterbi<-function(B,pi,p){
   segama[,1]=pi*B[,1]
   se<-array(,dim<-c(1,k))#存放Segama[j,t-1]*p[i,j]*B[i,t]
   for(t in 2:len.x){
      for(i in 1:k){
         for(j in 1:k){     
            se[1,j]=segama[j,t-1]*p[j,i]*B[i,t]                
         }  
       segama[i,t]=max(se)
       }
   }
   path[,1]=c(1,2,3)
   max=index=0
   se<-array(0,dim<-c(1,k))
   for(t in 2:len.x){
   for(i in 1:k){
         for(j in 1:k){
            se[1,j]=segama[j,(t-1)]*p[j,i]
         }
        index=which.max(se)
        path[i,t]=index
   }
  
}
   max=0
   index=0

   for(i in 1:k){#最后一个观察状态对应的概率最大的隐藏状态的概率
      if(segama[i,len.x]> max){
         max=segama[i,len.x]
         index=i  #最大值的下标
      }     
   }
   path.end<-array(,dim<-c(1,len.x))  #隐藏状态
   t=len.x
   while(t>0){
      path.end[1,t]=path[index,t]
      index=path[index,t]
      t=t-1
   }
return(path.end)  
}
segama<-array(,dim<-c(k,len.x)) #部分最优路径
path<-array(,dim<-c(k,len.x))  #隐藏状态序列

z=function_viterbi(B,pi,p)

#第一轮初始：隐藏状态K已知，观察值x已知，隐藏序列z已知，变点时间T1已知迭代
function_L<-function(z,B){
   B=B/10
   L=0
   for(t in 1:(len.x-1)){
               f=B[z[1,t],t]*p[z[1,t],z[1,t+1]]
               L=log(f)+L
      }
 L=L+log(B[z[1,len.x],len.x])
   return(L)
}
L1=function_L(z,B)



function_end<-function(){
  end=1
while(end>1e-05){    
   T1=function_T1(z,len.z,k) #求解新变点 
   a=function_lamuda(T1)  #求解新参数  
   B=function_hunxiao(z,x,lamuda)*10 #新的混淆矩阵 
   alpha=function_Forward(pi,B,p,k) #前向
   beta=function_Backward(beta,B) #后向
   kesi=function_kesi(k,alpha,p,beta,B) #kesi分子
   kesi.new=function_kesi.new(kesi,k) #新的kesi值
   gama=function_gama(kesi,alpha,beta,k)  #新的gama值
   p.new=function_p.new(kesi.new,gama,k)  #转移概率矩阵
   p=function_p.end(p.new,k)#概率矩阵标准化
   z=function_viterbi(B,pi,p) #隐藏转态序列
   end=function_L(z,B)-L1 #似然函数值
   L1=function_L(z,B)   
 }
     return(list(a,T1))
}
function_end()
