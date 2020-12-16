library(scales)
#Reading the file
df=read.csv("data.csv")
#taking back up
ec=df
ec1=df
set.seed(30)

# function for assigning the  cluster
ClusterAssignment=function(K,df,RValueNew)
{
  # dissimilarity matrix
  m <- as.matrix(dist(df,method="euclidean",diag=FALSE,upper=FALSE)) 
  #just initializing the vector
  a=0 
 # looping to check which centorid its near
for(i in 1:500){
for (j in 1:K) 
{
  M=RValueNew[j]
  a[j]=m[M,i]
}
b=which.min(a)
# assigning the clss
df$class[i]=b
}
  return(df)
}


# function for re-calculating the centroid

RecalculatingCentroid=function(K,RValueNew,df8)
{
  for(i in 1:K)
    {
      df1=df8[df8$class==i,]
      write.csv(df1,"ClusterSub.csv")
      m1<- as.matrix(dist(df1,method="euclidean",diag=FALSE,upper=FALSE))
      d=which.min(rowSums(m1))
      df2=read.csv("ClusterSub.csv")
      e=df2[d,1]
      #print(e)
      RValueNew[i]=e
    }
  return(RValueNew)
}

# function for calculating Silhouette distance for K=3
SilhoutedistanceK3=function(df3,df4,df5)
{
  c1=nrow(df3)
  for(l in 1:c1)
  {
    df3$avgdistanc1[l]=mean(sqrt((df4[,1] - df3[l,1])^2 + (df4[,2] - df3[l,2])^2+(df4[,3] - df3[l,3])^2))
    df3$avgdistanc2[l]=mean(sqrt((df5[,1] - df3[l,1])^2 + (df5[,2] - df3[l,2])^2+(df5[,3] - df3[l,3])^2))
    df3$b[l]=min(df3[l,5],df3[l,6])
  }
  m2<- as.matrix(dist(df3,method="euclidean",diag=FALSE,upper=FALSE))
  for (p1 in 1:c1) {
    d=mean(m2[p1,])
    df3$a[p1]=d
    s=(df3$b[p1]-df3$a[p1])/max(df3$a[p1],df3$b[p1])
    df3$s1[p1]=s
  }
  
  return(df3)
}


# function for calculating Silhouette distance for K=4

SilhoutedistanceK4=function(df3,df4,df5,df6)
{
  c1=nrow(df3)
  for(l in 1:c1)
  {
    df3$avgdistanc1[l]=mean(sqrt((df4[,1] - df3[l,1])^2 + (df4[,2] - df3[l,2])^2+(df4[,3] - df3[l,3])^2))
    df3$avgdistanc2[l]=mean(sqrt((df5[,1] - df3[l,1])^2 + (df5[,2] - df3[l,2])^2+(df5[,3] - df3[l,3])^2))
    df3$avgdistanc3[l]=mean(sqrt((df6[,1] - df3[l,1])^2 + (df6[,2] - df3[l,2])^2+(df6[,3] - df3[l,3])^2))
    df3$b[l]=min(df3[l,5],df3[l,6],df3[l,7])
  }
  m2<- as.matrix(dist(df3,method="euclidean",diag=FALSE,upper=FALSE))
  for (p1 in 1:c1) {
    d=mean(m2[p1,])
    df3$a[p1]=d
    s=(df3$b[p1]-df3$a[p1])/max(df3$a[p1],df3$b[p1])
    df3$s1[p1]=s
  }
  
  return(df3)
}

 

###############For the First K, K =3
K=3
RValue<- sample(1:500, K, replace=F)
print(RValue)

# For the 1st clustering choosing Random Centroids:
df_1st=ClusterAssignment(K,df,RValue)
df8=df_1st


# looping it maximum 100 times or until cluster assignments do not change
for(m in 1:100 )
{
#Recalucaluting the centroid:
RValueOld=RValue
RValueNew=RecalculatingCentroid(K,RValueOld,df8)

if (!isTRUE(all.equal(RValueOld,RValueNew)))
  {
  df8=ClusterAssignment(K,df,RValueNew)
  RValue=RValueNew
}
  else {
    print(paste0("Value of number of loops:", m))
    break
}
}


ec$Class=df8$class
#saving the file for K=3
write.csv(ec,"clusters_3.csv")


#calculating average Silhouette distance
cluster1 =subset(df8, class==1 )
cluster2 =subset(df8, class==2 )
cluster3 =subset(df8, class==3 )
a1=SilhoutedistanceK3(cluster1,cluster2,cluster3)
a2=SilhoutedistanceK3(cluster2,cluster3,cluster1)
a3=SilhoutedistanceK3(cluster3,cluster1,cluster2)

a4=rbind(a1,a2,a3)
FinalSilhoutedistanceK3= mean(a4$s1)
print(paste0("Silhouette distance with K=3 is: ",FinalSilhoutedistanceK3))

################# For the second K,K=4
K=4

RValue<- sample(1:500, K, replace=F)
print(RValue)

# For the 1st clustering choosing Random Centroids:
df_1st=ClusterAssignment(K,df,RValue)
df8=df_1st


# looping it maximum 100 times or until cluster assignments do not change
for(m in 1:100 )
{
  #Recalucaluting the centroid:
  RValueOld=RValue
  RValueNew=RecalculatingCentroid(K,RValueOld,df8)
  
  if (!isTRUE(all.equal(RValueOld,RValueNew)))
  {
    df8=ClusterAssignment(K,df,RValueNew)
    RValue=RValueNew
  }
  else {
    print(paste0("Value of number of loops:", m))
    break
  }
}

ec1$Class=df8$class

#saving the file for K=4
write.csv(ec1,"clusters_4.csv")


#calculating average Silhouette distance
cluster11 =subset(df8, class==1 )
cluster21 =subset(df8, class==2 )
cluster31 =subset(df8, class==3 )
cluster41 =subset(df8, class==4 )
a1=SilhoutedistanceK4(cluster11,cluster21,cluster31,cluster41)
a2=SilhoutedistanceK4(cluster21,cluster31,cluster11,cluster41)
a3=SilhoutedistanceK4(cluster31,cluster11,cluster21,cluster41)
a4=SilhoutedistanceK4(cluster41,cluster11,cluster21,cluster31)

a5=rbind(a1,a2,a3,a4)
FinalSilhoutedistanceK4= mean(a5$s1)
print(paste0("Silhouette distance with K=3 is: ",FinalSilhoutedistanceK3))
print(paste0("Silhouette distance with K=4 is :",FinalSilhoutedistanceK4))
  
  
  







