library(R.matlab)
library(gdata)
library(rpart)
library(rlist)
library(kohonen)
library(tidyverse)
library(cluster)
file<- 'D:/data mining/Stocks/aamc.us.txt'
df<-read.delim2(file, header = TRUE, sep = ",", dec = ",")
df_c<-as.matrix(as.numeric(as.character(df$Close)))
writeMat('serv.mat', df_c=df_c)

xt <- read.csv(file="serv.csv", header=FALSE)
test <- read.csv(file="test.csv", header=FALSE)
pro <- as.numeric(read.csv(file="pro.csv", header=FALSE))
fit1<-rpart(V31~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13
           +V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24
           +V25+V26+V27+V28+V29+V30,data=xt,method="anova")
fit2<-rpart(V32~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13
            +V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24
            +V25+V26+V27+V28+V29+V30,data=xt,method="anova")
fit3<-rpart(V33~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13
            +V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24
            +V25+V26+V27+V28+V29+V30,data=xt,method="anova")
fit4<-rpart(V34~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13
            +V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24
            +V25+V26+V27+V28+V29+V30,data=xt,method="anova")
fit5<-rpart(V35~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13
            +V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24
            +V25+V26+V27+V28+V29+V30,data=xt,method="anova")
printcp(fit1)
plotcp(fit1)
summary(fit1)
plot(fit1, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit1, use.n=TRUE, all=TRUE, cex=.8)
t1<-names(fit1$var)
t2<-names(fit2$var)
t3<-names(fit3$var)
t4<-names(fit4$var)
t5<-names(fit5$var)
t<-c(t1,t2,t3,t4,t5)
tt<-unique(t)
x_se<-xt[,tt]
t_se<-xt[,c(31:35)]
final<-as.matrix(cbind(x_se,t_se))

#SOM
som_grid <- somgrid(xdim = 3, ydim=2, topo="rectangular") # define rectangular type grid
som_model <- som(final, grid=som_grid, rlen=150, alpha=c(0.05,0.01), 
                 keep.data = TRUE)
plot(som_model, type = "mapping", pchs = 20, main = "Mapping Type SOM")
cen<-data.frame(som_model$codes) 

#K-means
kct <- kmeans(final, 6)
cen<-data.frame(kct$centers)
clusplot(final, kct$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

n<-length(cen)
cen$max1 <- apply(cen[, c(1:(n-5))], 1, max)
cen$max2 <- apply(cen[, c((n-4):n)], 1, max)
cen<-cen %>%
  mutate(profit=(max2>max1))
test_final<-test[,tt] 
cen_x<-rbind(cen[,1:length(tt)],test_final) 
  
dis<-dist(cen_x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
print(dis, diag = NULL, upper = NULL,
      digits = getOption("digits"), justify = "none",
      right = TRUE)
dis<-as.matrix(dis)
clus<-apply( dis[,1:6], 1, which.min)
pro2<-as.numeric(cen$profit[clus[-c(1:6)]])

predict<-data.frame('prediction'=pro2,'reality'=pro) %>%
  mutate(correctness=(pro2==pro))
