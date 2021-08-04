X=read.table("C:\\Users\\User\\Desktop\\Timber.csv",header=T,sep=",") 
attach(X) 
par(mfrow=c(3,2))
for(j in 5:10) 
{
boxplot(X[,j]~Locality,ylab=colnames(X)[j], main=paste(colnames(X)[j],
"versus locality")) 
} 
par(mfrow=c(3,2))
for(j in 5:10) 
{
boxplot(X[,j]~YEAR,ylab=colnames(X)[j], main=paste(colnames(X)[j],"versus year")) 
}
par(mfrow=c(3,2))
for(j in 5:10)
{
boxplot(X[,j]~Subculture,ylab=colnames(X)[j], main=paste(colnames(X)[j],
"versus subculture"))
} 
par(mfrow=c(3,2)) 
for(j in 5:10)
{
hist(X[,j],ylab=colnames(X)[j],xlab="", main=paste("Histogram
of",colnames(X)[j])) 
x=seq(-3,3,by=.001)
par(new=T)
plot(dnorm(x),type="l",xaxt="n",yaxt="n",xlab="",ylab="") 
}
cor(X)
x=cor(SeedWidth,SeedLength) 
y=(x/sqrt(1-x^2))*sqrt(length(SeedWidth)-1) 
p=1-pt(y,df=length(SeedWidth)-1))
a=Germination[YEAR==2000] 
b=Germination[YEAR==2002]
c=Rooting[YEAR==2000]
d=Rooting[YEAR==2002]
var.test(a,b) 
var.test(c,d) 
t.test(a,b) 
t.test(c,d)
aov(Elongation~as.factor(Locality))
aov(Multiples~as.factor(Locality))
aov(Rooting~as.factor(Locality))
aov(Germin~as.factor(Locality))
aov(SeedL~as.factor(Locality))
aov(Elongation~as.factor(REPLICATE))
aov(Multiples~as.factor(REPLICATE))
aov(Rooting~as.factor(REPLICATE))
aov(Germin~as.factor(REPLICATE))
aov(SeedL~as.factor(REPLICATE))
aov(Elongation~as.factor(Subculture))
aov(Multiples~as.factor(Subculture))
aov(Rooting~as.factor(Subculture))
aov(Germin~as.factor(Subculture))
aov(SeedL~as.factor(Subculture))
aov(Elongation~as.factor(YEAR))
aov(Multiples~as.factor(YEAR))
aov(Rooting~as.factor(YEAR))
aov(Germin~as.factor(YEAR))
aov(SeedL~as.factor(YEAR))



