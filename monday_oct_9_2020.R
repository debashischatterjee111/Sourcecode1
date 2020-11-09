---
title: "Classical Shape Ana (Draft)"
author: "Debashis Chatterjee"
date: "10/19/2020"
output: pdf_document
---






```{r}
require(shapes)
require(Morpho)
```

```{r}
data(apes)
proc <- procSym(apes$x)

```
```{r}
procSym

```

```{r}
par(mfrow=c(1,2))
plotshapes(apes$x[,,apes$group=="gorf"],symbol="f")
plotshapes(apes$x[,,apes$group=="gorm"],symbol="m")
```

```{r}
data(gorf.dat)
data(gorm.dat)
bookf<-bookstein2d(gorf.dat)
bookm<-bookstein2d(gorm.dat)
plotshapes(bookf$mshape,bookm$mshape,joinline=c(1,6,7,8,2,3,4,5,1))
```

```{r}
bookstein2d
```

```{r}
data(brains)
# plot first three brains
shapes3d(brains$x[,,1:3])
```


```{r}
data(digit3.dat)
k<-dim(digit3.dat)[1]
n<-dim(digit3.dat)[3]
plotshapes(digit3.dat,joinline=c(1:13))
```


```{r}
require(shapes)
data(gorf.dat)
data(gorm.dat)

ogf=gf=gorf.dat
ogm=gm=gorm.dat
```

```{r}
plotshapes(gm[,,1],joinline=c(1,6,7,8,2,3,4,5,1), col=1:8, symbol=1:8)
polygon(gm[,1,1],gm[,2,1])
```

```{r}

```


```{r}
dim(ogm)
```


```{r}
sortcoldat=function(dat,seq)
{dat[,,]=dat[seq,,]
  return(dat)
}
```

```{r}
ogf=sortcoldat(ogf,c(1,6,7,8,2,3,4,5))
ogm=sortcoldat(ogm,c(1,6,7,8,2,3,4,5))
```



```{r}
plot(c(min(gm[,1,])-30, 100+max(gm[,1,])), c(min(gm[,2,])-30, max(gm[,2,])+30), type = "n", main="Raw Data-plot of Apes: male & female", xlab="x",ylab="y")
#plotshapes(gm[,,1],joinline=c(1,6,7,8,2,3,4,5,1), col=1:8, symbol=1:8)

polygon(ogm[,1,1],ogm[,2,1],density=0, col="skyblue", border="blue")
points(ogm[,1,1],ogm[,2,1], col="blue",pch=1:8)
polygon(ogf[,1,1],ogf[,2,1],density=0, col="pink", border="red")
points(ogf[,1,1],ogf[,2,1], col="red",pch=1:8)
```

```{r}
require(BPviGM1)
apem10000.10=MCMCpostPsample2D(1.5,
rep(0.5,1),ogm[,,1:10],10000)
```
```{r}
PPLOTpostvar2D(apem10000.10, 1000)
```



```{r}
apem10000.20=MCMCpostPsample2D(1.5,
rep(0.5,1),ogm[,,1:20],10000)
```
```{r}
PPLOTpostvar2D(apem10000.20, 1000)
```

```{r}
apem10000.30=MCMCpostPsample2D(1.5,
rep(0.5,1),ogm[,,],10000)
```

```{r}
PPLOTpostvar2D(apem10000.30, 1000)
```


```{r}
apef10000.10=MCMCpostPsample2D(1.5,
rep(0.5,1),ogf[,,1:10],10000)
```

```{r}
apef10000.20=MCMCpostPsample2D(1.5,
rep(0.5,1),ogf[,,1:20],10000)
```


```{r}
apef10000.30=MCMCpostPsample2D(1.5,
rep(0.5,1),ogf,10000)
```



```{r}

#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(apem10000.10), xlim=c(10,16),ylim=c(0,4),col="skyblue", xlab=expression(("Bayes Estimate of ")~~tilde(sigma)),ylab="MCMC Posterior Sigma Density",cex.main=1,main=expression(paste(bold("MCMC  posterior of APE (male)")~~tilde(sigma)~~("+ uniform prior"))))
#abline(v=theta, col="black",lwd=2, lty=1)
abline(v=mean(apem10000.30), col="darkblue",lwd=2, lty=3)
#lines(density(apem10000.20), ylim=c(0,12),col="blue")
lines(density(apem10000.20), ylim=c(0,12),col="blue")
lines(density(apem10000.30), ylim=c(0,12),col="blueviolet")

legend("topleft",cex=0.6, c("n=10","n=20","n=30", expression(("(Bayes Estimate of ")~~tilde(sigma))), lty = c(1,1,1,3), col = c("skyblue","blue","blueviolet","darkblue"), lwd = c(1,1,1,1))
```




```{r}

#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(apef10000.10), xlim=c(10,16),ylim=c(0,4),col="orange", xlab=expression(("Bayes Estimate of ")~~tilde(sigma)),ylab="MCMC Posterior Sigma Density",cex.main=1,main=expression(paste(bold("MCMC  posterior of APE (female)")~~tilde(sigma)~~("+ uniform prior"))))
#abline(v=theta, col="black",lwd=2, lty=1)
abline(v=mean(apef10000.30), col="red4",lwd=2, lty=3)
#lines(density(apem10000.20), ylim=c(0,12),col="blue")
lines(density(apef10000.20), ylim=c(0,12),col="hotpink")
lines(density(apef10000.30), ylim=c(0,12),col="red")

legend("topleft",cex=0.6, c("n=10","n=20","n=30", expression(("(Bayes Estimate of ")~~tilde(sigma))), lty = c(1,1,1,3), col = c("orange","hotpink","red","red4"), lwd = c(1,1,1,1))
```




```{r}
ape5000=MCMCpostPsample2D(1.5,
rep(0.1,1),myData,5000)
```

```{r}
plotshapes(apes$x[,,1], apes$x[,,2],joinline=c(1,6,7,8,2,3,4,5,1), col=1:8, symbol=1:8)
```

```{r}
make.mvn <- function(mean, vcv) {
  logdet <- as.numeric(determinant(vcv, TRUE)$modulus)
  tmp <- length(mean) * log(2 * pi) + logdet
  vcv.i <- solve(vcv)

  function(x) {
    dx <- x - mean
    exp(-(tmp + rowSums((dx %*% vcv.i) * dx))/2)
  }
}
```


```{r}
mu=matrix(rep(1,16),8,2)
for(i in c(1,6,7,8,2,3,4,5,1))
{
  mu[i,]=apes$x[i,,1]
}
```


```{r}
sigma=50
  vcviso=(sigma^2)*matrix(c(1,0.5,0.5,1),2,2)
  
```

```{r}
f1=make.mvn(mu[1,],vcviso)
```
```{r}
f1=make.mvn(mu[1,],vcviso)
f2=make.mvn(mu[2,],vcviso)
f3=make.mvn(mu[3,],vcviso)
f4=make.mvn(mu[4,],vcviso)
f5=make.mvn(mu[5,],vcviso)
f6=make.mvn(mu[6,],vcviso)
f7=make.mvn(mu[7,],vcviso)
f8=make.mvn(mu[8,],vcviso)

```
```{r}
f <- function(x)
    f1(x) + f2(x)+f3(x)+f4(x)+f5(x)+f6(x)+f7(x)+f8(x)
```


```{r}

x <- seq(-100, 200, length=300)
y <- seq(-100, 300, length=300)
xy <- expand.grid(x=x, y=y)
z <- matrix(apply(as.matrix(xy), 1, f), length(x), length(y))

image(x, y, z, las=1)
contour(x, y, z, add=TRUE)
```


```{r}
mu1 <- c(-1, 1)
mu2 <- c(2, -2)
vcv1 <- matrix(c(1, .25, .25, 1.5), 2, 2)
vcv2 <- matrix(c(2, -.5, -.5, 2), 2, 2)
f1 <- make.mvn(mu1, vcv1)
f2 <- make.mvn(mu2, vcv2)
f <- function(x)
    f1(x) + f2(x)

x <- seq(-5, 6, length=71)
y <- seq(-7, 6, length=61)
xy <- expand.grid(x=x, y=y)
z <- matrix(apply(as.matrix(xy), 1, f), length(x), length(y))

image(x, y, z, las=1)
contour(x, y, z, add=TRUE)
```



```{r}
plotshapes(apes$x[,,1],joinline=c(1,6,7,8,2,3,4,5,1))


```
```{r}
data(gorf.dat)
data(gorm.dat)
plotshapes(gorf.dat,gorm.dat,joinline=c(1,6,7,8,2,3,4,5,1))
```






```{r}

data(macm.dat)
data(macf.dat)
plotshapes(macm.dat,macf.dat)
```




```{r}
data(dna.dat)
plotshapestime3d(dna.dat)
```









