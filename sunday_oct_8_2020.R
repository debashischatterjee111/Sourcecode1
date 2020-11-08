---
title: "CkeckCode(1)"
author: "Debashis Chatterjee"
date: "11/5/2020"
output: pdf_document
---
```{r}
x = c(1,2,3,4,5,6,7,8,9,10)
y = c(10,20,30,40,50,60,70,80,90,100)

plot(x, y, type="o", xlim=c(0,11), ylim=c(0, 110), 
      xlab="This is X-value", ylab="This is Y-value", main="Line segments")
segments(0,100,2,100, lwd=2, col="blue")

segments(1,90,2,90, lwd=2, col="blue")
# Define a set of n data points
xseg = c(4,5,6,7,4)
yseg = c(0,10,28,15,0)

# define a sequence from 1 to n-1
s = seq(length(xseg)-1)

# connect the sequence of points with line segments successively.
segments(xseg[s],yseg[s], xseg[s+1], yseg[s+1], col="green", lwd=2)

```

```{r}
## Drawing polygons

## First, draw a graph with points and lines with plot()
x = seq(0,20,0.5)
y = 3*x/(2+x)

plot(x,y,cex=0.7, type="o", xlab="Concentration [x]", ylab="d[x]/dt")


## Draw a polygon defining an area on the graph -- This is a red square
xx = c(2.3,7.5,7.5,2.5)
yy = c(1.6,1.6,2.3,2.3)
# When density=0, col refers to the line colour
polygon(xx,yy, density=0, col="red")


## Draw a polygon filled with blue colour.
xx = c(5,8,8,6.5,5)
yy = c(0,0,0.5,0.8,0.5)
# when density is not mentioned, col refers to filling colour.
polygon(xx,yy,col="blue")


## Draw a polygon filled with lines
xx = c(10,10,13)
yy = c(0.5,1.0,0.75)
## polygon is filled with 20 lines per inch, green in colour.
polygon(xx,yy,density=20, col="green", border="black")
```


```{r}
x = seq(0,20,0.5)
y = 30*x/(2+x)

plot(x,y,cex=0.7, type="o", xlab="Concentration [x]", ylab="d[x]/dt")

meanx=c(-2,2,0)
meany=c(1,1,4)
polygon(meanx,meany,density=20, col="green", border="black", xlim=c(-20,20), ylim=c(-4,10))
```

```{r}
x <- c(1:9, 8:1)
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
op <- par(mfcol = c(3, 1))
for(xpd in c(FALSE, TRUE, NA)) {
  plot(1:10, main = paste("xpd =", xpd))
  box("figure", col = "pink", lwd = 3)
  polygon(x, y, xpd = xpd, col = "orange", lty = 2, lwd = 2, border = "red")
}
par(op)
```
```{r}
n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
plot   (xx, yy, type = "n", xlab = "Time", ylab = "Distance")
polygon(xx, yy, col = "gray", border = "red")
title("Distance Between Brownian Motions")

```

```{r}
# Multiple polygons from NA values
# and recycling of col, border, and lty
op <- par(mfrow = c(2, 1))
plot(c(1, 9), 1:2, type = "n")
polygon(1:9, c(2,1,2,1,1,2,1,2,1),
        col = c("red", "blue"),
        border = c("green", "yellow"),
        lwd = 3, lty = c("dashed", "solid"))
plot(c(1, 9), 1:2, type = "n")
polygon(1:9, c(2,1,2,1,NA,2,1,2,1),
        col = c("red", "blue"),
        border = c("green", "yellow"),
        lwd = 3, lty = c("dashed", "solid"))
par(op)
```

```{r}
# Line-shaded polygons
plot(c(-5, 7), c(-2,6), type = "n", main="Raw Simulated Random Triangles", xlab="x",ylab="y")
polygon(1:9, c(2,1,2,1,NA,2,1,2,1),
        density = c(10, 20), angle = c(-45, 45))
```

```{r}
require(Morpho)
```
```{r}
data(boneData)
trafo <- computeTransform(boneLM[,,1],boneLM[,,2])
transLM <- applyTransform(boneLM[,,2],trafo)
```
```{r}
boneLM[,,1]-transLM
```
```{r}
rotonto
```
```{r}
if (require(shapes)) {
lims <- c(min(gorf.dat[,,1:2]),max(gorf.dat[,,1:2]))
rot <- rotonto(gorf.dat[,,1],gorf.dat[,,2]) ### rotate the second onto the first config
plot(rot$yrot,pch=19,xlim=lims,ylim=lims) ## view result
points(gorf.dat [,,1],pch=19,col=2) ## view original config
rev1 <- rotreverse(rot$yrot,rot)
#points(rev1,cex=2) ### show inversion by larger circles around original configuration
}

```
```{r}
rotonto
```


```{r}
#example of Pre-shape space and shape space
plot(c(-5, 10), c(-2,8), type = "n", main="Raw Data-plot of Simulated 5 Random Triangles", xlab="x",ylab="y")
a=c(0,0);b=c(-1,1);c=c(2,1); t1=rbind(a,b,c);
d=c(3,1);e=c(4,4);f=c(5,1); t2=rbind(d,e,f);
g=c(-3,3);h=c(3,4);i=c(3,3); t3=rbind(g,h,i);
j=c(-4,1);k=c(-3,2);l=c(-3,1); t4=rbind(j,k,l);
m=c(8,0);n=c(6,6);o=c(4,0); t5=rbind(m,n,o);

polygon(t1[,1],t1[,2],density=20, col="pink", border="red")
polygon(t2[,1],t2[,2],density=20, col="green", border="forestgreen")
polygon(t3[,1],t3[,2],density=20, col="skyblue", border="blue")
polygon(t4[,1],t4[,2],density=20, col="orange", border="darkorange")
polygon(t5[,1],t5[,2],density=20, col="yellow", border="gold3", lty = 2)
```



```{r}
require(Morpho);require(graphics);require(plotrix);
#example of Pre-shape space and shape space
plot(c(-2, 2), c(-2,2), type = "n", main="Pre  Shape-space-plot of  5 Random Triangles", xlab="x",ylab="y")
a=c(0,0);b=c(-1,1);c=c(2,1); t1=rbind(a,b,c);
d=c(3,1);e=c(4,4);f=c(5,1); t2=rbind(d,e,f);
g=c(-3,3);h=c(3,4);i=c(3,3); t3=rbind(g,h,i);
j=c(-4,1);k=c(-3,2);l=c(-3,1); t4=rbind(j,k,l);
m=c(8,0);n=c(6,6);o=c(4,0); t5=rbind(m,n,o);
rt1=scale(t1);
rt2=scale(t2);
rt3=scale(t3);
rt4=scale(t4);
rt5=scale(t5);

polygon(rt1[,1],rt1[,2],density=20, col="pink", border="red")
polygon(rt2[,1],rt2[,2],density=20, col="green", border="forestgreen")
polygon(rt3[,1],rt3[,2],density=20, col="blue1", border="blue", angle = -45)
polygon(rt4[,1],rt4[,2],density=20, col="orange", border="darkorange")
polygon(rt5[,1],rt5[,2],density=20, col="yellow", border="gold3", lty = 2)
draw.ellipse(x= c(0), y= c(0), c(1), c(1), border = 'gray', lwd = 1, lty=3)
```




```{r}
require(Morpho)
#example of Pre-shape space and shape space
plot(c(-5, 10), c(-2,8), type = "n", main="Kendal Shape-space-plot of  5 Random Triangles", xlab="x",ylab="y")
a=c(0,0);b=c(-1,1);c=c(2,1); t1=rbind(a,b,c);
d=c(3,1);e=c(4,4);f=c(5,1); t2=rbind(d,e,f);
g=c(-3,3);h=c(3,4);i=c(3,3); t3=rbind(g,h,i);
j=c(-4,1);k=c(-3,2);l=c(-3,1); t4=rbind(j,k,l);
m=c(8,0);n=c(6,6);o=c(4,0); t5=rbind(m,n,o);
rt1=t1;
fivetr <- array(rep(1, 3*2*5), dim=c(3, 2, 5))
fivetr[,,1]=t1;
fivetr[,,2]=t2;
fivetr[,,3]=t3;
fivetr[,,4]=t4;
fivetr[,,5]=t5;
rt2=rotonto(t1,t2)$yrot;
rt3=rotonto(t1,t3)$yrot;
rt4=rotonto(t1,t4)$yrot;
rt5=rotonto(t1,t5)$yrot;


polygon(rt1[,1],rt1[,2],density=20, col="pink", border="red")
polygon(rt2[,1],rt2[,2],density=20, col="green", border="forestgreen")
polygon(rt3[,1],rt3[,2],density=20, col="skyblue", border="blue")
polygon(rt4[,1],rt4[,2],density=20, col="orange", border="darkorange")
polygon(rt5[,1],rt5[,2],density=20, col="yellow", border="gold3", lty = 2)
```
```{r}
require(Morpho)
#example of Pre-shape space and shape space
plot(c(-1.5, 2), c(-1,2), type = "n", main="Kendal Shape-space-plot of those 5 Random Triangles", xlab="x",ylab="y")
a=c(0,0);b=c(-1,1);c=c(2,1); t1=rbind(a,b,c);
d=c(3,1);e=c(4,4);f=c(5,1); t2=rbind(d,e,f);
g=c(-3,3);h=c(3,4);i=c(3,3); t3=rbind(g,h,i);
j=c(-4,1);k=c(-3,2);l=c(-3,1); t4=rbind(j,k,l);
m=c(8,0);n=c(6,6);o=c(4,0); t5=rbind(m,n,o);
rt1=t1;
rt2=rotonto(t1,t2,scale = T)$yrot;
rt3=rotonto(t1,t3,scale = T)$yrot;
rt4=rotonto(t1,t4,scale = T)$yrot;
rt5=rotonto(t1,t5,scale = T)$yrot;

polygon(rt1[,1],rt1[,2],density=20, col="pink", border="red")
polygon(rt2[,1],rt2[,2],density=20, col="green", border="forestgreen")
polygon(rt3[,1],rt3[,2],density=20, col="skyblue", border="blue")
polygon(rt4[,1],rt4[,2],density=20, col="orange", border="darkorange")
polygon(rt5[,1],rt5[,2],density=20, col="yellow", border="gold3", lty = 2)
```

```{r}
myData = apes$x
for(i in 1:dim(myData)[3])
{myData[,,i]=
Morpho::rotonto(myData[,,1],myData[,,i])$yrot}
ape5000=MCMCpostPsample2D(1.5,
rep(0.1,1),myData,5000)
```

```{r}
cct=ct=ccq
vvt=vt=cvq
```

```{r}
for(i in 1:dim(ct)[3])
{cct[,,i]=
 Morpho::rotonto(ct[,,1],ct[,,i])$yrot
}

for(i in 1:dim(ct)[3])
{vvt[,,i]=
 Morpho::rotonto(vt[,,1],vt[,,i])$yrot
}
```

```{r}


cct5000.10=MCMCpostPsample2D(1.5,rep(0.1,1),cct[,,1:10],5000)
PPLOTpostvar2D(cct5000.10, 1000)

```

```{r}

vvt5000.10=c()
vvt5000.20=c()
vvt5000.50=c()
vvt5000.100=c()




  vvt5000.10=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:10],5000);
  vvt5000.20=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:20],5000);
  vvt5000.50=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:50],5000);
  vvt5000.100=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:100],5000);
```

```{r}
vvt5000=cbind(vvt5000.10,vvt5000.20,vvt5000.50,vvt5000.100);
#PPLOTpostvar2D(vvt5000.100, 1000)

```



```{r}

cct5000.10=c()
cct5000.20=c()
cct5000.50=c()
cct5000.100=c()




  cct5000.10=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:10],5000);
  cct5000.20=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:20],5000);
  cct5000.50=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:50],5000);
  cct5000.100=MCMCpostPsample2D(1.5,rep(0.1,1),vvt[,,1:100],5000);
```

```{r}
cct5000=cbind(cct5000.10,cct5000.20,cct5000.50,cct5000.100);
#PPLOTpostvar2D(vvt5000.100, 1000)

```



```{r}
theta=1.3;
#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(vvt5000[,1]), xlim=c(0.7,1.7),ylim=c(0,12),col="purple", xlab=expression(tilde(theta)),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of")~~tilde(theta)~~("+ uniform prior"))))
abline(v=theta, col="black",lwd=2, lty=1)
lines(density(vvt5000[,2]), ylim=c(0,12),col="blue")
lines(density(vvt5000[,3]), ylim=c(0,12),col="forestgreen")
lines(density(vvt5000[,4]), ylim=c(0,12),col="red")

legend("topright",cex=1, c("n=10","n=20","n=50","n=100", expression(theta_0)), lty = c(1,1,1,1,1), col = c("purple","blue","forestgreen","red", "black"), lwd = c(1,1,1,1,2))
```



```{r}
theta=0.8;
#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(cct5000[,1]), xlim=c(0.2,1.7),ylim=c(0,12),col="purple", xlab=expression(tilde(theta)),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of")~~tilde(theta)~~("+ uniform prior"))))
abline(v=theta, col="black",lwd=2, lty=1)
lines(density(cct5000[,2]), ylim=c(0,12),col="blue")
lines(density(cct5000[,3]), ylim=c(0,12),col="forestgreen")
lines(density(cct5000[,4]), ylim=c(0,12),col="red")

legend("topright",cex=1, c("n=10","n=20","n=50","n=100", expression(theta_0)), lty = c(1,1,1,1,1), col = c("purple","blue","forestgreen","red", "black"), lwd = c(1,1,1,1,2))
```







```{r}
tt=rep(1,1)
for(i in 1:30){
s=Pstep2D(tt,rep(0.01,1),cct)
print(s)
tt=Ppurturb2D(tt, rep(0.01,1))
}
```

