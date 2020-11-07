install.packages("devtools")
#You'll also need roxygen2 for documenting your functions (see below).
devtools::install_github("klutometis/roxygen")

library(roxygen2)


require(devtools)
require(roxygen2)
require(usethis)

#install.packages("roxygen2")
#The first thing you want to do is create the framework for your R package. We can do this using devtools:


#devtools::use_data(APE)

APE=data(apes)

BONE1=data(boneData)
SCP=data(scallopPLY)
DIGIT3=data(digit3.dat)
usethis::use_data(APE)
usethis::use_data(BONE1)
usethis::use_data(SCP)
usethis::use_data(DIGIT3)

#install("NBpagm")

devtools::document()



# now function to install your new package directly from the GitHub page.
#Test for Installation
#https://r-pkgs.org/git.html

## https://github.com/petrkeil/bPCA/blob/master/R/bPCA_functions.r

## https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html



devtools::load_all()
sinew::makeOxygen(pendulum, add_fields = "source")

devtools::use_vignette("introduction")

usethat::use_vignette("introduction")
#install_github('cats','debashischatterjee111')

#This automatically adds in the .Rd files to the man directory, and adds a NAMESPACE file to the main directory


require(devtools)
install_github("debashischatterjee111/BPviGM1")




#Triangle data generation
#install.packages("sinew")
#devtools::install_github("mdlincoln/docthis")

save(mydata, file="data/mydata.RData")

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



save(fivetr, file="data/fivetr.RData")




Fivetr=data(fivetr)
usethis::use_data(Fivetr)


devtools::document()

#Triangle data generation
#install.packages("sinew")
#devtools::install_github("mdlincoln/docthis")

save(mydata, file="data/mydata.RData")
###########################################


#Random Convex and Concave Quadrilateral data generation
#(sigma=2)


require(mvtnorm);
require(scales);
a=c(-6,4);b=c(0,6);c=c(6,-2);d=c(0,-2); q1=rbind(a,b,c,d);
e=c(20,-3);f=c(15,4); g=c(20,2);h=c(24,6);q2=rbind(e,f,g,h);
cvq1 <- array(rep(1, 4*2*1), dim=c(4, 2, 1));
ccq1 <- array(rep(1, 4*2*1), dim=c(4, 2, 1));
cvq1[,,1]=q1;
ccq1[,,1]=q2;

rnv=function(h){
  s=1.5;
  return(rmvnorm(1, mean=h, sigma=(s^2)*diag(2)))
}
rnc=function(h){
  s=0.8;
  return(rmvnorm(1, mean=h, sigma=(s^2)*diag(2)))
}

cvq <- array(rep(1, 4*2*1000), dim=c(4, 2, 1000))
ccq <- array(rep(1, 4*2*1000), dim=c(4, 2, 1000))
cvq[,,1]=q1;
ccq[,,1]=q2;
for(k in 2:1000)
{
  aa=c(-6,4);bb=c(0,6);cc=c(6,-2);dd=c(0,-2);
  qq1=rbind(rnv(aa),rnv(bb),rnv(cc),rnv(dd));
  ee=c(20,-3);ff=c(15,4); gg=c(20,2);hh=c(24,6);
  qq2=rbind(rnc(ee),rnc(ff),rnc(gg),rnc(hh));
  cvq[,,k]=qq1;
  ccq[,,k]=qq2;
}


save(cvq, file="data/cvq.RData");
save(ccq, file="data/ccq.RData");




plot(c(-10, 30), c(-12,10), type = "n", main=" Simulated RawData: 1000 Random convex & concave Quadrilaterals",cex.main=0.98, xlab="x",ylab="y")

polygon(cvq[,1,1],cvq[,2,1],density=0, col="pink", border="red", lwd=2)
for(k in 1:1000)
{
  polygon(cvq[,1,k],cvq[,2,k],density=0, col="pink", border=alpha(rgb(1,0,0), 0.018), lty=5, lwd=1)
}

polygon(ccq[,1,1],ccq[,2,1],density=0, col="blue", border="blue", lwd=2)
for(k in 1:1000)
{
  polygon(ccq[,1,k],ccq[,2,k],density=0, col="green", border=alpha(rgb(0,0,1), 0.018), lty=2, lwd=1)
}


legend("bottomright", legend=c("1000 convex Quadrilaterals, true sigma=1.5", "1000 concave quadrilaterals, true sigma=0.8"),
       col=c("red", "blue"), lty=c(1,1), cex=0.65,
       title="Object shape types", text.font=4, bg='white')

#col=alpha(rgb(1,0,0), 0.3)


polygon(fivetr[,1,1],fivetr[,2,1],density=20, col=k, border="red")
polygon(fivetr[,1,2],fivetr[,2,2],density=20, col="green", border="forestgreen")





fivetr[,,2]=t2;
fivetr[,,3]=t3;
fivetr[,,4]=t4;
fivetr[,,5]=t5;



save(fivetr, file="data/fivetr.RData")




Fivetr=data(fivetr)
usethis::use_data(Fivetr)


devtools::document()
