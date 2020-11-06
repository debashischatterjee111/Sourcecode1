rm(list=ls())
require(MASS)
require(mvtnorm)
seed = 100
set.seed(seed)
require(shapes)
require(Morpho)
choice=10
p=8
tauc=0.1
taub=0.1
data("apes")
myData = apes$x
M = myData[,,1]
W=myData[,,choice]
#colnames(M)=c("zx","zy")
#colnames(W)=c("wx","wy")
Z=M
Y=W
th=1
c=1:2
b=1
sig=0.1
dimm=d=2
mu=W
for(i in 1:p)
{mu[i,1]=c[1]+ (b*(cos(th)*Z[i,1]))+(b*(sin(th)*Z[i,2]))
mu[i,2]=c[2]+ (b*(-sin(th)*Z[i,1]))+(b*(cos(th)*Z[i,2]))
}


fratio <- function(t1, t2)
{
  c=c(t1[1],t1[2]);
  b=t1[3];
  th=t1[4];
  sig=t1[5];

  cc=c(t2[1],t2[2]);
  bb=t2[3];
  tth=t2[4];
  ssig=t2[5];

  #Building mu_1, mu_2

  mu1=mu2=W;
  for(i in 1:p)
  {mu1[i,1]=c[1]+ (b*(cos(th)*Z[i,1]))+(b*(sin(th)*Z[i,2]))
  mu1[i,2]=c[2]+ (b*(-sin(th)*Z[i,1]))+(b*(cos(th)*Z[i,2]))
  }

  for(i in 1:p)
  {mu2[i,1]=cc[1]+ (bb*(cos(tth)*Z[i,1]))+(bb*(sin(tth)*Z[i,2]))
  mu2[i,2]=cc[2]+ (bb*(-sin(tth)*Z[i,1]))+(bb*(cos(tth)*Z[i,2]))
  }


  sum1=0
  for(i in 1:p)
  { sum1= sum1+dmvnorm(W[i,], mean=mu1[i,], sigma=(sig^2)*diag(2),log=T)
  }
  f11=sum1;

  sum2=0
  for(i in 1:p)
  { sum2= sum2+dmvnorm(W[i,], mean=mu2[i,], sigma=(ssig^2)*diag(2),log=T)
  }
  f22=sum2;


  #print(f22)
  return(exp(f11-f22))
}



q <- function(t) {
  e = abs(rnorm(1,0,1))

  c=c(t[1],t[2]);
  b=t[3];
  th=t[4];
  sig=t[5];
  #rnorm(1, x, 0.1)
  u1 = runif(1,0,1)
  if( u1<0.5 )
  { new_c_1 = c[1]+0.5*e
  }
  else
  { new_c_1 = c[1]-0.5*e
  }

  u2 = runif(1,0,1)
  if( u2<0.5 )
  { new_c_2 = c[2]+0.5*e
  }
  else
  { new_c_2 = c[2]-0.5*e
  }

  new_c=c(new_c_1, new_c_2)

  ub = runif(1,0,1)
  if( ub<0.5 )
  { new_b = b+0.5*e
  }
  else
  { new_b = b-0.5*e
  }


  ut = runif(1,0,1)
  if( ut<0.5 )
  { new_th = th+0.5*e
  }
  else
  { new_th = th-0.5*e
  }

  us = runif(1,0,1)
  if( us<0.5 )
  { new_sig = sig+0.5*e
  }
  else
  {  new_sig = min(0.01,sig-0.5*e)
  }

  g=c(new_c[1], new_c[2],new_b, new_th,new_sig)
  return(g)

}

step <- function(t, q) {

  ## Pick new point
  tp <- q(t)

  ## Acceptance probability:
  alpha <- min(1, fratio(tp,t))
  ## Accept new point with probability alpha:
  if (runif(1) < alpha)
    t <- tp
  ## Returning the point:
  return(t)
}

step(rep(1,5),q)
#require(tcltk)
#require(progress)
#q2 <- function(x) rnorm(1, x, 0.08)

run <- function(t, q, nsteps)
  {
  res <- matrix(NA, nsteps, length(t))
  ptm <- proc.time()
  pb <- txtProgressBar(min = 0, max = nsteps, style = 3, char="=>")
  for (i in seq_len(nsteps))
    {
    res[i,] <- t <- step(t, q)

   # print(i)
   # if (i == nsteps) cat(': Done')
    # else cat('\014')
    #progress(i,progress.bar = T)


    if(i==1| i==round(nsteps/9)|i==round(nsteps/8)|i==round(nsteps/7)|i==round(nsteps/6)|i==round(nsteps/5)|i==round(nsteps/4)|i==round(nsteps/3)|i==round(nsteps/2)|i==round(nsteps/1.5)|i==round(nsteps/1.3)|i==round(nsteps/1.2)|i==round(nsteps))
      #{print(i)}
         { setTxtProgressBar(pb, i)
      cat(paste0('current sample:[', i,']   mcmc_run: ', round(i/ (nsteps-1) * 100), '% completed'))
     # print("**")
    }

    if (i == nsteps)
    { print("***");
      cat(': Done :');
      print(" ");
      print("***");
    }
       #else cat('\014')

  }
  print(proc.time()-ptm)
  drop(res)

}

#run
ress_10 <- run(rnorm(5, mean=1, sd=1),  q, 2000)

#par(mfrow=c(1,2))

#Plot with par

par()              # view current settings
opar <- par()      # make a copy of current settings
par(col.lab="red") # red x and y labels
hist(mtcars$mpg)   # create a plot with these new settings
par(opar)          # restore original settings
par(mfrow = c(1,1))
par(mfrow = c(1,1)) ## some random par change

#par(resetPar())

for(i in 1:5)
{
  plot(ress_10[1001:10000,i], type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1)
}
par()

1

dev.off()
par(def.par)
plot(ress_10[1001:10000,1], type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1)

