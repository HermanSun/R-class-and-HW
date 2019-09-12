
######################
#Unconstrained model##
######################

#Y: Dependent Variable
#X: Independent Variables
#K: Prespecified Number of Segments
#T0: Burn-In
#T: Sampling after Burn-in
#LML: Optional, if you want to calculate type LML in that position (Note: it takes some additional time).
#Opt_Hyper: Optional for setting Hyperparameters. Default values are a=10;b=50;sp1=2;sp2=0.5;r1=2;r2=0.01;

Unconstraind_Bayes_Mixture<-function(Y,X,K,T0,T,LML,Opt_Hyper){

if(missing(Opt_Hyper)) { 
             a=10;b=50;sp1=2;sp2=0.5;r1=2;r2=0.01;
        } else {
           a=Opt_Hyper$a;b=Opt_Hyper$b;
		sp1=Opt_Hyper$sp1; sp2=Opt_Hyper$sp2;
		r1=Opt_Hyper$r1; r2=Opt_Hyper$r2;
        }   


#hyperparameters
## a=10;b=50;sp1=2;sp2=0.5;r1=2;r2=0.01;

theta=rep(1,K);
liks=NULL;m=1;marls=NULL

#dimensions
n=length(Y);
p=dim(X)[2];

#instore posterior sampling
#postalp=NULL;
postz=array(0,c(p,K,T))
postmu=array(0,c(p,K,T))
posttau2=matrix(0,T,p);
postw=NULL;
postsig2=NULL;
postd=matrix(0,T,K);
postpx=NULL;
postH=matrix(0,T,n);
postnk=matrix(0,T,K);
postp=matrix(0,m,n);

#initionation
d=theta/sum(theta);
tau2=rep(rinvgamma(1,sp1,sp2),p);
w=0.5;
sig2=rinvgamma(1,r1,r2);

z=matrix(as.numeric(runif(p*K)>w), p,K);
mu=matrix(rnorm(p*K,0,1),p,K);
H=sample(1:K, n, replace=T, prob=d)

newp=reorder(mu,H,z,d);
mu=newp$mu;
H=newp$H;
z=newp$z;
d=newp$d;

V=matrix(0,p,K); 

################## MCMC ##################
for (iter in 1:(T0+T)){
if (iter/100==round(iter/100,dig=0)) print(iter) # print number of itertations

nk=rep(0,K);id=1:n;
for(i in 1:n) {
nk[H[i]]=nk[H[i]]+1
} # n of k segment

for(k in 1:K){
idk=id[H==k]
for(j in 1:p) 
V[j,k]=sum(X[idk,j]^2)
}

########## sample sig2 #############

res=0
for(i in 1:n)
res=res+(Y[i]-X[i,]%*%mu[,H[i]])^2;

ap=n/2+r1;
be=res/2+r2
sig2=rinvgamma(1,ap,be);


########## sample z and mu, w and tau_p2 #############
#sample z and mu
for (k in 1:K){
idk=id[H==k]
for (j in 1:p){
	# sample Z
	eps = Y[idk]-X[idk,-j]%*%mu[-j,k];
	tmpmean = sum(eps*X[idk,j])/sig2;
	tmpvar = V[j,k]/sig2+1/tau2[j]; 
	logr=log(1-w)-log(w)+ log(V[j,k]*tau2[j]/sig2+1)/2 - tmpmean^2/tmpvar/2
	if (logr>100) logr=100
	z[j,k]=ifelse(runif(1)< 1/(1+exp(logr)),1,0);

	# sample mu
	if (z[j,k]==0) mu[j,k]=0 else {
		bjmu = tmpmean/tmpvar;
		mu[j,k]=rnorm(1,bjmu,1/sqrt(tmpvar))
	}
}#for j
}#for k

aw=sum(z); bw=p*K-aw;
w=rbeta(1,aw+a, bw+b);

##taup2
at=rep(0,p)
bt=rep(0,p)

for (j in 1:p){
aw1=sum(z[j,])
bw1=(sum(mu[j,]^2))

at[j]=aw1/2+sp1
bt[j]=bw1*sum(z[j,])/2+sp2
tau2[j]=rinvgamma(1,at[j],bt[j]);
}


########## sample H and d ##################
d=as.vector(rdirichlet(1, nk+theta));

wp=matrix(0,K,n);
for(i in 1:n)
for(k in 1:K)
wp[k,i]=exp(-(Y[i]-X[i,]%*%mu[,k])^2/(2*sig2)) 

for (k in 1:K)
wp[k,]=wp[k,]*d[k]

for (i in 1:n){
H[i]=sample(1:K,1,replace=TRUE,wp[,i])
}



########## reordering ##################

newp=reorder(mu,H,z,d);
mu=newp$mu;
H=newp$H;
z=newp$z;
d=newp$d;


########## save posterior samples ########

if(iter>T0){
tmpT=iter-T0;
postz[,,tmpT]=z;
posttau2[tmpT,]=tau2
#postp=postp+pli;
postmu[,,tmpT]=mu;
postw=c(postw,w);
postsig2=c(postsig2,sig2)
postd[tmpT,]=d
postH[tmpT,]=H;
postnk[tmpT,]=nk;
}

lik=0
for (i in 1:n){
mut=X[i,]%*%mu[,H[i]];
temp1=log(dnorm(Y[i],mut,sqrt(sig2)))
lik=lik+temp1;
}
liks=c(liks,lik);
} # End of MCMC


## Posterior mean of beta ##

ss=matrix(0,p,K) 
for (i in 1:p)
for (j in 1:K)
{ 
ss[i,j]=mean(postmu[i,j,])
} 



## Posterior mean of Z ##

tt=matrix(0,p,K)
for (i in 1:p)
for (j in 1:K)
{ 
tt[i,j]=mean(postz[i,j,])
}


## calculate Odds Ratio ##

OR=matrix(0,p,K);
for (j in 1:p)
for (k in 1:K)
{
OR[j,k]=(b/a)*(sum(postz[j,k,])/(T-sum(postz[j,k,])))	
}
OR[OR>1000]=1000
# Max of OR is 1000


## membership calucation ##

memb=rep(0,n)
for (i in 1:n)
memb[i]<-as.numeric(names(table(postH[,i]))[which.max(table(postH[,i]))]) 
table(memb)

## McMc error ##
mcse=matrix(0,p,K);
for (j in 1:p)
for (k in 1:K){
mcse[j,k]=imse(postmu[j,k,])
}

## Optional Calculations ##

if(missing(LML))
list("coefficients"=as.matrix(ss),
		"memb"=memb,"Odds Ratio"=as.matrix(OR),"MCMC SE"=as.matrix(mcse))
else{

print("Calulating LML")


#########################
#Log Marginal Likelihood#
#########################


#############################################
# Likelihood of Y|all others (mu, H, sigma2)#
#############################################

sigstar=sum(postsig2)/T

Ylik=0
for(i in 1:n){
meanstar=X[i,]%*%ss[,memb[i]]
temp1=dnorm(Y[i],meanstar,sqrt(sigstar),log=TRUE)
Ylik=Ylik+temp1;
}

Ylik #Y likelihood


###Prior Parts###

## prior of H 
dd=matrix(0,T,n) 
for (t in 1:T)
for (i in 1:n){
dd[t,i]=log(postd[t,memb[i]]) 
}
pdd=apply(dd,1,sum)  
pdd=mean(pdd) 
pdd

## prior of beta
prss=sum(dt(ss, df=sp1, ncp=0, log = TRUE))

## prior of sigma2
prsig=dgamma(sigstar,r1,r2,log=T)

pri=pdd+prss+prsig
pri


###Posterior parts###


# H* #

pH=matrix(0,n,T)

for (t in 1:T)
for (i in 1:n){
eps1=exp(-((Y[i]-X[i,]%*%ss[,memb[i]])^2)/(sigstar*2))*postd[t,memb[i]];
eps2=0
for (l in 1:K){
temp3=exp(-((Y[i]-X[i,]%*%ss[,l])^2)/(sigstar*2))*postd[t,l];
eps2=eps2+temp3
}
pH[i,t]=eps1/eps2
}
pH=log(pH)


pHH=apply(pH,2,sum)
pHH=mean(pHH)
pHH


#sig*#

psig2=rep(0,T)
for (t in 1:T){
res=0
for(i in 1:n){
res=res+(Y[i]-X[i,]%*%postmu[,postH[t,i],t])^2;
}
ap1=n/2+r1;
be1=res/2+r2
psig2[t]=dinvgamma(sigstar,ap1,be1);
}
mean(log(psig2))



#mu*#


pmu=matrix(0,K,T)

for (j in 1:p){
for (t in 1:T){
for (k in 1:K){
idk1=id[postH[t,]==k]
if (sum(postz[j,k,t])==0) pmu[k,t]=0 else
{if (length(idk1)==1)Xidk=X[idk1,postz[j,k,t]==1] else {
Xidk=as.matrix(X[idk1,postz[,k,t]==1])
ss1=ss[(postz[,k,t]==1),k]
#sss=ss[,k]*postz[,k,t]
#ss1=sss[abs(sss)>0]
sp=sigstar/posttau2[t,j]
tempmean=(solve(sp*diag(ncol(Xidk))+t(Xidk)%*%Xidk))%*%(t(Xidk)%*%Y[idk1])
tempvar=solve((diag(ncol(Xidk))/posttau2[t,j])+(t(Xidk)%*%Xidk)/sigstar)
pmu[k,t]=dmvnorm(ss1,tempmean,tempvar)
}}}}}
ppmu=rep(0,T)
for (t in 1:T){
ppmu[t]=sum(log(pmu[,t][abs(pmu[,t])>0]))
}
ppmu=mean(ppmu)
ppmu

pos=pHH+ppmu+mean(log(psig2))

#logmarginal likelihood#

lml=Ylik+pri-pos


list("coefficients"=as.matrix(ss),
		"memb"=memb,"Odds Ratio"=as.matrix(OR),"MCMC SE"=as.matrix(mcse),"LML"=lml)}  



} # for the function of Unconstraind_Bayes_Mixture
