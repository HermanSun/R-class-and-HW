#######################
#Function Reordering###
#######################
reorder<-function(mu,H,z,d){
K=ncol(mu)
p=nrow(mu)
n=length(H)
stop=FALSE;l=1:K
#id=1:K;
for (i in 1:p){
if (stop) break;
lc=l[order(mu[i,l])]
mutemp=mu;Htemp=H;dtemp=d;ztemp=z;
for (t in 1:length(lc)){
mu[,l[t]]=mutemp[,lc[t]];
z[,l[t]]=ztemp[,lc[t]];
H[Htemp==lc[t]]=l[t];
d[l[t]]=dtemp[lc[t]];
}
l=sort(l[mu[i,l]==0])
if (length(l)<=1) stop=TRUE 
}
list("mu"=as.matrix(mu),"H"=H,"z"=as.matrix(z),"d"=d)
}#reorder
