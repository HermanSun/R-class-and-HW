
logit_nnet <- function(X, Y, step_size = 0.5, reg = 0, niteration){  

# step_size for gradient descent

# get dim of input 
N <- nrow(X) # number of examples  
K <- ncol(Y) # number of classes  
D <- ncol(X) # dimensionality   

# initialize parameters randomly 
W <- 0.01 * matrix(rnorm(K), nrow = D)  
#b <- matrix(0, nrow = 1, ncol = h)  
#W2 <- 0.01 * matrix(rnorm(h*K), nrow = h)  
#b2 <- matrix(0, nrow = 1, ncol = K)   

# gradient descent loop to update weight and bias  
for (i in 0:niteration){    

# class score    
scores <- X%*%W     

# compute and normalize class probabilities   
probs=matrix(0,N,2) 
exp_scores <- exp(-(scores))    
probs[,1] <- 1/ (1+exp_scores) 
probs[,2] <- exp_scores / (1+exp_scores)  

# compute the loss: sofmax and regularization    
corect_logprobs <- -log(probs)    
data_loss <- mean(corect_logprobs*Y)  #cost function (Cross Entropy Cost Function)   
reg_loss <- reg*sum(W*W)  #regularization term for parameters   
loss <- data_loss + reg_loss    
# check progress    
if (i%%1000 == 0 | i == niteration){      
print(paste("iteration", i,': loss', loss))} #print only every 1000 iterations    

# compute the gradient on scores (final delta)    
dscores <- probs[,1]-Y[,1]    
dscores <- dscores/N   
 
# finally into W,b    

dW <- t(X)%*%dscores    

# add regularization gradient contribution    
dW <- dW + reg *W     

# update parameter     
W <- W-step_size*dW    
}

#return(list(W, b, W2, b2))
return(list(W))
} 
#### end of nnet ########
