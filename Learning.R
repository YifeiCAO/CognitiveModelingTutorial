# Chapter 2, random-walk model
nreps <- 10000 # number of random walks
nsamples <- 2000 # number of times that evidence is being sampled for each decision

drift <- 0.03 # drift rate, the amount of evidence that is available during sampling
sdrw <- 0.3 # standard deviation of the distribution from which we will sample the evidence
criterion <- 3 # distance of the two boundaries

latencies <- rep(0,nreps) 
responses <- rep(0,nreps)
evidence <- matrix(0,nreps,nsamples+1)

for (i in c(1:nreps)) {
  evidence[i,] <- 
    cumsum(c(0,rnorm(nsamples,drift,sdrw))) # use of rnorm, drawing nsamples observations from a normal distribution with mean drift and sd sdrw
  p <- which(abs(evidence[i,])>criterion)[1] # when randomwalk crossed a response boundary, assign to p
  responses[i] <- sign(evidence[i,p]) # evaluate the sign of the evidence where the response was associated with top or bottom boundary
  latencies[i] <- p # p will be recorded as the ith observation in latencies
}

#plot up to 5 random-walk paths
tbpn <- min(nreps,5)
plot(1:max(latencies[1:tbpn]) + 10, type = "n",las = 1,
     ylim = c(-criterion - 0.5, criterion + 0.5),
     ylab = "Evidence", xlab = "Decision time")
for (i in c(1:tbpn)) {
  lines(evidence[i,1:(latencies[i] - 1)])
}
abline(h = c(criterion,-criterion),lty = "dashed")

#plot histograms of latencies
par(mfrow=c(2,1))
toprt <- latencies[responses>0]
topprop <- length(toprt)/nreps
hist(toprt,col='gray',
     xlab='Decision time', xlim=c(0,max(latencies)),
     main=paste('Top responses (',as.numeric(topprop),
                ') m=',as.character(signif(mean(toprt),4)),
                sep=''),las=1)
botrt <- latencies[responses<0]
botprop <- length(toprt)/nreps
hist(botrt,col='gray',
     xlab='Decision time', xlim=c(0,max(latencies)),
     main=paste('Top responses (',as.numeric(botprop),
                ') m=',as.character(signif(mean(botrt),4)),
                sep=''),las=1)

