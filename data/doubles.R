require("hyperdirichlet" , quietly=TRUE, save=FALSE)

doubles_noghost <- uniform(4)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,c(1,2),c(3,4),9,2)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,c(1,3),c(2,4),4,4)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,c(1,4),c(2,3),6,7)

doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,1,3,10,14)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,2,3,12,14)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,1,4,10,14)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,2,4,11,10)
doubles_noghost <- doubles_noghost + mult_bernoulli_obs(4,3,4,13,13)


doubles <- uniform(5)
doubles <- doubles + mult_bernoulli_obs(5,c(1,2,5),c(3,4),9,2) # ghost!
doubles <- doubles + mult_bernoulli_obs(5,c(1,3)  ,c(2,4),4,4)
doubles <- doubles + mult_bernoulli_obs(5,c(1,4)  ,c(2,3),6,7)

doubles <- doubles + mult_bernoulli_obs(5,1,3,10,14)
doubles <- doubles + mult_bernoulli_obs(5,2,3,12,14)
doubles <- doubles + mult_bernoulli_obs(5,1,4,10,14)
doubles <- doubles + mult_bernoulli_obs(5,2,4,11,10)
doubles <- doubles + mult_bernoulli_obs(5,3,4,13,13)
pnames(doubles) <- paste("p" , c(1:4, "G"),sep="")

