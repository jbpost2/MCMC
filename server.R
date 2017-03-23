library(shiny)

#######################################################
##First set up the MCMC sampler

#number of draws sets to create
niter<-10001

#Create Uniform Prior function
prior = function(theta){
	if((theta<0) || (theta>1)){  
		return(0)
	}else{
		return(1)}
}

#Binomial distribution for y|theta (ignoring the constant wrt theta)
likelihood = function(theta, y,n){
	return(theta^(y) * (1-theta)^(n-y))
}

#Want to sample from p(theta|y)
#create a function that takes in an observed y, sample size n, number of iterations to run, a starting value for theta, and the "jumping distribution's" sd
thetasampler <- function(y, n, niter, thetastartval, thetasd){

	#vector to store info
	theta<-jump<-r<-rep(0,niter)
	#generate random uniforms for acceptance decision
	runiforms<-runif(niter)
	#use the starting value as the first theta
	theta[1] = thetastartval

	#loop through the desired number of iterations
	for(i in 2:niter){
		#temporary value for current theta is previous theta
		currenttheta = theta[i-1]

		#get the next random draw add in a random value from the jumping distr. to our theta value
		jump[i]<-rnorm(1,0,thetasd)
		newtheta = currenttheta + jump[i]
		#make sure it is in the appropriate range, else set it to the edges
		if (newtheta<0){ 
			newtheta<-0
		}else if (newtheta>1){
			newtheta<-1
		}

		#Find r ratio: 
		r[i]<- prior(theta=newtheta)*likelihood(theta=newtheta,y=y,n=n)/(prior(theta=currenttheta)*likelihood(theta=currenttheta,y=y,n=n))

		#accept this new value with prob min(1,r), leave value the same with prob 1-r
		if(runiforms[i]<r[i]){
			theta[i] = newtheta       
		} else {
			theta[i] = currenttheta 
		}
	} 
	
	#Set values for first theta and Keep a variable to determine if new value was kept
	jump[1]<-NA
	runiforms[1]<-NA
	r[1]<-NA
	accept<- r>runiforms
	accept[1]<-TRUE

	#return the vector of theta values	
	return(data.frame(theta,jump,runiforms,r,accept))
}



##########################################################3
##Shiny server part

# Define server logic required to draw the plots
shinyServer(function(input, output,session) {

  #update the max value for the number of successes when using arrows to change
  observe({
    val <- input$n
    valy <- input$y
    updateNumericInput(session, "y", min = 0, max = val, step = 1)
  })
  
  #generate chain if settings change
  simDraws<-reactive({
    input$y
    input$thetastart
    input$thetaproposalsd
    input$n
    
    #call sampler    
	  data<-thetasampler(y=input$y,n=input$n,niter=niter,thetastartval=input$thetastart,thetasd=input$thetaproposalsd)
	  data
  })

  #create posterior plot with all the overlays
  output$posteriorPlot<-renderPlot({

    #get info   	
  	y<-input$y
	  n<-input$n
	  input$thetastart
	  input$thetaproposalsd
	  step<-input$step

	  #grab data
	  draws<-simDraws()

  	#true posterior first
	  x=seq(0,1,length=1000)
	  plot(x,dbeta(x,shape1=y+1, shape2=n-y+1),type="l",ylab="posterior",xlab="theta",main="Plot of True Posterior",lwd=3)

	  #histogram of data up to this step
	  h<-hist(draws$theta[1:step],plot=FALSE,breaks=seq(from=0,to=1,by=0.05))
  	
    #rescale max to have max of the posterior
    max<-max(h$counts)
    h$counts<-h$counts*max(dbeta(x,shape1=y+1, shape2=n-y+1))/max
    plot(h,add=TRUE,col=rgb(0.3,0.1,0.3,0.1))

  	#add in value for step "step"
    if(step==1){
      #add in intial value
      abline(v=draws$theta[step],col="Blue",lwd=3)
      mtext(side=3,at=draws$theta[step],text="Theta1")
    } else {
      #add previous theta
  	  abline(v=draws$theta[step-1],col="green",lwd=3)
  	  mtext(side=1,at=draws$theta[step-1],text=paste("Theta",step-1,sep=""))

  	  #overlay jumping distribution
  	  lines(x,max(dbeta(x,shape1=y+1, shape2=n-y+1))*dnorm(x,mean=draws$theta[step-1],sd=input$thetaproposalsd)/max(dnorm(x,mean=draws$theta[step-1],sd=input$thetaproposalsd)),col="Purple",lwd=3)
    
  	  #add in jumped value
  	  abline(v=draws$theta[step-1]+draws$jump[step],col="Blue",lwd=3)
  	  mtext(side=3,at=draws$theta[step-1]+draws$jump[step],text="Theta*")
    }
        
	  legend(x="topleft",legend=c("True Posterior","Jumping Distribution","Previous Theta","Candidate Theta"),col=c("Black","Purple","Green","Blue"),pch=15,lwd=3)
  })

  output$drawTable<-renderTable({

    #obtain inputs
    step<-input$step
	  input$y
	  start<-input$thetastart
	  input$thetaproposalsd
    input$n
  
    #grab data
	  draws<-simDraws()
	
    if(step==1){
      #Get draws
      out<-data.frame(Iter=step,Prev=NA,Jump=draws$jump[1:step],New=start,r=draws$r[1:step],runif=draws$runiforms[1:step],accept=draws$accept[1:step])
      colnames(out)<-c("Iteration","Previous Theta","Jump","New Theta","r Ratio","Random Uniform","Accept Theta?")
      out
    } else {
      out<-data.frame(Iter=1:step,Prev=c(NA,draws$theta[1:(step-1)]),Jump=draws$jump[1:step],New=c(start,draws$theta[1:(step-1)]+draws$jump[2:step]),r=draws$r[1:step],runif=draws$runiforms[1:step],accept=draws$accept[1:step])
      colnames(out)<-c("Iteration","Previous Theta","Jump","New Theta","r Ratio","Random Uniform","Accept Theta?")
      tail(out,n=5)
    }
  })    

})
