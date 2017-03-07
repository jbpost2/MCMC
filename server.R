library(shiny)

#number of draws sets to create
niter<-10001
#sample size
n<-20

#Create Uniform Prior function
prior = function(theta){
	if((theta<0) || (theta>1)){  # || here means "or"
		return(0)
		}else{
		return(1)}
}

#Binomial distribution for y|theta (ignoring the constant wrt theta
likelihood = function(theta, y,n){
	return(theta^(y) * (1-theta)^(n-y))
}

#Want to sample from p(theta|y)
#create a function that takes in an observed y, sample size n, number of iterations to run
#and starting values for parameter theta and the "jumping distribution's" sd

thetasampler = function(y, n, niter, thetastartval, thetasd){

	#vector to store sampled theta's
	theta<-jump<-r<-rep(0,niter)
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
		if (newtheta<0){ 
			newtheta<-0
		}else if (newtheta>1){
			newtheta<-1
		}

		#Find r ratio: 
		r[i]<- prior(theta=newtheta)*likelihood(theta=newtheta,y=y,n=n)/(prior(theta=currenttheta) * 
		likelihood(theta=currenttheta,y=y,n=n))

		#accept this new value with prob min(1,r), leave value the same with prob 1-r
		if(runiforms[i]<r[i]){
			theta[i] = newtheta       # accept move with probabily min(1,r)
		} else {
			theta[i] = currenttheta        # otherwise "reject" move, and stay where we are
		} #end if's
	} #end loop 
	#return the vector of theta values	
	return(data.frame(theta,jump,runiforms,r))
} #end function





# Define server logic required to draw the plots
shinyServer(function(input, output,session) {
 
    simDraws<-reactive({
        input$y
        input$thetastart
        input$thetaproposalsd
        
	data<-thetasampler(y=input$y,n=n,niter=niter,thetastartval=input$thetastart,thetasd=input$thetaproposalsd)
	data
    })

 
  output$posteriorPlot<-renderPlot({
  	
	y<-input$y
	input$thetastart
	input$thetaproposalsd
	step<-input$step

	draws<-simDraws()
	#first column is theta draws, 2nd is jumps, 3rd is runifs, 4th is r ratio

	#true posterior first
	x=seq(0,1,length=1000)
	plot(x,dbeta(x,shape1=y+1, shape2=n-y+1),type="l",ylab="posterior",xlab="theta",main="Plot of True Posterior")

	#histogram of data prior to this one
	if (step>1){
		h<-hist(draws[1:(step-1),1],plot=FALSE,breaks=seq(from=0,to=1,by=0.05))
        #rescale max to have max of the posterior
        max<-max(h$counts)
        h$counts<-h$counts*max(dbeta(x,shape1=y+1, shape2=n-y+1))/max
        plot(h,add=TRUE,col=rgb(0.3,0.1,0.3,0.1))
	}

	#add in value for step "step"
	abline(v=draws[step,1],col="green",lwd=3)
	mtext(side=1,at=draws[step,1],text=paste("Theta",step,sep=""))

	#overlay jumping distribution
	lines(x,max(dbeta(x,shape1=y+1, shape2=n-y+1))*dnorm(x,mean=draws[step,1],sd=input$thetaproposalsd)/max(dnorm(x,mean=draws[step,1],sd=input$thetaproposalsd)))
    
	#add in jumped value
	abline(v=draws[step,1]+draws[step+1,2],col="Blue",lwd=3)
	mtext(side=3,at=draws[step,1]+draws[step+1,2],text="Theta*")

	legend(x="topleft",legend=c("True Posterior","Jumping Distribution","Previous Theta","Candidate Theta"),col=c("Black","Purple","Green","Blue"),pch=15)
    })

  output$drawTable<-renderTable({
	step<-input$step
	input$y
	input$thetastart
	input$thetaproposalsd

	draws<-simDraws()
	
    if(step<5){
        #Get draws
        out<-data.frame(draws[1:step,1],draws[2:(step+1),2],draws[1:step,1]+draws[2:(step+1),2],draws[2:(step+1),4],draws[2:(step+1),3],c(draws[2:(step+1),4]>draws[2:(step+1),3]))
        colnames(out)<-c("Previous Theta","Jump","New Theta","r Ratio","Random Uniform","Accept Theta?")
        out
    } else {
        out<-data.frame(draws[(step-4):step,1],draws[(step-3):(step+1),2],draws[(step-4):step,1]+draws[(step-3):(step+1),2],draws[(step-3):(step+1),4],draws[(step-3):(step+1),3],c(draws[(step-3):(step+1),4]>draws[(step-3):(step+1),3]))
        colnames(out)<-c("Previous Theta","Jump","New Theta","r Ratio","Random Uniform","Accept Theta?")
        out
    }
  })    

})