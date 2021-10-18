library("queueing")

# Setting working directory
setwd("C:/Users/idpdl/Downloads/CUC/2021-I/Jackson Networks")

#Reading Tables
N1 <- read.csv('DemandaAltaNodo1.csv', header = T) #"Demanda alta" is spanish for high demand, "Nodo" is Spanish for Node 
N2 <- read.csv('DemandaAltaNodo2.csv', header = T)

#N1 <- read.csv('DemandaBajaNodo1.csv', header = T) #"Demanda Baja" is spanish for low demand, "Nodo" is Spanish for Node 
#N2 <- read.csv('DemandaBajaNodo2.csv', header = T)

k <- 250 #Max No. of Customers


#Pre-locating the Outcomes to be calculated
Outcomes <- as.data.frame(matrix(c(rep(0,dim(N1)[1]*k*13)),dim(N1)[1]*k,13))
colnames(Outcomes) <- c("t","i","Throughput","L","W", "Lq1", "Lq2", 
                        "RO1", "RO2", "Throughput1", "Throughput2","w1", "w2")
counter <- 0

for (t in 1:dim(N1)[1]) { #t is the time in hours
  for (i in 1:k) {  #i is the No. of customers
    counter <- counter+1
    lambda <- N1$Tasa_media_llegadas[t] #"Tasa media de llegadas" is Spanish for mean arrival rate
    mu <- N1$Tasa_media_.servicio[t] #"Tasa media de servicio" is Spanish for mean service rate
    lambda2 <- N2$Lambda[t]
    mu2 <- N2$Muh[t]
    n <- round(N2$L[t], digits = 0)
    
    #Creating Transition Probability Matrix
    prob <- matrix(rep(0,((i+N2$S[t])*(i+N2$S[t]))), nrow=(i+N2$S[t]),
                   ncol=(i+N2$S[t]), byrow=TRUE)
     temp <- unlist(lapply(1:i, function(b){
              paste0("c", toString(b))
      }))
    temp2 <- unlist(lapply(1:N2$S[t], function(b){
      paste0("s", toString(b))
    }))
    colnames(prob) <- append(temp, temp2)
    rownames(prob) <- append(temp, temp2)
    prob[grepl("c", rownames(prob)), grepl("s", colnames(prob))] <- 1/(N2$S[t])
    prob[grepl("s", rownames(prob)), grepl("c", colnames(prob))] <- 1/i
    
    #Reading No. of servers
    S <- N2$S[t]
    
    #Creating List to be fed to the CJN model
    List_t <- {}
    for (j in 1:(i+S)) {
      #For node 1: MMInf ; For node 2: MM1
      if(j<=i){
        n_temp <- NewInput.MMInf(lambda2, mu2, i)
      }
      
      if(j>i){
        n_temp <- NewInput.MM1(lambda2, mu2, -1)  
      }
      
      assign(paste("n.", as.character(j), sep=""), n_temp)
      if(j< (S+i)){
        List_t <- paste(List_t, paste0("n.", as.character(j), ",", sep=""))
      }
      if(j== (S+i)){
        List_t <- paste(List_t, paste0("n.", as.character(j), sep=""))
      }
    }
    List_t <- paste0("(", List_t, ")")
    
    eval(parse(text = paste("List_t2 <-", paste0("list", List_t))))
    
    
    i_CJN <- NewInput2.CJN(prob, n=i, z=0, operational=FALSE, 
                           method=0, tol=0.001, List_t2)
    # i_CJN <- NewInput.CJN(prob, i, z=0, operational=FALSE, 
    #                        method=0, tol=0.001, n1, n2,)
    
    #Building the model
    o_CJN <- QueueingModel(i_CJN)
    #Retrieving Outcomes
    Outcomes[counter,] <- c(t,i,o_CJN$Throughput,o_CJN$L,o_CJN$W, sum(o_CJN$Lk[1:i]), 
                            sum(o_CJN$Lk[(i+1):(S+i)]), sum(o_CJN$ROk[1:i]), o_CJN$ROk[i+1],
                            sum(o_CJN$Throughputk[1:i]), sum(o_CJN$Throughputk[(i+1):(S+i)]),
                            o_CJN$Wk[1], o_CJN$Wk[i+1])
  }
}
##Determining optimum capacity (period-wise) and plotting throughput results
#Pre-locating the Optimum Capacity Vector
Optimum_Capacity <- as.data.frame(c(rep(0,dim(N1)[1])),)
par(mfrow=c(3,4))
for (t in 1:dim(N1)[1]) {
  temp <- Outcomes[(k*(t-1)+1):(k*t),]
  #Here, we use a cutoff of 99.33% of the maximum Throughput to define the optimum*
  x <- min(temp$i[temp$Throughput>=(1-exp(-5))*max(temp$Throughput)]) 
  Optimum_Capacity[t,1] <- x
  plot(temp$i, temp$Throughput, main=paste("t =",toString(t),"h"),
       xlab="Capacity (Customers)", ylab="Throughput")
  abline(v=x, col="green") #Green vertical line for optimum capacity
  abline(v=177, col="red") #Red vertical line for epidemiology-based capacity threshold
}
colnames(Optimum_Capacity) <- "Opt_Cap"
Optimum_Capacity$time <- 1:dim(N1)[1]
Optimum_Capacity <- as.data.frame(Optimum_Capacity)
## Plotting period-wise optimum capacity + epidemiology-based capacity threshold
library(ggplot2)
ggplot(Optimum_Capacity, aes(time, Opt_Cap)) +
  xlab("time (h)")+
  ylab("Optimum Capacity (Customers)")+
  geom_point()+
  geom_line()+
  #Adding horizontal dashed red line for the epidemiology-based capacity threshold
  geom_hline(yintercept=177, linetype="dashed", color = "red")

write.csv(Outcomes, file = "OutcomesHD_Paper3.csv")