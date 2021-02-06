
library(extRemes)
library(igraph)
library(ggplot2)

CollectiveBrain <- function(N, Ticks, Reps, Start, edge.prob, alpha, beta) {
  
  output <- as.data.frame(matrix(NA,Ticks,Reps))  # create a matrix with Ticks rows and Reps columns, filled with NAs, then convert to data.frame
  
  names(output) <- paste("repetition", 1:Reps, sep="")  # purely cosmetic: rename the columns with run1, run2 etc.
  
  
  for (r in 1:Reps) {
  ################Set up population starting culture:  
  
  mode = Start - alpha #new location of the Gumbel mode due to information loss 
  
  x = revd(N,loc=mode,scale=beta,shape=0) #Starting cultural skills of tick = 1 population. loc is the mode of the Gumbel distribution, scale is the dispersion and shape=0 specifies that the generalised extereme value distribution that we desire is the Gumbel.
  
  agents <- as.data.frame(x, row.names = NULL, optional = FALSE) #Produce population of N agents, each with a z value sampled from the Gumbel distribution x.
  
  ids <- 1:N  #Give agents ids so that they can be given social network addresses.
  
  agents <- cbind(ids, agents) #Add ids column to agents
  
  names = c("ID", "skill_z")  #Name the columns in the agents data frame
  names(agents) = names
  
  
  
  output[1,r] <- mean(agents$skill_z) #compute zbar value and send it to the output dataframe
  
  
  
  
  ################Generating random (Erdos-Renyi method) social network for agents:
  
  g <- make_empty_graph(n = N) # Generates empty graph
  
  #Now a nice forloop to generate probabilistic edges between agents:
  
  for (i in 1:N) { #For all agents
    
    for (j in 1:N) {  #and for all possible friends of agents
      
      individual.prob <- runif(n = 1, min = 0, max = 1)  #Sample uniform distribution
      
      if (individual.prob <= edge.prob) {  #Form friendship if sampled value is equal to or less than edge probability
        
        g <- add.edges(g, c(i, j))  #From edge between ith and jth agents
      }
    }
  }
  
  m <- as_adjacency_matrix(g) #converts to adjacency matrix for transmission modelling (if oblique learning rows by columns are the two discrete generations)
  
  # diag(m) <- 0 #If you wish to use horizontal network transmission instead of oblique, uncomment this line of code that prevents reflexive relationships (transmission within a generation does not involve one's self)
  
  
  for (t in 2:Ticks) {
    
    previous_agents <- agents #Pass current agents into previous generation status
    
    for (i in 1:N) {
      
      agents[i, ] #Take the ith agent
      
      neighbours.i <- which(m[i,] == 1) # find all the neighbours of ith individual (in either current or previous generation)
      
      if (length(neighbours.i) > 0) { #As long as the ith agent actually has any ties, do the following:
        
        neighbouring.agents <- previous_agents[neighbours.i, ] #Use social network neighbour ids as an index for agents in the data frame (either previous or current generation)
        
        #Reorder neighbouring agents according to skill to impliment skill-biased social learning
        neighbouring.agents <- neighbouring.agents[order(-neighbouring.agents$skill_z), ]
        
        z_h <- neighbouring.agents$skill_z[1] #Take the highest skill value of all neighbours
        
        location = z_h - alpha #specifies that mode of the gumbel distribution should be the best model - alpha
        
        agents$skill_z[i] <- revd(1, loc = location, scale = beta, shape = 0) #We use the extRemes package to produce the Gumbel distribution to sample ith agent's new trait value.
        
      }
      
    }
    
    output[t,r] <- mean(agents$skill_z)
    
  } #end of ticks for-loop
  
  
  } #End of repetitions for-loop
  

  skill <- rowMeans(output)
  
  output <- as.data.frame(skill)
  
  time <- 1:Ticks
  
  output <- cbind(time, output)
  
  output <- as.data.frame(output)
  
  ggplot(output,  aes(x=time, y=skill)) + 
    geom_line() +
    xlab("time") +
    ylab("average skill-level") +
    ggtitle(paste("Cumulative Culture in Social Networks: N = ", N, ", alpha = ", alpha, ", beta = ", beta, ", Sociality = ", edge.prob, sep = "")) +
    theme_bw()
 
  
} 


CollectiveBrain(N = 100, Ticks = 100, Reps = 5, Start = 0, edge.prob = 1, alpha = 5, beta = 1)
CollectiveBrain(N = 100, Ticks = 100, Reps = 5, Start = 0, edge.prob = 0.8, alpha = 5, beta = 1)
CollectiveBrain(N = 100, Ticks = 100, Reps = 5, Start = 0, edge.prob = 0.5, alpha = 5, beta = 1)



