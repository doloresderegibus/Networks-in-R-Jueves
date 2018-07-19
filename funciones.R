#tarea de lo que tenemos que generar
require(igraph)

source("R/functions.r")

g <- readEcoNetwork("Data/WeddellSea_FW.csv")
plotEcoNetworkTrophLevel(g)
tien <- topologicalIndicesEcoNetwork(g)

g <- readEcoNetwork("Data/BarentsBoreal_FW.csv")
plotEcoNetworkTrophLevel(g)
tien <- rbind(tien,topologicalIndicesEcoNetwork(g))



# tres funciones
#1. to read networks in any of the formats(matriz de adyacencia (que es una matriz cuadrada) o lista de interacciones) and return an igraph object

#2. Plot networks with trophic levels

#3. Calculate all the networks indices given an igraph object and return a data.frame

#es generico porque es la funcion

#1

require(igraph)

readEcoNetwork<-function(filename){
  
  #leer en formato csv un data.frame
  web<-read.csv(filename)
  if((ncol(web)-1) == nrow(web)){
    g<-graph_from_adjacency_matrix(web)   #leer como matriz de adyacencia (que es la matriz cuadrada)
  }else{
    web<-web[,c(2,1)]
    g<-graph.data.frame(web)    #entonces hay que leerlo como una lista de interacciones (son dos columnas), y para saber eso hay que mirar el dataframe 
    
  }
  
  return(g) #es lo ultimo que se pone para que la funcion funcione
}
#2


plotEcoNetworkTrophLevel<-function(g){
  plot(g)
  lMat <-matrix(
    nrow=vcount(g),  # Rows equal to the number of vertices (species)
    ncol=2
  )
  
  lMat[,2]<-jitter(tl$TL,0.1)              # y-axis value based on trophic level
  lMat[,1]<-runif(vcount(g))               # randomly assign along x-axis
  
  plot(g, edge.width=.3,edge.arrow.size=.4,
       vertex.label=NA,
       edge.color="grey50",
       edge.curved=0.3, layout=lMat)
  
  
  # Add colors to the plot
  #
  require(RColorBrewer)
  
  colTL <-as.numeric(cut(tl$TL,11))   # Divide trophic levels in 11 segments
  colnet <- brewer.pal(11,"RdYlGn")   # Assign colors to trophic levels
  V(g)$color <- colnet[12-colTL]      # Add colors to the igraph object
  
  
  plot(g, edge.width=.3,edge.arrow.size=.4,
       vertex.label=NA,
       edge.color="black",
       edge.curved=0.3, layout=lMat)
  #3
 
  topologicalIndicesEcoNetwork<-function(tiel){
    
    deg <- degree(g, mode="out") # calculate the in-degree: the number of predators  
    
    nTop <- length(V(g)[outdegree==0]) # Top predators do not have predators
    
    deg <- degree(g, mode="in") # calculate the in-degree: the number of preys
    
    V(g)$indegree <-  deg
    
    nBasal <- length(V(g)[indegree==0]) # Basal species do not have preys 
    
    vcount(g)-nTop-nBasal # Intermediate species
    
    size <- vcount(g)
    
    links <- ecount(g)
    
    linkDen <- links/size          # Linkage density
    
    conn <- links/size^2           # Connectance
    
    pathLength <- average.path.length(g)   # Average path length 
    
    clusCoef <- transitivity(g, type = "global")   # Clustering coefficient  
    
    data.frame(size, links,conn)
    return(data.frame)
     }
  
}
  
  
  
  
  
  
  
  
  