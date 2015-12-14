library('rgl')
library('Rcpp')
library('plyr')

degreeDistribution <- function(n){
  counts = edgeCounter()
  x11()
  barplot(counts[,2], names = counts[,1], main="Bar Graph Edge Distribution", xlab="Edge Count", ylab="Vertices")
  #x11()
  #plot(counts[,2], main="Line Graph Edge Distribution", xlab="Edge Count", ylab="Vertices", type="o")
  print(paste("ACTUAL DEGREE: ",sum(counts[,2]*counts[,1])/n))
}

generateGraph <- function(x, y, z, n, radius, neighbors){
  print("PLOTTING GRAPH")
  plot3d(x,y,z, col="red", size=5)
  set_points(cbind(x,y,z))
  
  print("GENERATING CONNECTIONS")
  m = distance3D(radius, neighbors)
  pairs_x = m[,1]
  pairs_y = m[,2]
  pairs_z = m[,3]
  segments3d(pairs_x, pairs_y, pairs_z, col=rgb(0, 0, 0, 0.3), alpha=.3)
  
  print("GRAPHING MAX NODE")
  open3d()
  plot3d(x,y,z, col="red", size=5)
  max = maxEdges()
  max_x = max[,1]
  max_y = max[,2]
  max_z = max[,3]
  segments3d(max_x, max_y, max_z, col=rgb(0, 0, 0, 0.3))
  
  
  print("CREATING EDGE DISTRIBUTION")
  degreeDistribution(n)
  
  print("CALCULATING SMALLEST LAST ORDERING")
  stats = smallestLastOrdering3()
  print("GRAPHING SMALLEST LAST ORDERING")
  x11()
  plot(stats[,2], col="red", type="l", ylim=range(c(stats[,1],stats[,2])))
  lines(stats[,1], col="blue", type="l")
  
  print("GENERATING COLORS")
  coloring = colorGraph()
  max = max(coloring[,4])
  colors = rainbow(max)
  print("PLOTTING COLORS")
  open3d()
  plot3d(coloring[,1],coloring[,2],coloring[,3], col=colors[coloring[,4]], size=5)
  segments3d(pairs_x, pairs_y, pairs_z, col=rgb(0, 0, 0, 0.1), alpha=.3)
  print(paste("COLOR COUNT: ", max))
  color_table = table(coloring[,4])
  x11()
  barplot(color_table, col=colors[1:max])
  
  print("FINDING MAX BIPARTIDE SUBGRAPH")
  calculateSubgraphs()
  edges_one = maximumSubgraph1()
  edges_two = maximumSubgraph2()
  edges_three = maximumSubgraph3()
  
  print("GRAPHING MAX BIPARTIDE SUBGRAPHs")
  open3d();
  plot3d(edges_one[,1],edges_one[,2],edges_one[,3], col=colors[edges_one[,4]], size=5)
  segments3d(edges_one[,1], edges_one[,2], edges_one[,3], col=rgb(0, 0, 0, 0.1))
  open3d()
  plot3d(edges_two[,1],edges_two[,2],edges_two[,3], col=colors[edges_two[,4]], size=5)
  segments3d(edges_two[,1], edges_two[,2], edges_two[,3], col=rgb(0, 0, 0, 0.1))
  open3d()
  plot3d(edges_three[,1],edges_three[,2],edges_three[,3], col=colors[edges_three[,4]], size=5)
  segments3d(edges_three[,1], edges_three[,2], edges_three[,3], col=rgb(0, 0, 0, 0.1))
}

generateRGG <- function(n, degree, type){
  #sourcing our CSV files
  sourceCpp("~/Documents/AEProject/Wireless-Sensor-Network-Simulation/3dHandler.cpp")
  if(type == "square"){
    radius = sqrt((degree+1) / (n*pi))
    xs <- runif(n, 0, 1)
    ys <- runif(n, 0, 1)
    zs <- numeric(n)
  }else if(type == "disk"){
    radius = sqrt((degree+1)/n)
    xs = vector('numeric')
    ys = vector('numeric')
    while(length(xs) < n){
      x_ <- runif(1, -1, 1)
      y_ <- runif(1, -1, 1)
      if((x_^2 + y_^2) < 1){
        xs = append(xs, x_)
        ys = append(ys, y_)
      }  
    }
    zs = numeric(n)
  }else if(type == "sphere"){
    radius = sqrt(((4*degree) + 4)/n)
    zs <- runif(n, -1, 1)
    theta <- runif(n, 0, 2*pi)
    xs = sqrt(1 - zs^2)*cos(theta)
    ys = sqrt(1 - zs^2)*sin(theta)
  }else{
    print("GRAPH TYPE NOT FOUND")
  }
  
  print(paste("NODES: ", n))
  print(paste("RADIUS: ", radius))
  print(paste("TARGET DEGREE: ", degree))
  generateGraph(xs, ys, zs, n, radius, degree)
}


generateRGG(100, 100, "square")
