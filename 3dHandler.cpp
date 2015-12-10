#include <Rcpp.h>
#include <map>
#include <limits>
using namespace Rcpp;

NumericMatrix points;
NumericMatrix pairs;
int n;
int x = 0;
int y = 1;
int z = 2;
// [[Rcpp::export]]
void set_points(NumericMatrix p){
  points = p;
  n = p.nrow();
}

// [[Rcpp::export]]
NumericMatrix smallestLastOrdering() {
  NumericMatrix final(points.nrow(),2);
  
  NumericVector pnts(points.nrow());
  for(int i = 0; i < points.nrow(); i++){
    pnts[i] = points(i,x);
  }
  
  NumericVector prs(pairs.nrow());
  for(int i = 0; i < pairs.nrow(); i++){
    prs[i] = pairs(i,x);
  }
  
  std::map<double, int> counts;
  std::map<double, int> org_counts;
  for(int i = 0; i < pnts.size(); i++){ 
    counts[pnts[i]] = 0;
    org_counts[pnts[i]] = 0;
  }
  
  //counts all edges left in pairs
  for(int i = 0; i < prs.size(); i++){
    if(org_counts.count(prs[i]) != 0){
      org_counts[prs[i]]++;
    }else{
      org_counts[prs[i]] = 1;
    }
  }
  
  int next = pnts.size()-1;
  std::map<double, int>::iterator it;
  while (next >= 0){
    //counts all edges left in pairs
    for(int i = 0; i < prs.size(); i++){
      if(prs[i] != 0){
        if(counts.count(prs[i]) != 0){
          counts[prs[i]]++;
        }else{
          counts[prs[i]] = 1;
        }
      }
    }
    
    //find min key
    int min = std::numeric_limits<int>::max();
    double minKey = 0.0;
    for (it = counts.begin(); it != counts.end(); ++it  )
    {
      if(it->second < min){
        min = it->second;
        minKey = it->first;
      }
    }
    
    //add to output
    final(next, 0) = min;
    final(next, 1) = org_counts[minKey];
    next--;
    
    //remove from map and pairs
    counts.erase(minKey);
    for(int i = 0; i < prs.size(); i++){
      if(prs[i] == minKey){
        prs[i] = 0;
        if(i%2 == 0){
          prs[i+1] = 0;
        }else{
          prs[i-1] = 0;
        }
      }
    }
    
    for (it = counts.begin(); it != counts.end(); ++it  )
    {
      it->second = 0;
    }
  }
  return final;
}

// [[Rcpp::export]]
NumericMatrix maxEdges() {
  std::map<double, int> counts;
  for(int i = 0; i < pairs.nrow(); i++){
    if(counts.count(pairs(i,0)) != 0){
      counts[pairs(i,0)]++;
    }else{
      counts[pairs(i,0)] = 1;
    }
  }
  
  int max = 0;
  double maxKey = 0;
  std::map<double, int>::iterator it;
  for (it = counts.begin(); it != counts.end(); ++it  )
  {
    if(it->second > max){
      max = it->second;
      maxKey = it->first;
    }
  }
  
  std::cout << "[1] \"MAX EDGE #: " << max << "\"" << std::endl;
  
  int finalSize = max*2;
  NumericMatrix final(finalSize,3);
  int num = 0;
  for(int i = 0; i < pairs.nrow(); i++){
    if(double(pairs(i,0)) == maxKey){
      final(num,0) = pairs(i,0);
      final(num,1) = pairs(i,1);
      final(num,2) = pairs(i,2);
      num++;
      if(i%2 == 0){ 
        final(num,0) = pairs(i+1,0);
        final(num,1) = pairs(i+1,1);
        final(num,2) = pairs(i+1,2);
      }else{
        final(num,0) = pairs(i-1,0);
        final(num,1) = pairs(i-1,1);
        final(num,2) = pairs(i-1,2);
      }
      num++;
    }
  }
  
  return final; 
}


// [[Rcpp::export]]
NumericMatrix distance3D(double radius) {
  int size = n*n;
  if (size < 0){
    size = n/2*n/2;
  }
  NumericMatrix xx(size, 3);
  int num = 0;
  
   for (int i = 0; i < n; i++){
    for (int j = i+1; j < n; j++){
      if (sqrt(pow((points(i,x) - points(j,x)), 2) + pow((points(i,y) - points(j,y)), 2) + pow((points(i,z) - points(j,z)), 2)) <= radius){
        if(num < size){
            xx(num, 0) = points(i,x);
            xx(num, 1) = points(i,y);
            xx(num, 2) = points(i,z);
            num++;
            xx(num, 0) = points(j,x);
            xx(num, 1) = points(j,y);
            xx(num, 2) = points(j,z);
            num++;
        }
            
      }
    }
   }
     
  NumericMatrix final(num, 3);
  for (int i = 0; i < num; i++){
    final(i, 0) = xx(i, 0);
    final(i, 1) = xx(i, 1);
    final(i, 2) = xx(i, 2);
  }

  pairs = final;
  return final;
}


// [[Rcpp::export]]
NumericMatrix edgeCounter() {
  std::map<double, int> posCounts;
  for(int i = 0; i < pairs.nrow(); i++){
    if(posCounts.count(pairs(i,x)) != 0){
      posCounts[pairs(i,x)]++;
    }else{
      posCounts[pairs(i,x)] = 1;
    }
  }

  int max = 0;
  std::map<double, int>::iterator it;
  for (it = posCounts.begin(); it != posCounts.end(); ++it  )
  {
    if(it->second > max){
      max = it->second;
    }
  }
  max++;
  
  NumericMatrix final(max,2);
  int zeros = 0;
  for (it = posCounts.begin(); it != posCounts.end(); ++it  )
  {
    final(it->second, 1)++;
    zeros++;
  }  
  final(0,1) = n - zeros;
  
  for(int i = 0; i < max; i++){
    final(i,0) = i;
  }
  
  return final;
}
