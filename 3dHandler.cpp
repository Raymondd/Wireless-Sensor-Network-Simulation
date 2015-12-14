#include <Rcpp.h>
#include <map>
#include <limits>
#include <vector>
#include <algorithm>
using namespace Rcpp;

NumericMatrix points;
NumericMatrix pairs;
NumericVector smallestLast;
int n;
int x = 0;
int y = 1;
int z = 2;
typedef std::vector<double> list;
std::map<double, list> adj_list;
std::map<double, int> colors;

struct point{double point[3];};
std::map<double, point> map_points;

int max1[2] = {1,2};
int max2[2] = {1,3};
int max3[2] = {1,4};



// [[Rcpp::export]]
void set_points(NumericMatrix p){
  points = p;
  n = p.nrow();
  for(int i = 0; i < n; i++){
    point myPoint = {p(i,x), p(i,y), p(i,z)};
    map_points[p(i,x)] = myPoint;
  }
}

// [[Rcpp::export]]
void printAdjList(){
  std::map<double, list>::iterator l;
  for (l = adj_list.begin(); l != adj_list.end(); ++l  )
  {
    std::cout << l->first << " : ";
    for(double i : l->second)
      std::cout << i << ", " ;
      
    std::cout << std::endl;
  }
}

// [[Rcpp::export]]
NumericMatrix colorGraph(){
  
  for(int i = 0; i < n; i++){
    double current = smallestLast[i];
    
    int color = 1;
    bool colorFound = true;
    while(colorFound){
      colorFound = false;
      for(double neighbor : adj_list[current]){
        if(colors[neighbor] == color){
          if(!colorFound){
            color++;
            colorFound = true;
          }
        }
      }
    }
    
    colors[current] = color;
  }
  
  NumericMatrix final(n,4);
  std::map<double, int>::iterator l;
  int index = 0;
  for (l = colors.begin(); l != colors.end(); ++l){
    //double point_[3] = map_points[l->first][0]; 
    final(index, 0) = map_points[l->first].point[0];
    final(index, 1) = map_points[l->first].point[1];
    final(index, 2) = map_points[l->first].point[2];
    final(index, 3) = l->second;
    index++;
  }
  return final;
}

// [[Rcpp::export]]
NumericMatrix smallestLastOrdering() {
  NumericMatrix final(points.nrow(),2);
  NumericVector order(points.nrow());
  
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
    order[next] = minKey;
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
  
  smallestLast = order;
  return final;
}

// [[Rcpp::export]]
NumericMatrix smallestLastOrdering2() {
  NumericMatrix final(points.nrow(),2);
  NumericVector order(points.nrow());
  
  std::map<double, int> counts;
  std::map<double, int> org_counts;
  std::map<double, list>::iterator l;
  for (l = adj_list.begin(); l != adj_list.end(); ++l  )
  {
    counts[l->first] = l->second.size();
    org_counts[l->first] = l->second.size();
  }
  
  int next = n-1;
  std::map<double, int>::iterator it;
  while (next >= 0){
    //find min key
    int min = counts.begin()->second;
    double minKey = counts.begin()->first;
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
    order[next] = minKey;
    next--;
    
    //decrement neighbors and remove key
    counts.erase(minKey);
    for(double i : adj_list[minKey]){
      if(counts.find(i) != counts.end()){
        counts[i]--;
      }
    }
  }
  smallestLast = order;
  return final;
}

// [[Rcpp::export]]
NumericMatrix smallestLastOrdering3() {
  NumericMatrix final(points.nrow(),2);
  NumericVector order(points.nrow());
  
  std::map<double, int> counts;
  std::map<double, int> org_counts;
  std::map<int, list> buckets;
  std::map<double, list>::iterator l;
  int max = 0;
  for (l = adj_list.begin(); l != adj_list.end(); ++l)
  {
    int size = l->second.size();
    counts[l->first] = size;
    org_counts[l->first] = size;
    
    if(buckets.find(size) == buckets.end()){
      buckets[size] = list();
    }
    buckets[size].push_back(l->first);
    
    if(size > max){
      max = size;
    }
  }
  
  int next = n-1;
  std::map<int, list>::iterator it;
  while (next >= 0){
    //find min key
    int min;
    double minKey;
    for (int i = 0; i < max+1; i++)
    {
      if(buckets[i].size() > 0){
        min = i;
        minKey = buckets[i][0];
        buckets[i].erase(buckets[i].begin());
        break;
      }
    }
    
    //add to output
    final(next, 0) = min;
    final(next, 1) = org_counts[minKey];
    order[next] = minKey;
    next--;
    
    //decrement neighbors and remove key
    counts.erase(minKey);
    for(double i : adj_list[minKey]){
      if(counts.find(i) != counts.end()){
        buckets[counts[i]].erase(std::remove(buckets[counts[i]].begin(), buckets[counts[i]].end(), i), buckets[counts[i]].end());
        counts[i]--;
        if(buckets.find(counts[i]) == buckets.end()){
          buckets[counts[i]] = list();
        }
        buckets[counts[i]].push_back(i);
      }
    }
  }
  smallestLast = order;
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
NumericMatrix distance3D(double radius, int neighbors) {
  int size = n*neighbors*2;
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
            
          adj_list[points(i,x)].push_back(points(j,x)); 
          adj_list[points(j,x)].push_back(points(i,x));
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
NumericMatrix lineSweepDistance(double radius) {
  int size = n*n;
  if (size < 0){
    size = std::numeric_limits<int>::max();
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
            
          adj_list[points(i,x)].push_back(points(j,x)); 
          adj_list[points(j,x)].push_back(points(i,x));
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
  for (it = posCounts.begin(); it != posCounts.end(); ++it)
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

// [[Rcpp::export]]
void calculateSubgraphs(double radius){
  
  std::map<double, int>::iterator l;
  for(int i=1; i < 4; i++){
    for(int j = i+1; j <= 4, j++){
      int count = 0;
      for (l = colors.begin(); l != colors.end(); ++l){ 
        if(l->second == i || l->second == j){
          for (int i=0; i < adj_list[l->first].size(); i++) {
            if(colors[adj_list[l->first][i]] == i || colors[adj_list[l->first][i]] == j){
              count++;
            }
          }
        }
      }
    }
  }
}

// [[Rcpp::export]]
NumericMatrix maximumSubgraph1(){
  NumericMatrix sub_points(pairs.nrow(),3);
  
  std::map<double, int>::iterator l;
  int index = 0;
  for (l = colors.begin(); l != colors.end(); ++l){ 
    if(l->second == max1[0] || l->second == max1[1]){
      for (int i=0; i < adj_list[l->first].size(); i++) {
        if(colors[adj_list[l->first][i]] == max1[0] || colors[adj_list[l->first][i]] == max1[1]){
          sub_points(index, 0) = map_points[l->first].point[0];
          sub_points(index, 1) = map_points[l->first].point[1];
          sub_points(index, 2) = map_points[l->first].point[2];
          index++;
          sub_points(index, 0) = map_points[adj_list[l->first][i]].point[0];
          sub_points(index, 1) = map_points[adj_list[l->first][i]].point[1];
          sub_points(index, 2) = map_points[adj_list[l->first][i]].point[2];
          index++;
        }
      }
    }
  }
  
  
  NumericMatrix final(index, 3);
  for (int i = 0; i < index; i++){
    final(i, 0) = sub_points(i, 0);
    final(i, 1) = sub_points(i, 1);
    final(i, 2) = sub_points(i, 2);
  }
  return final;
}

// [[Rcpp::export]]
NumericMatrix maximumSubgraph2(){
  
}

// [[Rcpp::export]]
NumericMatrix maximumSubgraph3(){
  
}

