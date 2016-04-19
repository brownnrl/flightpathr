# flightpathr 

Tools to analyze aircraft and flight path data.

Right now there's only one really useful function, `distanceFromPath()`. This 
measures the distance between an actual flight track (a matrix of long/lat data 
at each time point) and a flight path (a matrix of long/lat data at each 
waypoint). Horizontal and vertical distance are returned separately.

The helper function `maxDistanceFromPath()` finds the horizontal and vertical
distance at the point of maximum "slant range" (i.e., Euclidean distance).
