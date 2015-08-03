#define a function to filter the data
#m is the data quality raster
#return a raster stack, layer 1-4 represent different quality levels
QAfind.mt2 = function(m){ 
  ext = extent(m)
  proj.geo = projection(m)
  
  m = as.matrix(m)
  nc = ncol(m);nr = nrow(m)
  
  bit.start <- c(1,2,seq(5, 32, 4))
  bit.end <- c(1,4,seq(8, 32, 4))
  
  #change value into bit
  qc.bit = apply(m,1,function(x){paste(rev(as.integer(intToBits(rev(x)))), collapse="")})
  bit.start.band1 = bit.start[9]+32*(0:(nc-1))
  bit.end.band1 = bit.end[9]+32*(0:(nc-1))
  
  bit.start.band2 = bit.start[8]+32*(0:(nc-1))
  bit.end.band2 = bit.end[8]+32*(0:(nc-1))
  #band 3
  bit.start.band3 = bit.start[7]+32*(0:(nc-1))
  bit.end.band3 = bit.end[7]+32*(0:(nc-1))
  
  #band 4
  bit.start.band4 = bit.start[6]+32*(0:(nc-1))
  bit.end.band4 = bit.end[6]+32*(0:(nc-1))
  
  #band 5
  bit.start.band5 = bit.start[5]+32*(0:(nc-1))
  bit.end.band5 = bit.end[5]+32*(0:(nc-1))
  
  #band 6
  bit.start.band6 = bit.start[4]+32*(0:(nc-1))
  bit.end.band6 = bit.end[4]+32*(0:(nc-1))
  
  #band 7
  bit.start.band7 = bit.start[3]+32*(0:(nc-1))
  bit.end.band7 = bit.end[3]+32*(0:(nc-1))
  
  #return a list, each element is a points
  qc.bit.band1 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band1, bit.end.band1),2)}) 
  qc.bit.band2 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band2, bit.end.band2),2)})
  qc.bit.band3 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band3, bit.end.band3),2)})
  qc.bit.band4 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band4, bit.end.band4),2)})
  qc.bit.band5 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band5, bit.end.band5),2)})
  qc.bit.band6 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band6, bit.end.band6),2)})
  qc.bit.band7 = lapply(qc.bit,function(x){strtoi(substring(x, bit.start.band7, bit.end.band7),2)})
  
  qc = data.frame(band1 = unlist(qc.bit.band1), 
                  band2 = unlist(qc.bit.band2),
                  band3 = unlist(qc.bit.band3),
                  band4 = unlist(qc.bit.band4),
                  band5 = unlist(qc.bit.band5),
                  band6 = unlist(qc.bit.band6),
                  band7= unlist(qc.bit.band7))
  
  qc0 = apply(qc, 1, function(x){length(which(x > 0))})  #only select quality as 0
  qc1 = apply(qc, 1, function(x){length(which(x > 1))})  #only select quality as 0, 1
  qc2 = apply(qc, 1, function(x){length(which(x > 2))})  #only select quality as 0, 1,2
  qc3 = apply(qc, 1, function(x){length(which(x > 3))})  #only select quality as 0, 1,2
    
  #change back to raster
  q0 = raster(matrix(qc0, nrow = nr, ncol = nc, byrow = TRUE),    
                xmn=ext@xmin, xmx=ext@xmax,
                ymn=ext@ymin, ymx=ext@ymax, 
                crs=CRS(proj.geo))
  
  q1 = raster(matrix(qc1, nrow = nr, ncol = nc, byrow = TRUE),    
              xmn=ext@xmin, xmx=ext@xmax,
              ymn=ext@ymin, ymx=ext@ymax, 
              crs=CRS(proj.geo))
  
  q2 = raster(matrix(qc2, nrow = nr, ncol = nc, byrow = TRUE),    
              xmn=ext@xmin, xmx=ext@xmax,
              ymn=ext@ymin, ymx=ext@ymax, 
              crs=CRS(proj.geo))
  
  q3 = raster(matrix(qc3, nrow = nr, ncol = nc, byrow = TRUE),    
              xmn=ext@xmin, xmx=ext@xmax,
              ymn=ext@ymin, ymx=ext@ymax, 
              crs=CRS(proj.geo))
  
  return(stack(q0,q1,q2,q3))
}
