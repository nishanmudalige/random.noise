random.vec.na = function(v, p){
  
  if(!is.vector(v)){
    warning("v must be a vector")
    return()
  }
  
  if(!is.numeric(p) | p<0 | p>1){
    warning("p must be a real number between 0 and 1")
    return()
  }
  
  n = length(v)
  na.indices = sample(1:n, p*n)
  v[na.indices] = NA
  
  if( !any(is.na(v)) ){
    warning("No NA's produced. Either vector is too small or p is too small")
  }
  
  return(v)
}


random.na = function(df, p){
  
  if( !is.vector(df) & !is.data.frame(df) ){
    warning("df must be a vector or a data fame.")
    return()
  }
  
  df = lapply(df, random.vec.na, p=p)
  df = as.data.frame.list(df, stringsAsFactors = F)
  return(df)
}
