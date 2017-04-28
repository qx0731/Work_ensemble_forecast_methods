decorrelate = function(data, k1, k2)
  # input: data frame 
  # k1 the dimension for MDS
  # k2 the dimension for Kmean
  # output: data frame + mds fit + kmean fit
{
  df = cor(data[ ,sapply(data, is.numeric)], use = "complete")
  dd = df[rowSums(is.na(df)) != ncol(df)-1, colSums(is.na(df)) != nrow(df)-1]
  dd = dd[-1, -1]
  fit = cmdscale(1-abs(dd),eig=TRUE, k1)  # number here is determined by fit results 
  
  cl = kmeans(fit$points, k2)
  cl$cluster
  
  r_info = df[1, -1]
  r_info  = r_info[!is.na(r_info)]
  select_name ='response'
  include_name = ''
  
  for (i in 1:k2){
    select_name = c(select_name, names(which.max(abs(r_info[cl$cluster == i]))))
  }
  target_all1 = cbind(data[, select_name, drop = FALSE], data[, !sapply(data, is.numeric), drop = FALSE])
  return (list(target_all1, fit, cl))
}