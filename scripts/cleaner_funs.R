#Libraries

#Functions
factor.Adjust = function(data,adj,narep=FALSE) {
  for (i in seq(nrow(adj))) { 
    x = as.character(unlist(unname(adj[i,])))
    feature = x[1]
    replace = x[2] 
    with = x[3]
    
    if (!narep) {
      
      if (is.factor(data[,feature])) {
        feature.NewLevels = gsub(replace,with,levels(data[,feature]),fixed=TRUE)
        levels(data[,feature]) = feature.NewLevels
      } else
        data[data[,feature]==as.numeric(replace),feature] = as.numeric(with)
    } else {
      my.Class = class(data[,feature])
      data[is.na(data[,feature]),feature] = with
      class(data[,feature]) = my.Class
      
    }
  }
  return(data)
}

indicator.Add = function(data,indices) {
  for (i in seq(length(indices))) {
    new.Col = names(indices)[i]
    data[,new.Col] = 0
    data[indices[[i]],new.Col] = 1
  }
  return(data)
}

impute.bySampling = function(data) {
  data.Complete = data[!is.na(data)]
  na.Indices = which(is.na(data))
  
  sample.Size = length(na.Indices)
  data[na.Indices] = sample(data.Complete,size=sample.Size,replace=TRUE)
  return(data)
}

find.Inconsistencies = function(data1, data2) {
  lvl.Check = list()
  for (i in seq(length(data1))) {
    print(paste(colnames(data1)[i],sum(!(data1[,i] %in% data2[,i]))))
  }
  return(lvl.Check)
}
