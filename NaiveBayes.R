library(mlbench)
library(AppliedPredictiveModeling)


myNaiveBayes = function(formula, df, laplaceCorrection = 0){
  #want to estimate p(c|x~) where x~ is a vector of features
  #that is p(~x|c)p(c)/p(~x)
  #find for a given ~x the c with the highest probability
  data = model.frame(formula, df)
  y = data[,1]
  nbModel = list()
  for(featureName in colnames(data[-1])){
    feature = data[[featureName]]
    nbModel[[featureName]] = data.frame(matrix(ncol=length(levels(feature)),nrow=length(levels(y))))
    #print(head(feature))
    #print(featureName)
    #print(length(levels(feature)))
    #print(length(levels(y)))
    #print(data.frame(matrix(ncol=length(levels(feature)),nrow=length(levels(y)))))
    #print(nbModel[[featureName]])
    colnames(nbModel[[featureName]]) = levels(feature)
    rownames(nbModel[[featureName]]) = levels(y)
    for(level in levels(feature)){
      for(levelY in levels(y)){
        nbModel[[featureName]][levelY,level] = prob(feature==level,y==levelY, laplaceCorrection = laplaceCorrection,
                                                    laplaceCorrectionFeatureCount = laplaceCorrection*length(levels(feature)))  
      }
    }
  }
  
  nbModel$labelProb = structure(numeric(0), names = character(0))
  for(levelY in levels(y)){
    nbModel$labelProb[levelY] = sum(y==levelY)/length(y)
  }
  
  return(nbModel)
}

predictNB = function(nbModel, namedFeatures){
  probDF = probabilitiesNB(nbModel, namedFeatures)
  return(colnames(probDF)[max.col(probDF, ties.method = "first")])
}

probabilitiesNB = function(nbModel,namedFeatures){
  probOfLabel = data.frame(matrix(ncol=nrow(nbModel[[1]]),nrow=nrow(namedFeatures)))
  colnames(probOfLabel) = rownames(nbModel[[1]])
  for(label in rownames(nbModel[[1]])){
    prob = nbModel$labelProb[label] #P(c)
    for(featureName in names(namedFeatures)){
      #print(featureName)
      #print(namedFeatures[[featureName]])
      #print(label)
      prob = prob * nbModel[[featureName]][label,namedFeatures[[featureName]]]
      #print(as.numeric(as.vector(prob[1,])))
    }
    probOfLabel[label] = as.numeric(as.vector(prob[1,]))
  }
  return(probOfLabel)
}

prob = function(xBool,cBool,laplaceCorrection=0, laplaceCorrectionFeatureCount=0){
  #P(x|c) estimate
  #Example: prob(iris$Sepal.Length>5.1,iris$Species=="setosa")
  #for the rows where c = cLevel
  #how many of their x was xLevel
  return((sum(cBool&xBool)+laplaceCorrection)/(sum(cBool)+laplaceCorrectionFeatureCount))
}


binByQuantile = function(x){
  quantiles = unique(quantile(x))
  quantiles[1] = quantiles[1] - 1 #so one value isn't NA'd
  return(cut(x, quantiles))
}

newIris = function(){
  catIris = data.frame(Species=iris$Species)
  catIris$Sepal.Length = binByQuantile(iris$Sepal.Length)
  catIris$Sepal.Width = binByQuantile(iris$Sepal.Width)
  catIris$Petal.Width = binByQuantile(iris$Petal.Width)
  catIris$Petal.Length = binByQuantile(iris$Petal.Length)
  return(catIris)
}

categorize = function(df){
  return(data.frame(lapply(df,function(x){
    #print(head(x))
    if(is.numeric(x)){
      binByQuantile(x)
    }else{
      x
    }
  })))
}

set.seed(5)

nbAccuracy = function(formula, df, percentTrain, laplace = 0){
  trainIndexes = sample(1:nrow(df),percentTrain*nrow(df))
  train = df[trainIndexes,]
  test = df[-trainIndexes,]
  testStripped = model.frame(formula,test)[-1] #remove the dependent variable
  nbModel = myNaiveBayes(formula, train, laplaceCorrection = laplace)
  sum(predictNB(nbModel,testStripped)==model.frame(formula,test)[[1]])/nrow(test)
}

nbMatrix = function(formula, df, percentTrain, laplace = 0){
  trainIndexes = sample(1:nrow(df),percentTrain*nrow(df))
  train = df[trainIndexes,]
  test = df[-trainIndexes,]
  testStripped = model.frame(formula,test)[-1] #remove the dependent variable
  nbModel = myNaiveBayes(formula, train, laplaceCorrection = laplace)
  table(paste("predicted",predictNB(nbModel,testStripped)),paste("actual", model.frame(formula,test)[[1]]))
}

repeatedTest = function(count, formula, df, percentTrain, laplace = 0){
  acc=0
  for(i in 1:count){
    acc=testNBAccuracy(formula, categorize(df), percentTrain, laplace)+acc
  }
  return(acc/count)
}