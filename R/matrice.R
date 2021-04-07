
#' Title
#'
#' @param bp
#' @param chromosomelist
#' @param plasmidlist
#' @param k
#' @param nchromosome
#' @param nplasmid
#'
#' @return
#' @export
#'
#' @examples
Matrice <- function(bp = 1000, chromosomelist,plasmidlist,k = 5,nchromosome=1000,nplasmid = 1000){
  observation = nchromosome+nplasmid
result <- matrix(nrow=observation,ncol=noligo)
for (i in 1:nchromosome) {
  #bp=sample(300:5000)
  x3 <- sample(1:nchromosome, 1)
  currentgenome = readDNAStringSet(chromosomelist[x3])
  while (width(currentgenome)<bp) {
    x3 <- sample(1:nchromosome, 1)
    currentgenome = readDNAStringSet(plasmidlist[x3])
  }
  start = sample(1:(width(currentgenome)-bp),1)
  currentgenome = currentgenome[[1]][start:(start+bp-1)]
  result[i,] =oligonucleotideFrequency(x = currentgenome,k)
  print(i)
}

for (i in (nchromosome+1):nplasmid) {
  #bp=sample(300:5000)
  x3 <- sample(1:nplasmid, 1)
  currentgenome = readDNAStringSet(plasmidlist[x3])
  while (width(currentgenome)<bp) {
    x3 <- sample(1:nplasmid, 1)
    currentgenome = readDNAStringSet(plasmidlist[x3])
  }
  start = sample(1:(width(currentgenome)-bp),1)
  currentgenome = currentgenome[[1]][start:(start+bp-1)]
  result[i,]=oligonucleotideFrequency(x = currentgenome,k)
  print(i)
}

result = data.frame(result)
y = rep(1,1000)
x = rep(0,1000)
yx = c(y,x)
result$prediction = yx


test = oligonucleotideFrequency(x = currentgenome,k)
colnames(result)=c(names(test),"prediction")
return(result)
}
