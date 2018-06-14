
#install.packages('devtools')
#devtools::install_github("r-lib/progress")

aleatGen <- function(v, rep){
  library(progress)
  aux <- vector(mode = "list", length = 7)
  aux[[1]] <- numeric(rep)
  aux[[6]] <- progress_bar$new(
    format = "  Calculando reamostragens.. [:bar:percent] Tempo Restante :eta",
    total = rep, clear = FALSE, width= 80)
  while(sum(aux[[1]][aux[[1]]==0] == 0) != 0){
    aux[[2]] <- sample(v, length(v), replace=T)
    aux[[3]] <- numeric(length(v)); aux[[4]] <- 1; aux[[5]] <- 1
    for (j in seq(1, length(v)-1, by=1)) {
      if((j == length(v)-1) && (aux[[2]][j] == aux[[2]][j+1])){
        aux[[4]] = aux[[4]] + 1; aux[[3]][aux[[5]]] <- aux[[4]]; aux[[5]] = aux[[5]] + 1; aux[[4]] = 1
      } else if ((j == length(v)-1) && (aux[[2]][j] != aux[[2]][j+1])){
        aux[[3]][aux[[5]]] <- aux[[4]]; aux[[5]] = aux[[5]] + 1; aux[[4]] = 1}
      if(aux[[2]][j] == aux[[2]][j+1]){
        aux[[4]] <- aux[[4]] + 1
      } else {
        aux[[3]][aux[[5]]] <- aux[[4]]; aux[[5]] = aux[[5]] + 1; aux[[4]] = 1}}
    aux[[3]] <- aux[[3]][aux[[3]]!=0]
    aux[[1]][sum(aux[[1]][aux[[1]]==0] == 0)] <- length(aux[[3]]); aux[[6]]$tick()}
  return(list(Repeticoes= rep, Sequencias = aux[[1]]))
}

aleatCheck <- function(rnd, esT) {
  aux <- vector(mode = "list", length = 3)
  aux[[1]] <- numeric(length(unique(rnd$Sequencias)))
  for(j in seq(1, length(unique(rnd$Sequencias)), by=1)){
    aux[[1]][j] <- (sum(rnd$Sequencias == sort(unique(rnd$Sequencias))[j]))/length(rnd$Sequencias)
  }
  aux[[3]] <- data.frame(sort(unique(rnd$Sequencias)), aux[[1]])
  colnames(aux[[3]]) <- c('Alfa','Beta')
  aux[[2]] <- sort(unique(rnd$Sequencias))[which(aux[[1]] == rev(aux[[1]])[sum(cumsum(rev(aux[[1]])) <= 0.05)])]
  ifelse(length(sort(unique(rnd$Sequencias))[which(aux[[1]] == aux[[1]][sum(cumsum(aux[[1]]) <= 0.05)])]) == 0, 
         aux[[2]][2] <- 0, 
         aux[[2]][2] <- sort(unique(rnd$Sequencias))[which(aux[[1]] == aux[[1]][sum(cumsum(aux[[1]]) <= 0.05)])])
  if(esT > aux[[2]][1]){
    if(esT < aux[[2]][2])
    aux[[2]] <- paste0('Rejeita-se H0, p-valor: ', 1-((sum(rnd$Sequencias > 7) + sum(rnd$Sequencias < 7))/length(rnd$Sequencias)))
  } else {
    aux[[2]] <- paste0('Não se rejeita H0, p-valor: ', 1-((sum(rnd$Sequencias > 7) + sum(rnd$Sequencias < 7))/length(rnd$Sequencias)))
  }
  return(list(Tabela=aux[[3]], Decisao=aux[[2]]))
}

vetor <- c(1,1,0,0,0,1,0,0,0,0,1,1,0,1)
bootstrap <- aleatGen(vetor, 10000)
resultados <- aleatCheck(bootstrap, 7)
resultados

  
