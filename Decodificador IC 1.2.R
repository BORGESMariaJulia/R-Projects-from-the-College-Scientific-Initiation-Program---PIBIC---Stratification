#L = número de estratos
#u = vetor de chaves aleatórias
#L = número de estratos
#X = Vetor população
#Decod1 ordena o vetor u, Decod2 não ordena

#objetivos: gerar percentis entre 80 e 99 em relação a população X; fazer pts de
#corte bi; achar qtde. valores por estrato Nh; achar Shx2 por estrato h;


'''
---------------------------------- DECODIFICADOR 1 -----------------------------
'''

L <- 7 # número de estratos
u <- c(runif((L-1), 0, 1)) #número de cortes
#X <- c(runif(1000, 1, 30))

#vetor teste com valores repetidos para saber se eles caem no mesmo estrato
#X <- c(10,20,3,4,5,6,7,8,9,10,10,10,13,12,15,16,16,17,17,20,22)

X<-Kozak1
Decod1 <- function (u, X, L) {
  u.ordenado <- sort(u) #vetor de chaves ordem crescente
  
  px <- c(quantile(sort(X), probs = seq(0, 1, 0.01))) #percentis de x
  percentil.escolhido <- sample(px[80: 99], 1, replace = F)
  
  #vetor com os pontos de corte
  bi <- numeric(length(u.ordenado)) 
  for (i in 1:length(u)) {
    bi[i] = min(X) + percentil.escolhido * u.ordenado[i]/sum(u.ordenado[1:i])
  }
  
  #Range dos pontos de corte com bi ordenado
  pt.cortes <- unique(c(min(X)-1, sort(bi), max(X) + 1))
  #corta o vetor X em estratos e aloca de acordo com os cortes
  estratos <- cut(X, breaks = pt.cortes, labels = F) 
  lista_estratos <- split(X, estratos)
  names(lista_estratos) <- paste0('N', seq_along(lista_estratos))
  
  Nh <- c(sapply(lista_estratos, length)) #número de termos por estrato
  Shx2 <- c(sapply(lista_estratos, var)) #variância por estrato
  
  ###Ajuste de Nh para tornar a solução viável
  estratos_inviaveis <- which(Nh < 2)
  
  if (length(estratos_inviaveis) > 0){
    L <- length(Nh)
    maior_estrato <- which.max(Nh)
    dif <- sum(2-Nh[estratos_inviaveis])
    Nh[estratos_inviaveis] <- 2
    Nh[maior_estrato] <- Nh[maior_estrato]-dif
    Shx2 <- rep(0,L)
    for(i in 1:L) {
      Shx2[i] <- var(X[1:Nh[i]])
      X <- X[-c(1:Nh[i])]
    }
  }
  
  return(list(Nh=Nh, Shx2= Shx2, lista_estratos = lista_estratos, bi = bi))
}

resultado.D1 <- Decod1(u, X, L)
(resultado.D1)

'''
---------------------------------- DECODIFICADOR 2 -----------------------------
'''
X<- Kozak1
L<-7
u <- c(runif((L-1), 0, 1)) #número de cortes

Decod2 <- function (u, X, L) {
  px <- c(quantile(sort(X), probs = seq(0, 1, 0.01))) #percentis de x
  percentil.escolhido <- sample(px[80: 99], 1, replace = F)
  
  #vetor com os pontos de corte
  bi <- numeric(length(u)) 
  for (i in 1:length(u)) {
    bi[i] = min(X) + percentil.escolhido * u[i]/sum(u[1:i])
  }
  
  #Range dos pontos de corte com bi ordenado
  pt.cortes <- unique(c(min(X)-1, sort(bi), max(X) + 1))
  #corta o vetor X em estratos e aloca de acordo com os cortes
  estratos <- cut(X, breaks = pt.cortes, labels = F) 
  lista_estratos <- split(X, estratos)
  names(lista_estratos) <- paste0('N', seq_along(lista_estratos))
  
  Nh <- c(sapply(lista_estratos, length)) #número de termos por estrato
  Shx2 <- c(sapply(lista_estratos, var)) #variância por estrato
  
  ###Ajuste de Nh para tornar a solução viável
  estratos_inviaveis <- which(Nh < 2)
  
  if (length(estratos_inviaveis) > 0){
    L <- length(Nh)
    maior_estrato <- which.max(Nh)
    dif <- sum(2-Nh[estratos_inviaveis])
    Nh[estratos_inviaveis] <- 2
    Nh[maior_estrato] <- Nh[maior_estrato]-dif
    Shx2 <- rep(0,L)
    for(i in 1:L) {
      Shx2[i] <- var(X[1:Nh[i]])
      X <- X[-c(1:Nh[i])]
    }
  }
  
  return(list(Nh=Nh, Shx2= Shx2, lista_estratos = lista_estratos, bi = bi))
}

resultado.D2 <- Decod2(u, X, L)
(resultado)




'''
---------------------------------- DECODIFICADOR 3 -----------------------------
'''

X<- Kozak1
#X <- c(runif(10, 1, 30))
n <- unique(X) #extensão do vetor X sem repetição/ X´
u <- runif(length(n), 0, 1) #número de cortes

Decod3c <- function(u, X, L)
{
  X.unico <- sort(unique(X))
  
  # Ajusta u automaticamente para o tamanho correto
  u <- u[seq_len(length(X.unico))]
  u.ordenado <- sort(u)
  
  b <- ceiling(u.ordenado * L)
  b[b == 0] <- 1
  b[b > L] <- L
  
  cortes.L <- seq(0, 1, length.out = L + 1)
  
  est.dec <- cut(
    u.ordenado,
    breaks = cortes.L,
    labels = FALSE,
    include.lowest = TRUE
  )
  
  lista_est_dec <- split(u.ordenado, est.dec)
  names(lista_est_dec) <- paste0("I", seq_along(lista_est_dec))
  
  Nh.I <- sapply(lista_est_dec, length)
  
  lista_X_unico_estratos <- split(X.unico, b)
  
  bc <- c()
  
  if (L > 1) {
    for (k in 1:(L - 1)) {
      estrato.k <- lista_X_unico_estratos[[as.character(k)]]
      
      if (!is.null(estrato.k) && length(estrato.k) > 0) {
        bc <- c(bc, max(estrato.k))
      }
    }
  }
  
  pt.cortes <- sort(unique(c(min(X) - 1, bc, max(X) + 1)))
  
  estratos_X <- cut(
    X,
    breaks = pt.cortes,
    labels = FALSE,
    include.lowest = TRUE
  )
  
  lista_estratos_X <- split(X, estratos_X)
  names(lista_estratos_X) <- paste0("E", seq_along(lista_estratos_X))
  
  Nh <- sapply(lista_estratos_X, length)
  
  Shx2 <- sapply(lista_estratos_X, function(v) {
    if (length(v) > 1) var(v) else 0
  })
  estratos_inviaveis<-which(Nh<2)
  if (length(estratos_inviaveis) > 0){
    L <- length(Nh)
    maior_estrato <- which.max(Nh)
    dif <- sum(2-Nh[estratos_inviaveis])
    Nh[estratos_inviaveis] <- 2
    Nh[maior_estrato] <- Nh[maior_estrato]-dif
    Shx2 <- rep(0,L)
    for(i in 1:L) {
      Shx2[i] <- var(X[1:Nh[i]])
      X <- X[-c(1:Nh[i])]
    }
  }
  
  return(list(
    u.ordenado = u.ordenado,
    cortes.L = cortes.L,
    b = b,
    Nh.I = Nh.I,
    X.unico = X.unico,
    lista_X_unico_estratos = lista_X_unico_estratos,
    bc = bc,
    pt.cortes = pt.cortes,
    Nh = Nh,
    Shx2 = Shx2,
    lista_estratos_X = lista_estratos_X
  ))
}
resultado <- Decod3c(u, X, L)
(resultado)

'''
------------------------------ ainda vou usar ----------------------------------


#brkga(Data=X, Fo, Dc=Decod)



----------------------------------- teste --------------------------------------
'''
