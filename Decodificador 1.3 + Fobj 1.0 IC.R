'''
#u = vetor de chaves aleatórias
#L = número de estratos
#X = Vetor população
#XL = vetor população mais número de estratos no final
#Decod1 ordena o vetor u, Decod2 não ordena

#objetivos: gerar percentis entre 80 e 99 em relação a população X; fazer pts de
#corte bi; achar qtde. valores por estrato Nh; achar Shx2 por estrato h;

#Observação: Ao usar o vetor de teste X, aconteceu de ter menos de L estratos
#deve ser pq X tem poucas obs. Usando a pop artificial unif (1000),q é a menor,
#isso muda

AJUSTE DO GEMINI SOBRE OS DECODIFICADORES

     ###Ajuste de Nh para tornar a solução viável
    estratos_inviaveis <- which(Nh < 2)
    
    if (length(estratos_inviaveis) > 0){
      L <- length(Nh)
      maior_estrato <- which.max(Nh)
      dif <- sum(2 - Nh[estratos_inviaveis])
      
      # Ajusta os tamanhos (Nh)
      Nh[estratos_inviaveis] <- 2
      Nh[maior_estrato] <- Nh[maior_estrato] - dif
      
      # CORREÇÃO: Recalcula Shx2 direto da lista original de estratos,
      # atribuindo variância 0 (ou um valor mínimo) apenas onde não há dados 
      suficientes para calcular var
      for(i in 1:L) {
        valores_do_estrato <- lista_estratos[[i]]
        if (length(valores_do_estrato) >= 2) {
          Shx2[i] <- var(valores_do_estrato)
        } else {
          Shx2[i] <- 0 # Se tem 0 ou 1 elemento, a variância interna é nula
          
---------------------------------- DECODIFICADOR 1 -----------------------------
ordena u
'''
#vetor teste com valores repetidos para saber se eles caem no mesmo estrato
#X <- c(10,20,3,4,5,6,7,8,9,10,10,10,13,12,15,16,16,17,17,20,22)

L <- 7 # número de estratos
u <- c(runif((L-1), 0, 1)) #número de cortes
X <- c(runif(1000, 1, 30))

Decod1 <- function (u, XL) {
  u.ordenado <- sort(u) #vetor de chaves ordem crescente
  L <- length(u.ordenado) + 1 #número de estratos
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
    dif <- sum(2 - Nh[estratos_inviaveis])
    
    # Ajusta os tamanhos (Nh)
    Nh[estratos_inviaveis] <- 2
    Nh[maior_estrato] <- Nh[maior_estrato] - dif
    
    # CORREÇÃO: Recalcula Shx2 direto da lista original de estratos,
    # atribuindo variância 0 (ou um valor mínimo) apenas onde não há dados suficientes para calcular 'var'
    for(i in 1:L) {
      valores_do_estrato <- lista_estratos[[i]]
      if (length(valores_do_estrato) >= 2) {
        Shx2[i] <- var(valores_do_estrato)
      } else {
        Shx2[i] <- 0 # Se tem 0 ou 1 elemento, a variância interna é nula
      }
    }
  }
  
  NhSh = vector(length = (2*L))
  NhSh[1:L]<-Nh
  NhSh[(L+1):(2*L)]<-Shx2
  (NhSh)
  
  return(list(NhSh = NhSh, Nh = Nh, Shx2 = Shx2, L = L, lista_estratos=lista_estratos))
}

resultado.D1 <- Decod1(u, XL)
(resultado.D1)

n = 100
Nh <- resultado.D1$Nh
Shx2 <- resultado.D1$Shx2
nh <- vector(length = L)
vtx <- vector(length = L)

#denominador neyman: vetorização -> soma(pop h * dev.pad h)
den.ney <- sum(Nh * sqrt(Shx2))

Fobj <- function (X, NhSh, n){
  for (h in 1:L){
    #amostra por estrato usando neyman, sqrt(Shx2) pois é desv.pad
    a <- (n * Nh[h] * sqrt(Shx2[h]))/ den.ney
    a <- round(a)
    a <- max(2, a) 
    a <- min(a, Nh[h])
    nh[h] <- a
    
    #variância total
    v <- ((Nh[h]**2) * ((Shx2[h])/nh[h]) * (1 - (nh[h]/Nh[h])))
    vtx[h] <- v
    
  }
  vtx <- sum(vtx)
  (nh)
  (vtx)
  
  #coeficiente de varição
  cv = 100 * (sqrt(vtx)/sum(X))
  
  return(list(nh = nh, vtx = vtx, cv = cv ))
}
r.Fobj.D1 <- Fobj(X, NhSh, n)
(r.Fobj.D1)


'''
#Comparando a Variância Total por estratificação com Amostragem Casual Simples:

pop.N <- length(X)
Vtx.acm <- (pop.N^2) * (var(X) / n) * (1 - n/pop.N)
cat("Variância total por amostragem simples:", Vtx.acm, "\n")
cat("Variância total por estratificação (vtx):", r.Fobj.D1$vtx, "\n")

#Com isso é possível notar que estratificar realmente reduz a variância
'''


'''
---------------------------------- DECODIFICADOR 2 -----------------------------
não ordena u
'''

#vetor teste com valores repetidos para saber se eles caem no mesmo estrato
#X <- c(10,20,3,4,5,6,7,8,9,10,10,10,13,12,15,16,16,17,17,20,22)

L <- 7 # número de estratos
u <- c(runif((L-1), 0, 1)) #número de cortes
X <- c(runif(1000, 1, 30))

Decod2 <- function (u, X) {
  L <- length(u) + 1 #número de estratos
  
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
  
  NhSh = vector(length = (2*L))
  NhSh[1:L]<-Nh
  NhSh[(L+1):(2*L)]<-Shx2
  (NhSh)
  
  return(NhSh)
}

resultado.D2 <- Decod2(u, X)
(resultado.D2)



'''
---------------------------------- DECODIFICADOR 3 -----------------------------
'''

X <- c(runif(1000, 1, 30))
n <- unique(X) #extensão do vetor X sem repetição/ X´
u <- runif(length(n), 0, 1) #número de cortes

Decod3c <- function(u, X)
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
  
  NhSh = vector(length = (2*L))
  NhSh[1:L]<-Nh
  NhSh[(L+1):(2*L)]<-Shx2
  (NhSh)
  
  return(NhSh)
}

resultado.D3 <- Decod3c(u, X)
(resultado.D3)


'''
------------------------------ ainda vou usar ----------------------------------


#brkga(Data=X, Fo, Dc=Decod)



----------------------------------- teste --------------------------------------
'''
