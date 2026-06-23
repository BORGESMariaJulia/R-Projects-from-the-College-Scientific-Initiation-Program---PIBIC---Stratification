'''
# 1- Consertar outputs dos decodificadores; OK
# 2- achar n; 2.1- Criar função objetivo; -- achei nh e estipulei n ---
# 3- Implementar decod e Fobj no BRKGA; OK

# L = número de estratos
# n = amostra total (ACHAR)
# Nh = População do estrato h
# Shx2 = variãncia do estrato h
# nh = amostra no estrato h

> Observação: Usar o vetor de teste X, decorre em ter menos de L estratos, 
deve ser pq X tem poucas obs. Usando a pop artificial unif (1000), q é A menor, 
isso muda;
> As alterações começam a partir da linha 45;
> Fobj na linha 95;
> brkga na linha 139; 

LIXO
Nh <- resultado.D1$Nh
Shx2 <- resultado.D1$Shx2
nh <- vector(length = L)
vtx <- vector(length = L)
XL <- list(X, L)
#nomear para chamar por letra e não número
names(XL) = c('X', 'L') 

L <- length(u.ordenado) + 1 #número de estratos


-----------------------------DECODIFICADOR 1------------------------------------
'''
L <- 7 
u <- c(runif((L-1), 0, 1)) #cortes entre 0 e 1
X <- c(runif(1000, 1, 30))


Decod1 <- function (u, X, L) {
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
 
  return(NhSh)
}

resultado.D1 <- Decod1(u, X)
(resultado.D1)

n = 100 #arbitrário
NhSh <- resultado.D1

Fobj <- function (NhSh, n)
{
  Nh <- NhSh[1:L]
  Shx2 <- NhSh[(L+1):(2*L)]
  den.ney <- sum(Nh * sqrt(Shx2))
  
  nh <- vector(length = L)
  for (h in 1:L) {
    #amostra por estrato usando neyman, sqrt(Shx2) pois é desv.pad
    a <- (n * Nh[h] * sqrt(Shx2[h]))/ den.ney
    a <- round(a)
    a <- max(2, a)
    a <- min(a, Nh[h])
    nh[h] <- a
    #variância total
  }
  
  soma.n = sum(nh)
  if (soma.n != n){
    if (soma.n > n){
      excesso = soma.n - n
      while (excesso>0){
        qr <- which.max(nh)
        nh[qr] <- nh[qr]-1
        excesso <- excesso-1
    }
  }
  else {
    falta = n-soma.n
    while (falta > 0) {
      qr <- which.max(Nh-nh)
      nh[qr] <- nh[qr]+1
      falta <- falta-1
      }
    }
    soma.n <- sum(nh)
  }
  
  vtx <- sum((Nh**2) * (Shx2/nh) * (1 - (nh/Nh)))
  #coeficiente de varição
  cv = 100 * sqrt(vtx)/sum(X)
  return(cv)
}
r.Fobj.D1 <- Fobj(NhSh, n)
(r.Fobj.D1)





'''
#Comparando a Variância Total por estratificação com Amostragem Casual Simples:

pop.N <- length(X)
Vtx.acm <- (pop.N^2) * (var(X) / n) * (1 - n/pop.N)
cat("Variância total por amostragem simples:", Vtx.acm, "\n")
cat("Variância total por estratificação (vtx):", r.Fobj.D1$vtx, "\n")

#Com isso é possível notar que estratificar realmente reduz a variância


#implementando BRKGA

brkga(Data = X,
      Fo = Fobj,
      Dc = Decod1,
      rc = 0.7,
      pe = 0.2,
      pm = 0.2,
      n,
      p = 100,
      ng = 2000,
      ngw = 500,
      MaxTime = 3600,
      MAX = F,
      Exa1 = NULL, 
      Exa2 = n
)
'''
