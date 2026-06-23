unf <- function(mn,mx){
  punf.a <- data.frame(runif(1000, mn, mx))
  punf.b <- data.frame(runif(5000, mn, mx))
  punf.c <- data.frame(runif(10000, mn, mx))
  punf.d <- data.frame(runif(25000, mn, mx))
  punf.e <- data.frame(runif(50000, mn, mx))
  
  write.table(punf.a, file = 'UNIF_1000.txt', sep = '\t', dec = '.')
  write.table(punf.b, file = 'UNIF_5000.txt', sep = '\t', dec = '.')
  write.table(punf.c, file = 'UNIF_10000.txt', sep = '\t', dec = '.')
  write.table(punf.d, file = 'UNIF_25000.txt', sep = '\t', dec = '.')
  write.table(punf.e, file = 'UNIF_50000.txt', sep = '\t', dec = '.')
  
  return()
}
popsunf <- unf(1,2)



no <- function( md, desvpad){
  pon.a <- data.frame(rnorm(1000, mean = md, sd = desvpad))
  pon.b <- data.frame(rnorm(5000, mean = md, sd = desvpad))
  pon.c <- data.frame(rnorm(10000, mean = md, sd = desvpad))
  pon.d <- data.frame(rnorm(25000, mean = md, sd = desvpad))
  pon.e <- data.frame(rnorm(50000, mean = md, sd = desvpad))
  
  write.table(pon.a, file = 'NORM_1000.txt', sep = '\t', dec = '.')
  write.table(pon.b, file = 'NORM_5000.txt', sep = '\t', dec = '.')
  write.table(pon.c, file = 'NORM_10000.txt', sep = '\t', dec = '.')
  write.table(pon.d, file = 'NORM_25000.txt', sep = '\t', dec = '.')
  write.table(pon.e, file = 'NORM_50000.txt', sep = '\t', dec = '.')
  
  return()
}
popsno <- no(3,10)


#shape = numero de eventos esperados
#scale = tempo médio entre eventos
gm <- function(sh , sc){
  pgm.a <- data.frame(rgamma(1000, shape = sh, scale = sc))
  pgm.b <- data.frame(rgamma(5000, shape = sh, scale = sc))
  pgm.c <- data.frame(rgamma(10000, shape = sh, scale = sc))
  pgm.d <- data.frame(rgamma(25000, shape = sh, scale = sc))
  pgm.e <- data.frame(rgamma(50000, shape = sh, scale = sc))
  
  write.table(pgm.a, file = 'GAMMA_1000.txt', sep = '\t', dec = '.')
  write.table(pgm.b, file = 'GAMMA_5000.txt', sep = '\t', dec = '.')
  write.table(pgm.c, file = 'GAMMA_10000.txt', sep = '\t', dec = '.')
  write.table(pgm.d, file = 'GAMMA_25000.txt', sep = '\t', dec = '.')
  write.table(pgm.e, file = 'GAMMA_50000.txt', sep = '\t', dec = '.')
  
  return()
}
popsgm <- gm(3,10)



ex <- function(sc){
  pex.a <- data.frame(rexp(1000, scale = sc))
  pex.b <- data.frame(rexp(5000, scale = sc))
  pex.c <- data.frame(rexp(10000, scale = sc))
  pex.d <- data.frame(rexp(25000, scale = sc))
  pex.e <- data.frame(rexp(50000, scale = sc))
  
  write.table(pex.a, file = 'EXPO_1000.txt', sep = '\t', dec = '.')
  write.table(pex.b, file = 'EXPO_5000.txt', sep = '\t', dec = '.')
  write.table(pex.c, file = 'EXPO_10000.txt', sep = '\t', dec = '.')
  write.table(pex.d, file = 'EXPO_25000.txt', sep = '\t', dec = '.')
  write.table(pex.e, file = 'EXPO_50000.txt', sep = '\t', dec = '.')
  
  return()
}
popsex <- ex(3)



quiq <- function(gl){
  pquiq.a <- data.frame(rchisq(1000, df = gl))
  pquiq.b <- data.frame(rchisq(5000, df = gl))
  pquiq.c <- data.frame(rchisq(10000, df = gl))
  pquiq.d <- data.frame(rchisq(25000, df = gl))
  pquiq.e <- data.frame(rchisq(50000, df = gl))
  
  write.table(pquiq.a, file = 'CHISQ_1000.txt', sep = '\t', dec = '.')
  write.table(pquiq.b, file = 'CHISQ_5000.txt', sep = '\t', dec = '.')
  write.table(pquiq.c, file = 'CHISQ_10000.txt', sep = '\t', dec = '.')
  write.table(pquiq.d, file = 'CHISQ_25000.txt', sep = '\t', dec = '.')
  write.table(pquiq.e, file = 'CHISQ_50000.txt', sep = '\t', dec = '.')
  
  return()
}
popsquiq <- quiq(3)



