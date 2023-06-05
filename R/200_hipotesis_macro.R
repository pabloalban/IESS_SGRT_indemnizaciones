message(paste(rep("-", 100), collapse = ""))

#Paquetes-------------------------------------------------------------------------------------------
require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(ggfortify)  #for plotting timeseries
require(chron)
require(lubridate)
require(zoo)
require(lmtest)
require(MTS)
require(lattice)
require(grid)
require(fUnitRoots)

"macro" <- function ( c_pib, c_a, c_p,  c_s, c_su, c_i,  th ) 
{
#Cargando Datos-------------------------------------------------------------------------------------
message("\tCargando tasas macro")
load(paste0(parametros$RData, "IESS_contexto_economico.RData"))


#Preparando data por años---------------------------------------------------------------------------
tasas_macro <- tasas_macro %>%
  dplyr::filter(anio>=2006) %>%
  group_by( anio ) %>%
  mutate( tasa_activa = mean( tasa_activa, na.rm= TRUE),
          tasa_pasiva = mean( tasa_pasiva, na.rm= TRUE),
          pib = sum( pib_int, na.rm = TRUE),
          sbu = mean( sbu, na.rm = TRUE),
          sal = sum( sal_prom, na.rm = TRUE),
          inflacion = mean( inflacion_anual, na.rm = TRUE) ) %>%
  mutate( sal = ifelse(sal>0, sal, NA)) %>%
  ungroup() %>%
  distinct( anio, .keep_all = TRUE) %>%
  dplyr::select( anio, pib, tasa_activa, tasa_pasiva, sal, sbu, inflacion)



#Pruebas de estacionariedad de Dickey-Fuller--------------------------------------------------------
#Ho: No hay raíz unitaria presente
aux_1 <- adfTest( diff( tasas_macro$pib, differences = 1 ) )
aux_2 <- adfTest( diff( tasas_macro$tasa_activa, differences = 1 ) )
aux_3 <- adfTest( diff( tasas_macro$tasa_pasiva, differences = 1 ) )
aux_4 <- adfTest( diff( tasas_macro$sal, differences = 1 ) )
aux_5 <- adfTest( diff( tasas_macro$sbu[5:15], differences = 1 ) ) #se remueve incrementos atípicos del 2008
aux_6 <- adfTest( diff( tasas_macro$inflacion, differences = 1 ) )

dickey_fuller <- data.frame( variable = c('$\\Delta$ PIB',
                                          '$\\Delta$ Tasa activa',
                                          '$\\Delta$ Tasa pasiva',
                                          '$\\Delta$ Salarios Promedio',
                                          '$\\Delta$ SBU',
                                          '$\\Delta$ Inflación Promedio'),
                             estadistico = c( aux_1@test$statistic,
                                              aux_2@test$statistic,
                                              aux_3@test$statistic,
                                              aux_4@test$statistic,
                                              aux_5@test$statistic,
                                              aux_6@test$statistic ),
                             p_valor = c( aux_1@test$p.value,
                                          aux_2@test$p.value,
                                          aux_3@test$p.value,
                                          aux_4@test$p.value,
                                          aux_5@test$p.value,
                                          aux_6@test$p.value ) )

#Reducción de variables-----------------------------------------------------------------------------
pi =  c_pib * sqrt(stats::var(tasas_macro$pib, na.rm = TRUE)); #10 #inv
i1 = c_i * sqrt(stats::var(tasas_macro$inflacion, na.rm = TRUE)); #4 #inv
s = c_s * sqrt(stats::var(tasas_macro$sal, na.rm = TRUE));  #100 #inv
a = c_a * sqrt(stats::var(tasas_macro$tasa_activa, na.rm = TRUE)); #120 #prop
p = c_p * sqrt(stats::var(tasas_macro$tasa_pasiva, na.rm = TRUE)); #200 #prop
su = c_su * sqrt(stats::var(tasas_macro$sbu, na.rm = TRUE)) ; #1.8 #inv max 50

#Diferenciación de la series------------------------------------------------------------------------
data <- tasas_macro %>%
  dplyr::filter(anio >= 2006) %>%
  mutate(diff_pib = c(NA, diff(pib,1))/pi,
         diff_tasa_activa = c(NA,diff(tasa_activa,1))/a,
         diff_tasa_pasiva = c(NA,diff(tasa_pasiva,1))/p,
         diff_sal_prom = c(NA,diff(sal,1))/s,
         diff_sbu = c(NA,diff(sbu, differences = 1 ))/su,
         diff_inflacion = c(NA,base::diff((inflacion),1))/i1) %>%
  dplyr::select(-pib, -tasa_activa, -tasa_pasiva, -sal, -anio, -sbu, -inflacion)
data <- data[-1,]

train <- tasas_macro %>%
  dplyr::filter(anio >= 2006) %>%
  # mutate( sbu = ifelse( anio %in% c(2022 ),
  #                       NA,
  #                       sbu ) ) %>%
  mutate(diff_pib = c(NA, diff(pib,1))/pi,
         diff_tasa_activa = c(NA,diff(tasa_activa,1))/a,
         diff_tasa_pasiva = c(NA,diff(tasa_pasiva,1))/p,
         diff_sal_prom = c(NA,diff(sal,1))/s,
         diff_sbu = c(NA, diff(sbu, differences = 1 ))/su,
         diff_inflacion = c(NA,base::diff((inflacion),1))/i1) %>%
  dplyr::select(-pib, -tasa_activa, -tasa_pasiva, -sal, -anio, -sbu, -inflacion) %>%
  na.omit( . )

#Estimación del modelo VAR(1)-----------------------------------------------------------------------
m1<-VAR(train, include.mean = F, p = 1)
m2=refVAR(m1,  thres = th )
m2 = m2
#m2$coef
variables <- data.frame(var=c('$\\Delta$ PIB',
                              '$\\Delta$ Tasa activa',
                              '$\\Delta$ Tasa pasiva',
                              '$\\Delta$ Salario Promedio',
                              '$\\Delta$ SBU',
                              '$\\Delta$ Inflación Promedio'))
coeficientes <- cbind(variables,
                      t(as.data.frame(m2$coef)) )

covarianza <- m2$Sigma 
covarianza <- data.frame( var=c('$\\Delta$ PIB',
                                '$\\Delta$ Tasa activa',
                                '$\\Delta$ Tasa pasiva',
                                '$\\Delta$ Salario Promedio',
                                '$\\Delta$ SBU',
                                '$\\Delta$ Inflación Promedio'), covarianza )
#Prueba de indepencia de residuos-------------------------------------------------------------------

#función mq aumentada-------------------------------------------------------------------------------
mq_aumentada <- function (x, lag = 24, adj = 0) 
{
  if (!is.matrix(x)) 
    x = as.matrix(x)
  nr = nrow(x)
  nc = ncol(x)
  g0 = var(x)
  ginv = solve(g0)
  qm = 0
  QM = NULL
  df = 0
  for (i in 1:lag) {
    x1 = x[(i + 1):nr, ]
    x2 = x[1:(nr - i), ]
    g = cov(x1, x2)
    g = g * (nr - i - 1)/(nr - 1)
    h = t(g) %*% ginv %*% g %*% ginv
    qm = qm + nr * nr * sum(diag(h))/(nr - i)
    df = df + nc * nc
    dff = df - adj
    mindeg = nc^2 - 1
    pv = 1
    if (dff > mindeg) 
      pv = 1 - pchisq(qm, dff)
    QM = rbind(QM, c(i, qm, dff, pv))
  }
  pvs = QM[, 4]
  dimnames(QM) = list(names(pvs), c("  m  ", "    Q(m) ", 
                                    "   df  ", " p-value"))
  cat("Ljung-Box Statistics: ", "\n")
  printCoefmat(QM, digits = 3)
  return(QM)
}

residuos = m2$residuals
residuos <- as.data.frame(residuos)
colnames(residuos) <- c("diff_pib",
                        "diff_tasa_activa",
                        "diff_tasa_pasiva",
                        "diff_sal_prom",
                        "diff_sbu",
                        "diff_inflacion")



box_ljung <- mq_aumentada( residuos, lag = 8) %>% #H0: Independencia de los residuos | Si p>0.05 no se rechaza la H0
  `colnames<-`(c("m", "Q", "df", "p_valor"))
#Prueba de nulidad de los coeficientes del VAR(1)---------------------------------------------------

##Función VARchi()-----------------------------------------------------------------------------------
"VARchi" <- function(x,p=1,include.mean=T, thres ){
  # Fits a vector AR(p) model, then performs
  # a chi-square test to zero out insignificant parameters.
  if(!is.matrix(x))x=as.matrix(x)
  Tn=dim(x)[1]
  k=dim(x)[2]
  if(p < 1)p=1
  ne=Tn-p
  ist=p+1
  y=x[ist:Tn,]
  if(include.mean){
    xmtx=cbind(rep(1,ne),x[p:(Tn-1),])
  }
  else {
    xmtx=x[p:(Tn-1),]
  }
  if(p > 1){
    for (i in 2:p){
      xmtx=cbind(xmtx,x[(ist-i):(Tn-i),])
    }
  }
  ndim=dim(xmtx)[2]   # #perform estimation
  res=NULL
  xm=as.matrix(xmtx)
  xpx=crossprod(xm,xm)
  xpxinv=solve(xpx)
  xpy=t(xm)%*%as.matrix(y)
  beta=xpxinv%*%xpy
  resi=y-xm%*%beta
  sse=t(resi)%*%resi/(Tn-p-ndim)
  C1=kronecker(sse,xpxinv)
  dd=sqrt(diag(C1))
  #
  bhat=c(beta)
  tratio=bhat/dd
  para=cbind(bhat,dd,tratio)
  npar=length(bhat)
  K=NULL
  omega=NULL
  for (i in 1:npar){
    if(abs(tratio[i]) < thres){
      idx=rep(0,npar)
      idx[i]=1
      K=rbind(K,idx)
      omega=c(omega,bhat[i])
    }
  }
  v=dim(K)[1]
  K=as.matrix(K)
  cat("Number of targeted parameters: ",v,"\n")
  #####print(K)
  if(v > 0){
    C2=K%*%C1%*%t(K)
    C2inv=solve(C2)
    tmp=C2inv%*%as.matrix(omega,v,1)
    chi=sum(omega*tmp)
    pvalue=1-pchisq(chi,v)
    cat("Chi-square test and p-value: ",c(chi,pvalue),"\n")
  }
  else{
    print("No contraints needed")
  }
  VARchi<-list( chi = chi,
                pvalue = pvalue )
}

test_nulidad_nulidad = VARchi(data[c(2:16),],p=1, th = 1.96) #H0: Nulidad de los coeficientes

test_nulidad_nulidad <- data.frame( concepto = c('Estadístico',
                                                 'P-valor' ),
                                    valor = c( test_nulidad_nulidad$chi,
                                               test_nulidad_nulidad$pvalue ) )

#Prueba de normalidad de los residuos---------------------------------------------------------------

aux_1 <- shapiro.test( residuos$diff_pib ) #H0: La variable sigue una distribución normal | Si p>0.05 no se rechaza la H0
aux_2 <- shapiro.test( residuos$diff_tasa_activa )
aux_3 <- shapiro.test( residuos$diff_tasa_pasiva )
aux_4 <- shapiro.test( residuos$diff_sal_prom )
aux_5 <- shapiro.test( residuos$diff_sbu )
aux_6 <- shapiro.test( residuos$diff_inflacion )

shapiro_test <- data.frame( variable = c('$\\Delta$ PIB',
                                          '$\\Delta$ Tasa activa',
                                          '$\\Delta$ Tasa pasiva',
                                          '$\\Delta$ Salarios Promedio',
                                          '$\\Delta$ SBU',
                                          '$\\Delta$ Inflación Promedio'),
                             estadistico_w = c( aux_1$statistic,
                                              aux_2$statistic,
                                              aux_3$statistic,
                                              aux_4$statistic,
                                              aux_5$statistic,
                                              aux_6$statistic ),
                             p_valor = c( aux_1$p.value,
                                          aux_2$p.value,
                                          aux_3$p.value,
                                          aux_4$p.value,
                                          aux_5$p.value,
                                          aux_6$p.value ) )

#Pruebas de homocedasticidad-----------------------------------------------------------------------

"MarchTest" <- function (zt, lag = 10) 
{
  if (!is.matrix(zt)) 
    zt = as.matrix(zt)
  nT = dim(zt)[1]
  k = dim(zt)[2]
  C0 = cov(zt)
  zt1 = scale(zt, center = TRUE, scale = FALSE)
  C0iv = solve(C0)
  wk = zt1 %*% C0iv
  wk = wk * zt1
  rt2 = apply(wk, 1, sum) - k
  m1 = acf(rt2, lag.max = lag, plot = F)
  acf = m1$acf[2:(lag + 1)]
  c1 = c(1:lag)
  deno = rep(nT, lag) - c1
  Q = sum(acf^2/deno) * nT * (nT + 2)
  pv1 = 1 - pchisq(Q, lag)
  cat("Q(m) of squared series(LM test): ", "\n")
  cat("Test statistic: ", Q, " p-value: ", pv1, "\n")
  rk = rank(rt2)
  m2 = acf(rk, lag.max = lag, plot = F)
  acf = m2$acf[2:(lag + 1)]
  mu = -(rep(nT, lag) - c(1:lag))/(nT * (nT - 1))
  v1 = rep(5 * nT^4, lag) - (5 * c(1:lag) + 9) * nT^3 + 9 * 
    (c(1:lag) - 2) * nT^2 + 2 * c(1:lag) * (5 * c(1:lag) + 
                                              8) * nT + 16 * c(1:lag)^2
  v1 = v1/(5 * (nT - 1)^2 * nT^2 * (nT + 1))
  QR = sum((acf - mu)^2/v1)
  pv2 = 1 - pchisq(QR, lag)
  cat("Rank-based Test: ", "\n")
  cat("Test statistic: ", QR, " p-value: ", pv2, "\n")
  cat("Q_k(m) of squared series: ", "\n")
  x = zt^2
  g0 = var(x)
  ginv = solve(g0)
  qm = 0
  df = 0
  for (i in 1:lag) {
    x1 = x[(i + 1):nT, ]
    x2 = x[1:(nT - i), ]
    g = cov(x1, x2)
    g = g * (nT - i - 1)/(nT - 1)
    h = t(g) %*% ginv %*% g %*% ginv
    qm = qm + nT * nT * sum(diag(h))/(nT - i)
    df = df + k * k
  }
  pv3 = 1 - pchisq(qm, df)
  cat("Test statistic: ", qm, " p-value: ", pv3, "\n")
  cut1 = quantile(rt2, 0.95)
  idx = c(1:nT)[rt2 <= cut1]
  x = zt[idx, ]^2
  eT = length(idx)
  g0 = var(x)
  ginv = solve(g0)
  qm2 = 0
  df = 0
  for (i in 1:lag) {
    x1 = x[(i + 1):eT, ]
    x2 = x[1:(eT - i), ]
    g = cov(x1, x2)
    g = g * (eT - i - 1)/(eT - 1)
    h = t(g) %*% ginv %*% g %*% ginv
    qm2 = qm2 + eT * eT * sum(diag(h))/(eT - i)
    df = df + k * k
  }
  pv4 = 1 - pchisq(qm2, df)
  cat("Robust Test(5%) : ", qm, " p-value: ", pv4, "\n")
  MarchTest<-list( Q = Q, pv1 = pv1,
                   QR = QR, pv2 = pv2,
                   qm = qm, pv3 = pv3,
                   qm2 = qm2, pv4 = pv4 )
}

homocedasticidad <- MarchTest(residuos) #H0: Las desviaciones no tienen heterocedasticidad condicional | Si p>0.05 no se rechaza la H0

homocedasticidad <- data.frame( prueba = c('Test LM',
                                         'Test basado en rango',
                                         '$Q_{k}(m)$ de la serie al cuadrado',
                                         'Test robustez (5\\%)' ),
                            estadistico = c( homocedasticidad$Q,
                                             homocedasticidad$QR,
                                             homocedasticidad$qm,
                                             homocedasticidad$qm2 ),
                            p_valor = c( homocedasticidad$pv1,
                                         homocedasticidad$pv2,
                                         homocedasticidad$pv3,
                                         homocedasticidad$pv4 ) )

#Prueba de multicolinealidad------------------------------------------------------------------------
library(ppcor)
correlaciones <- pcor( data[2:15,], method = "kendall" )
ma_correlaciones <- data.frame( variable = c('$\\Delta$ PIB',
                                             '$\\Delta$ Tasa activa',
                                             '$\\Delta$ Tasa pasiva',
                                             '$\\Delta$ Salarios promedio',
                                             '$\\Delta$ SBU',
                                             '$\\Delta$ Inflación Promedio'), correlaciones$estimate )
ma_multi_p_valores <- data.frame( variable = c('$\\Delta$ PIB',
                                             '$\\Delta$ Tasa activa',
                                             '$\\Delta$ Tasa pasiva',
                                             '$\\Delta$ Salarios Promedio',
                                             '$\\Delta$ SBU',
                                             '$\\Delta$ Inflación Promedio'), correlaciones$p.value )
ma_multi_p_valores <- ma_multi_p_valores %>%
  mutate( diff_pib = if_else( diff_pib <= 0.05 & diff_pib > 0,
                              0.1,
                              diff_pib ),
          diff_tasa_activa = if_else( diff_tasa_activa <= 0.05 & diff_tasa_activa > 0,
                              0.1,
                              diff_tasa_activa ),
          diff_tasa_pasiva = if_else( diff_tasa_pasiva <= 0.05 & diff_tasa_pasiva > 0,
                              0.1,
                              diff_tasa_pasiva ),
          diff_sal_prom = if_else( diff_sal_prom <= 0.05 & diff_sal_prom > 0,
                              0.1,
                              diff_sal_prom ),
          diff_sbu = if_else( diff_sbu <= 0.05 & diff_sbu > 0,
                              0.1,
                              diff_sbu ),
          diff_inflacion = if_else( diff_inflacion <= 0.05 & diff_inflacion > 0,
                              0.1,
                              diff_inflacion )      )
#Predicciones del modelo----------------------------------------------------------------------------
#Predicción hasta 12/2028 por datos perdidos
data[is.na(data)]<-0

for (i in seq(17,17,1)) {
  aux <- Vpmiss(data, m2$Phi, m2$Sigma, tmiss=i, c(1,0,0,0,0,1), m2$cnst)
  data[i,2] <- aux[1]
  data[i,3] <- aux[2]
  data[i,4] <- (aux[3])
}

for (i in seq(18,22,1)) {
  aux <- Vpmiss(data, m2$Phi, m2$Sigma, tmiss=i, c(1,0,0,0,0,1), m2$cnst)
  data[i,2] <- aux[1]
  data[i,3] <- aux[2]
  data[i,4] <- abs(aux[3])
  data[i,5] <- abs(aux[4])
}

#Predicciones de 2029  a 2062-----------------------------------------------------------------------
b<-as.matrix(data[c(6:22),])
m2$data<-as.matrix(data[c(6:22),])
pred <- VARpred(m2, h = 34, orig = 0, output = TRUE)
data <-rbind( data, pred$pred)

fechas <- crossing(anio = 2006:2062)
aux <- as.data.frame(rbind(c(NA),data))
pred <- cbind(fechas, aux)


#Transformación a la serie original-----------------------------------------------------------------
aux <- c(pred$diff_pib)
aux <- diffinv(aux[-1]*pi, lag = 1, xi=c(46802044))
pred['pib'] <- aux


aux <- c(pred$diff_tasa_activa)
aux <- diffinv(aux[-1]*a, lag = 1, xi=c(0.08845000))
pred['ta'] <- aux


aux <- c(pred$diff_tasa_pasiva)
aux <- diffinv(aux[-1]*p, lag = 1, xi=c(0.04348333))
pred['tp'] <- aux


aux <- c(pred$diff_sal_prom)
aux <- diffinv(aux[-1]*s, lag = 1, xi=c(4040.464))
pred['sal'] <- aux


aux <- c(pred$diff_sbu)
aux <- diffinv( aux[-c(1)] * su, lag = 1, differences = 1,  xi=c( 160 ) )
pred['sbu'] <- aux

aux <- c(pred$diff_inflacion)
aux <- diffinv(aux[-1]*i1, lag = 1, xi=c(0.033008495))
pred['inf'] <- aux


#Calculo de las tasas de crecimiento promedio-------------------------------------------------------
aux <-pred %>%
  dplyr::distinct(anio,.keep_all = TRUE) %>%
  dplyr::mutate( t_pib = (pib - dplyr::lag(pib))/dplyr::lag(pib) ) %>%
  dplyr::mutate(t_sal = (sal - dplyr::lag(sal))/dplyr::lag(sal) ) %>%
  dplyr::mutate(t_sbu = (sbu - dplyr::lag(sbu))/dplyr::lag(sbu) ) %>%
  dplyr::select( anio, t_pib, t_sal, t_sbu, ta, tp, inf) %>%
  dplyr::filter(anio>=2023, anio<=2062)

(t_pib = mean(aux$t_pib, na.rm = TRUE))
(t_sal = mean(aux$t_sal, na.rm = TRUE))
(t_sbu = mean(aux$t_sbu, na.rm = TRUE))
(t_ta = mean(aux$ta, na.rm = TRUE))
(t_tp = mean(aux$tp, na.rm = TRUE))
(t_inf = mean(aux$inf, na.rm = TRUE))

tasas_macro_crec <- aux

#Tabla resumen de parámetros------------------------------------------------------------------------

Hipotesis <- tasas_macro_crec %>%
  dplyr::filter(anio>=2023, anio<=2062) %>%
  dplyr::mutate(t_pib = 100 * mean( t_pib,   na.rm = TRUE),
                ta = 100 * mean( ta,   na.rm = TRUE),
                tp = 100 * mean( tp,   na.rm = TRUE),
                t_sal = 100 * mean( t_sal,   na.rm = TRUE),
                t_sbu = 100 * mean( t_sbu,   na.rm = TRUE),
                inf = 100 * mean( inf,   na.rm = TRUE) ) %>%
  distinct( t_pib, .keep_all = TRUE) %>%
  dplyr::select( t_pib, ta, tp, t_sal, t_sbu, inf ) %>%
  gather(., key = 'hipotesis', value = 'tasas') %>%
  mutate( hipotesis = c('Crecimiento del PIB (a precios actuales)',
                        'Tasa Activa Referencial',
                        'Tasa Pasiva Referencial',
                        'Crecimiento Salarial',
                        'Crecimiento del SBU',
                        'Inflación Promedio Acumulada'))

lista = list( Hipotesis = Hipotesis,
             coeficientes = coeficientes,
             shapiro_test = shapiro_test,
             homocedasticidad = homocedasticidad,
             ma_correlaciones = ma_correlaciones,
             predicciones = pred,
             covarianza = covarianza,
             test_nulidad_nulidad = test_nulidad_nulidad,
             box_ljung = box_ljung,
             ma_multi_p_valores = ma_multi_p_valores )
return( lista )
}


#---------------------------------------------------------------------------------------------------
th = 1.96
#Calibración modelo---------------------------------------------------------------------------------
# c_i = 1
# c_pib = 0.8
# c_a = 0.8
# c_p = 0.1
# c_s = 0.85
# c_su = 0.7
#---------------------------------------------------------------------------------------------------
#  1.539198 11.740603  5.570311  1.593252  2.594404  1.552025
c_i = 1
c_pib = 0.4624148
c_a = 1.44919
c_p = -0.141776
c_s = 1
c_su = 0.5

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
#  1.575492 8.587327 5.926767 1.587014 2.797914 1.552025
c_i = 1
c_pib = 0.8
c_a = 0.8
c_p = 0.1
c_s = 1
c_su = 0.7

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas
#---------------------------------------------------------------------------------------------------
# 2.019144 9.153392 6.034336 2.314659 3.770772 1.552025
c_i = 1
c_pib = 0.5
c_a = 0.8
c_p = 0.1
c_s = 1
c_su = 0.7

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
# 1.418709 13.420878  5.919735  1.332374  1.355884  1.552025
c_i = 1
c_pib = 0.8
c_a = 0.8
c_p = 0.1
c_s = 0.9
c_su = 0.1

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
#  1.435170 8.329428 5.891385 1.343333 2.350605 1.552025
c_i = 1
c_pib = 1.2
c_a = 0.75
c_p = 0.1
c_s = 1.08
c_su = 0.65

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
# 1.445563 8.389744 5.891635 1.359389 2.279574 1.552025
c_i = 1
c_pib = 1.2
c_a = 0.75
c_p = 0.1
c_s = 1.088
c_su = 0.60

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas
#---------------------------------------------------------------------------------------------------
# 1.435170 8.329428 5.891385 1.343333 2.350605 1.552025
c_i = 1
c_pib = 1.2
c_a = 0.75
c_p = 0.1
c_s = 1.08
c_su = 0.65

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )

a$Hipotesis$tasas
#---------------------------------------------------------------------------------------------------
# 2.054509 11.243145  6.100424  2.378580  2.836040  1.552025
c_i = 1
c_pib = 0.4
c_a = 0.75
c_p = 0.1
c_s = 0.88
c_su = 0.3

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
# 2.054509 11.243145  6.100424  2.378580  2.836040  1.552025
c_i = 1
c_pib = 0.4
c_a = 0.75
c_p = 0.1
c_s = 0.88
c_su = 0.3

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas



#---------------------------------------------------------------------------------------------------
# 2.054509 11.243145  6.100424  2.378580  2.836040  1.552025
c_i = 1
c_pib = 1.05
c_a = 075
c_p = 0.01
c_s = 0.88
c_su = 0.5

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
#---------------------------------------------------------------------------------------------------
# 1.558945 8.451947 5.904017 1.536589 2.554934 1.552025
c_i = 1
c_pib = 1.02
c_a = 0.75
c_p = 0.1
c_s = 1.082
c_su = 0.65

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas


#---------------------------------------------------------------------------------------------------
# 1.704602 8.638994 5.929932 1.787639 2.837162 1.552025
c_i = 1
c_pib = 0.8
c_a = 0.75
c_p = 0.1
c_s = 1.045
c_su = 0.65

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
# 1.811658 8.763022 5.949114 1.964901 3.035517 1.552025
c_i = 1
c_pib = 0.71
c_a = 0.75
c_p = 0.1
c_s = 1.043
c_su = 0.65

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas



#---------------------------------------------------------------------------------------------------
# 1.836717 8.741008 5.951001 2.002991 3.060207 1.552025
c_i = 1
c_pib = 0.70
c_a = 0.73
c_p = 0.1
c_s = 1.035
c_su = 0.65

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas


#---------------------------------------------------------------------------------------------------
# 2.015777 9.059276 6.002122 2.295624 3.333230 1.552025
c_i = 1
c_pib = 0.55
c_a = 0.715
c_p = 0.1
c_s = 0.99
c_su = 0.6

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas



#---------------------------------------------------------------------------------------------------
# 2.071420 8.991351 6.002122 2.377125 3.371124 1.552025
c_i = 1
c_pib = 0.55
c_a = 0.68
c_p = 0.1
c_s = 0.99
c_su = 0.6

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas


#---------------------------------------------------------------------------------------------------
#  1.923020 9.376457 6.008424 2.154617 2.859400 1.552025
c_i = 1
c_pib = 0.51
c_a = 0.68
c_p = 0.1
c_s = 0.89
c_su = 0.45

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
# 1.916095 9.092155 6.002832 2.136516 2.799816 1.552025
c_i = 1
c_pib = 0.50
c_a = 0.60
c_p = 0.1
c_s = 0.825
c_su = 0.45

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas


#---------------------------------------------------------------------------------------------------
#  1.821843 9.025233 5.995991 1.990138 2.684339 1.552025
c_i = 1
c_pib = 0.50
c_a = 0.60
c_p = 0.1
c_s = 0.787
c_su = 0.45

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas


#---------------------------------------------------------------------------------------------------
#  1.829237 9.163087 5.996531 2.001673 2.601463 1.552025
c_i = 1
c_pib = 0.50
c_a = 0.60
c_p = 0.1
c_s = 0.79
c_su = 0.41

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
a$coeficientes

#---------------------------------------------------------------------------------------------------
#  2.278416 8.665947 5.996531 2.654520 2.964419 1.552025
c_i = 1
c_pib = 0.50
c_a = 0.60
c_p = 0.1
c_s = 0.79
c_su = 0.41

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
a$coeficientes

#---------------------------------------------------------------------------------------------------
#  1.755695 9.105100 5.991131 1.886527 2.512065 1.552025
c_i = 1
c_pib = 0.50
c_a = 0.60
c_p = 0.1
c_s = 0.76
c_su = 0.41

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
a$coeficientes

#---------------------------------------------------------------------------------------------------
#  2.278416 8.665947 5.996531 2.654520 2.964419 1.552025
c_i = 1
c_pib = 0.50
c_a = 0.4
c_p = 0.1
c_s = 0.79
c_su = 0.41

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas

#---------------------------------------------------------------------------------------------------
# 2.099595 8.698961 5.991131 2.391177 2.790082 1.552025

c_i = 1
c_pib = 0.50
c_a = 0.43
c_p = 0.1
c_s = 0.76
c_su = 0.41

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas


#---------------------------------------------------------------------------------------------------
#   1.959226 8.633854 5.982670 2.176857 2.630504 1.552025

c_i = 1
c_pib = 0.50
c_a = 0.43
c_p = 0.1
c_s = 0.713
c_su = 0.41

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
a$coeficientes

#---------------------------------------------------------------------------------------------------
#   1.959226 8.867112 5.982670 2.176857 2.464207 1.552025

c_i = 1
c_pib = 0.50
c_a = 0.43
c_p = 0.1
c_s = 0.713
c_su = 0.33

a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
a$coeficientes

#---------------------------------------------------------------------------------------------------
#   1.959226 8.633854 5.982670 2.176857 2.630504 1.552025

c_i = 1
c_pib = 0.50
c_a = 0.43
c_p = 0.1
c_s = 0.713
c_su = 0.41
th = 1.96
a <- macro( c_pib, c_a, c_p,  c_s, c_su, c_i, th )
a$Hipotesis$tasas
a$coeficientes
g <- a$predicciones %>%
  dplyr::select(pib,ta,tp,sal,inf, sbu)

autoplot(ts(g$pib,
            start = c(2006,1),
            frequency = 1), type = "l", ylab="pib") #+ ggtitle("Time Series ")

autoplot(ts(g$sal,
            start = c(2006,1),
            frequency = 1) , type = "l", ylab="salario anual") #+ ggtitle("Time Series ")

autoplot(ts(g$sbu,
            start = c(2006,1),
            frequency = 1) , type = "l",ylab = "sbu") #+ ggtitle("Time Series ")

autoplot(ts(g$ta,
            start = c(2006,1),
            frequency = 1), type = "l", ylab = "tasa activa") #+ ggtitle("Time Series ")

autoplot(ts(g$tp,
            start = c(2006,1),
            frequency = 1), type = "l", ylab = "tasa pasiva") #+ ggtitle("Time Series ")

autoplot(ts(g$inf,
            start = c(2006,1),
            frequency = 1), type = "l", ylab = "inflación promedio" ) #+ ggtitle("Time Series ")


#Asignando nombres a variables----------------------------------------------------------------------

tasas_macro_pred <- a$predicciones %>%
  dplyr::select(anio,
                pib_nominal:= pib,
                tasa_activa:= ta,
                tasa_pasiva:= tp,
                sal_anual:=sal,
                sbu,
                inflación_prom:=inf)


Hipotesis <- a$Hipotesis

coeficientes <- a$coeficientes

shapiro_test <- a$shapiro_test

homocedasticidad <- a$homocedasticidad

ma_correlaciones <- a$ma_correlaciones

covarianza <- a$covarianza

test_nulidad_nulidad <- a$test_nulidad_nulidad

box_ljung <- a$box_ljung

ma_multi_p_valores <- a$ma_multi_p_valores

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando tasas macro' )

save( tasas_macro_pred,
      Hipotesis,
      coeficientes,
      shapiro_test,
      homocedasticidad,
      ma_correlaciones,
      covarianza,
      test_nulidad_nulidad,
      box_ljung,
      ma_multi_p_valores,
      file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
# 
# #Exportar a excel-----------------------------------------------------------------------------------
write.xlsx(tasas_macro_pred,
           file = paste0( parametros$resultado_seguro , 'IESS_macro_estudio.xlsx' ),
           sheetName = "hipotesis_predicciones",
           col.names = TRUE,
           row.names = FALSE,
           append = FALSE)
# 
# # Borrar elementos restantes -----------------------------------------------------------------------
# message(paste(rep("-", 100), collapse = ""))
# rm(list = ls()[!(ls() %in% c("parametros"))])
# gc()