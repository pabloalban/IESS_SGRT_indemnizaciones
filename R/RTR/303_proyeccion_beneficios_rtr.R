message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGenerando proyección de beneficios' )
load( paste0( parametros$RData, 'IESS_proyeccion_salarios_escenario_1.Rdata' ) )
load( paste0( parametros$RData, 'IESS_salarios_pensiones_iniciales_v3.RData' ) )
load( paste0( parametros$RData, 'IESS_probabilidades_transicion_edad_tiempo_servicio.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_pensiones_PA_PT_ajustadas_ini.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_pensiones_PP_ajustadas_ini.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_proy_beneficarios_prestacion.RData' ) )
load(paste0( parametros$RData_seg, 'IESS_RTR_porc_incap_indemn_edad_sexo_int.RData' ) )
load(paste0( parametros$RData_seg, 'IESS_RTR_duracion_subsidios_sexo_edad_int.RData' ) )
load(paste0( parametros$RData_seg, 'IESS_RTR_pensiones_VO_ajustadas_ini.RData' ) )
load(paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros', 'pen_afi', 'sal_afi', 'sal_proy',
                                 'pob_proy','pensiones_PA_PT_ini_int_rtr','pensiones_PP_ini_int_rtr',
                                 'porc_incap_indemn_edad_sexo_int', 'duracion_subsidios_sexo_edad_int',
                                 'porc_incap_subsidios_edad_sexo_int',
                                 'cot_proy', 'prob_tot', 'Pxs', 'sbu_proy', 'coef_pen_mean',
                                 'pensiones_MV_rtr_int', 'pensiones_MO_rtr_int','rho','r') ) ] )

# Horizonte de proyección
t_max <- parametros$horizonte
t<- 0: t_max

# Edades
x_max <- parametros$edad_max
x <- 0:x_max

# Sexos
sexo<-c('F','M')

# Proyección pensiones de PA y PT de RTR------------------------------------------------------------
message( '\tGenerando proyección de pensiones de PA y PT de RTR' )

pen_proy <- as.data.table(expand.grid(t=t, sexo=sexo ,x=x,stringsAsFactors = FALSE))
pen_proy<-pen_proy[,i_p := esc$hip_esc$i_p[1]]
pen_proy<-merge(pen_proy,sal_proy[,list(t,sexo,x,sal_mes)],by=c("t","sexo","x"),all.x = TRUE)
pen_proy<-merge(pen_proy,sbu_proy[,list(t,sbu)],by=c("t"),all.x = TRUE)

#Añadiendo número de pensionistas de RTR------------------------------------------------------------
pen_proy <- merge( pen_proy, 
                   pob_proy[,-c(4:27) ], 
                   by = c( 'sexo', 'x','t' ), all.x = TRUE )

message( '\tAñadiendo pensiones iniciales suavizadas de RTR' )
#Añadiendo las pensiones iniciales ajustadas de RTR-------------------------------------------------
pensiones_PA_PT_ini_int_rtr<-as.data.table(pensiones_PA_PT_ini_int_rtr)[,list(x=edad,
                                                                              pen_pa_pt_ini=pension_pa_pt_ini_int)]
pen_proy <- merge( pen_proy, 
                   pensiones_PA_PT_ini_int_rtr, 
                   by = c( 'x' ), all.x = TRUE )

pensiones_PP_ini_int_rtr<-as.data.table(pensiones_PP_ini_int_rtr)[,list(x=edad,pension_pp_ini=pension_pp_ini_int)]


pen_proy <- merge( pen_proy, 
                   pensiones_PP_ini_int_rtr, 
                   by = c( 'x' ), all.x = TRUE )

#Añadiendo las pensiones iniciales ajustadas de orfandad y viudedad de RTR--------------------------
pensiones_MO_rtr_int<-as.data.table(pensiones_MO_rtr_int)[,list(x,pension_mo_ini=pension_mo_ini_int)]
pen_proy <- merge( pen_proy, 
                   pensiones_MO_rtr_int, 
                   by = c( 'x' ), all.x = TRUE )

pensiones_MV_rtr_int<-as.data.table(pensiones_MV_rtr_int)[,list(x,pension_mv_ini=pension_mv_ini_int)]


pen_proy <- merge( pen_proy, 
                   pensiones_MV_rtr_int, 
                   by = c( 'x' ), all.x = TRUE )


#Añadiendo porcentaje de incapacidad en Indemnizaciones---------------------------------------------
porc_incap_indemn_edad_sexo_int<- as.data.table(porc_incap_indemn_edad_sexo_int)
porc_incap_indemn_edad_sexo_int <- porc_incap_indemn_edad_sexo_int[,list(x=edad_siniestro,
                                                                         sexo,
                                                                         porc_incap_indemn=porc_incap_indemn_int)]
pen_proy <- merge( pen_proy, 
                   porc_incap_indemn_edad_sexo_int, 
                   by = c( 'x','sexo' ), all.x = TRUE )
#Añadiendo porcentaje de incapacidad de los Subsidios-----------------------------------------------
duracion_subsidios_sexo_edad_int<-as.data.table(duracion_subsidios_sexo_edad_int)
duracion_subsidios_sexo_edad_int<-duracion_subsidios_sexo_edad_int[,list(x=edad,
                                                                         sexo,
                                                                         duracion_sub=pro_duracion_dias_int)]
pen_proy <- merge( pen_proy, 
                   duracion_subsidios_sexo_edad_int, 
                   by = c( 'x','sexo' ), all.x = TRUE )

#Añadiendo porcentaje de duración (en años) de los Subsidios----------------------------------------
porc_incap_subsidios_edad_sexo_int<-as.data.table(porc_incap_subsidios_edad_sexo_int)
porc_incap_subsidios_edad_sexo_int<-porc_incap_subsidios_edad_sexo_int[,list(x=edad, sexo,
                                                                             porc_cobro_sub=porc_subsidios_prom_int)]

pen_proy <- merge( pen_proy, 
                   porc_incap_subsidios_edad_sexo_int, 
                   by = c( 'x','sexo' ), all.x = TRUE )

message( '\tCalculo de prestaciones de RTR' )
#Calculo de las pensiones de PA y PT de RTR---------------------------------------------------------
pen_proy[ , pen_pa_pt := 12 * pen_pa_pt_ini * ( 1 + i_p )^t ]
pen_proy[ , pen_pa_pt := pmax( pen_pa_pt, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_pa_pt := pmin( pen_pa_pt, 4.5 * 12 * sbu ) ]
pen_proy[ , pen_pa_pt := pen_pa_pt + pen_pa_pt / 12 + sbu ]
pen_proy[ x < 48, pen_pa_pt := 0 ]

#Calculo de las pensiones de PP de RTR--------------------------------------------------------------
pen_proy[ , pen_pp := 12 * pension_pp_ini * ( 1 + i_p )^t ]
pen_proy[ , pen_pp := pmax( pen_pp, 0.5 * 12 * sbu ) ]
pen_proy[ , pen_pp := pmin( pen_pp, 2.5 * 12 * sbu ) ]
pen_proy[ , pen_pp := pen_pp + pen_pp / 12 + sbu ]
pen_proy[ x < 48, pen_pp := 0 ]

#Calculo de las pensiones de orfandad de RTR--------------------------------------------------------
#rho:= número de causantes con hijos menores / huerfanos beneficiarios
pen_proy[ , pen_mo := 12 * pension_mo_ini * ( 1 + i_p )^t ]
pen_proy[ , pen_mo := pmax( pen_mo, 0.6 * 0.4 * rho * 12 * sbu ) ] #suponiendo que el causante trabajó 20 años
pen_proy[ , pen_mo := pmin( pen_mo, 4.5* rho * 12 * sbu ) ]
pen_proy[ , pen_mo := pen_mo + pen_mo / 12 + 0.4 * rho * sbu ]
pen_proy[ x > 17, pen_mo := 0 ]


#Calculo de las pensiones de viudedad de RTR--------------------------------------------------------
pen_proy[ , pen_mv := 12 * pension_mv_ini * ( 1 + i_p )^t ]
pen_proy[ , pen_mv := pmax( pen_mv, 0.6 * 12 * sbu ) ] #suponiendo que el causante trabajó 20 años
pen_proy[ , pen_mv := pmin( pen_mv, 4.5 * 12 * sbu ) ]
pen_proy[ , pen_mv := pen_mv + pen_mv / 12 + sbu ]

#Calculo de pensiones de incapacidad permanente parcial---------------------------------------------
#No se calcula, por se despresiable (USD 150.203,08 en 2018)

#Calculo  de Indemnizaciones de RTR-----------------------------------------------------------------
#r es la proporción entre salario del beneficiario de Indemnización y los cotizantes
pen_proy[ , pen_indemn := 60 * r  *sal_mes * porc_incap_indemn ]
pen_proy[ , pen_indemn := pmin( pen_indemn, 100 * sbu ) ]
pen_proy[ x < 18, pen_indemn := 0 ]

#Calculo de subsidios de RTR------------------------------------------------------------------------
pen_proy[ , pen_sub := porc_cobro_sub * duracion_sub * sal_mes / 30 ]
pen_proy[ x < 18, pen_sub := 0 ]


setorder( pen_proy, t, sexo, x )

#Seleccionar solo las pensiones por edad y sexo de RTR----------------------------------------------
pen_proy <- pen_proy[ , list( t, sexo, x, i_p, sal_mes, sbu, pen_pa_pt,
                              pen_pp, pen_mo, pen_mv, pen_indemn, pen_sub ) ]

ben_proy<-pen_proy

#Guaradar en Rdata----------------------------------------------------------------------------------
message( '\tGuardando en Rdata la proyección de beneficios de RTR' )
save( ben_proy,
      file = paste0( parametros$RData_seg, 'IESS_RTR_proyeccion_beneficios.RData' ) )

#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()
