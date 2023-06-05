message( paste( rep( '-', 100 ), collapse = '' ) )

# Descripción estados
# 1 = Individuos en la PEA no activos
# 2 = Activos
# 2_cot = Activos cotizantes
# 2_ces = Activos cesantes
# 3 = Pensionistas de vejez
# 4 = Pensionistas de invalidez
# 5 = Muertos ( no causados por accidente de trabajo )
# 6 = Pensionistas de montepío
# 7 = Hijos de cotizantes
# 8 = Cónjuges no asegurados de cotizantes

# estados auxiliares para medir los flujos de beneficios:
# 9 = Pensionistas de invalidez por riesgos del trabajo
# 10 = Indemnizaciones
# 11 = Subsidios
# 12 = Accidentes fatales ( causados por accidente de trabajo )
# 13 = Montepíos de orfandad de riesgos del trabajo
# 14 = Montepíos de viudedad de riesgos del trabajo
# 15 = Pensionistas por incapacidad permanente parcial

# Cargando información -----------------------------------------------------------------------------
# Bases globales para todos los seguros
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )
load( paste0( parametros$RData, 'IESS_probabilidades_transicion.RData' ) )
load( paste0( parametros$RData, 'IESS_tabla_mortalidad_dinamica.RData' ) )

setnames( pob_proy, 
          c( 't', 'sexo', 'x', 'l1', 'l2', 'l3', 'l4', 'l5', 'l1_1', 'l1_2', 'l1_5', 'l2_2', 'l2_3', 
             'l2_4', 'l2_5', 'l3_3', 'l3_5', 'l4_4', 'l4_5', 'tau', 'l2_cot', 'l2_ces', 'l6', 'l7', 'l8' ) )

# Bases propias de RTR
load( paste0( parametros$RData_seg, "IESS_RTR_siniestralidad_indemnizaciones_edad_sexo_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_RTR_siniestralidad_subsidios_edad_sexo_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_RTR_siniestralidad_accidentes_laborales_fatales.RData" ) )
load( paste0( parametros$RData_seg, "IESS_RTR_grupo_familiar.RData" ) )
load( paste0( parametros$RData_seg, "IESS_RTR_fdp_ingresos_huerfanos_montepio.RData" ) )
load( paste0( parametros$RData_seg, "IESS_RTR_fdp_ingresos_viudas_montepio.RData" ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_poblacion_inicial.RData' ) )

# Tasas --------------------------------------------------------------------------------------------
message( '\tPreparando información para proyecciones' )
# Preparando tablas de siniestralidad de indemnizaciones
sin_ind <- siniestralidad_indeminizaciones_edad_sexo_int[, list( x, sexo, tas_ind = tasa_sin_indem_int ) ]
# plot( sin_ind$tasa_sin_indem_int )

# Preparando tablas de siniestralidad de subsidios
sin_sub <- siniestralidad_subsidios_edad_sexo_int[, list( x, sexo, tas_sub = tasa_sin_sub_int ) ]
# plot( sin_sub$tasa_sin_sub_int )

# Accidentes laborales fatales
sin_fat <- incidencia_FA[, list( x = edad, tas_fat = exp( log_tasa_inc_FA_int ) ) ]
sin_fat <- rbind( sin_fat[ , list( x, sexo = 'F', tas_fat = 0.15 * tas_fat ) ], 
                  sin_fat[ , list( x, sexo = 'M', tas_fat = 5.00 * tas_fat ) ] )
# plot( sin_fat[ , list( x, tas_fat ) ] )

sin_cau <- merge( sin_ind, sin_sub, by = c( 'sexo', 'x' ) )
sin_cau <- merge( sin_cau, sin_fat, by = c( 'sexo', 'x' ), all.x = TRUE )
sin_cau[ is.na( tas_fat ), tas_fat := 0 ]
setcolorder( sin_cau, c( 'sexo', 'x', 'tas_ind', 'tas_sub', 'tas_fat' ) )
setorder( sin_cau, sexo, x )

# Población que debe evolucionar según las tasas de mortalidad
# Horizonte de proyección
t_horiz <- parametros$horizonte

# Año inicial de proyección
fec_ini <- parametros$anio_ini

# Año final de proyección
fec_fin <- fec_ini + t_horiz

# Tiempo
t <- 0:t_horiz

# Edades
x_max <- parametros$edad_max
x <- 0:x_max

N <- length( t )
M <- length( x )

# Población inicial de pensionistas por incapacidad permanente parcial
pop_ini_pp <- pob_ini_rtr[ estado == "PP", list( x, sexo, lx ) ] # no está alisado

# Alisamiento población inicial
mod_ini_pp_f <- lm( log( lx ) ~ bs( x, df = 15, degree = 3, Boundary.knots = range( pop_ini_pp$x ) ), 
                    data = pop_ini_pp[ sexo == 'F' & is.finite( log( lx ) ) & x >= 20 & x <= 95 ] )
pop_ini_ali_f <- data.table( x = x )
pop_ini_ali_f[ , log_lx := predict( object = mod_ini_pp_f, newdata = pop_ini_ali_f ) ]
pop_ini_ali_f[ , lx := exp( log_lx ) ]
pop_ini_ali_f[ x > 95 | x < 20, lx := 0 ]
pop_ini_ali_f[ , sexo := 'F' ]

# plot( pop_ini_ali_f$x, pop_ini_ali_f$lx )

mod_ini_pp_m <- lm( log( lx ) ~ bs( x, df = 15, degree = 3, Boundary.knots = range( pop_ini_pp$x ) ), 
                    data = pop_ini_pp[ sexo == 'M' & is.finite( log( lx ) ) & x >= 20 & x <= 95 ] )
pop_ini_ali_m <- data.table( x = x )
pop_ini_ali_m[ , log_lx := predict( object = mod_ini_pp_m, newdata = pop_ini_ali_m ) ]
pop_ini_ali_m[ , lx := exp( log_lx ) ]
pop_ini_ali_m[ x > 95 | x < 20, lx := 0 ]
pop_ini_ali_m[ , sexo := 'M' ]

# plot( pop_ini_ali_m$x, pop_ini_ali_m$lx )

pop_ini_ali_pp <- rbind( pop_ini_ali_f, pop_ini_ali_m )

l0_f <- pop_ini_ali_pp[ sexo == 'F' ]
setorder( l0_f, x )
l0_f <- as.matrix( l0_f[ , list( lx ) ] )

l0_m <- pop_ini_ali_pp[ sexo == 'M' ]
setorder( l0_m, x )
l0_m <- as.matrix( l0_m[ , list( lx ) ] )

pop_ini_orf <- pob_ini_rtr[ estado == 'MO', list( x, sexo, lx ) ]
l0_orf_f <- pop_ini_orf[ sexo == 'F' ]
setorder( l0_orf_f, x )
l0_orf_f <- as.matrix( l0_orf_f[ , list( lx ) ] )

l0_orf_m <- pop_ini_orf[ sexo == 'M' ]
setorder( l0_orf_m, x )
l0_orf_m <- as.matrix( l0_orf_m[ , list( lx ) ] )

pop_ini_viu <- pob_ini_rtr[ estado == 'MV', list( x, sexo, lx ) ]
l0_viu_f <- pop_ini_viu[ sexo == 'F' ]
setorder( l0_viu_f, x )
l0_viu_f <- as.matrix( l0_viu_f[ , list( lx ) ] )

l0_viu_m <- pop_ini_viu[ sexo == 'M' ]
setorder( l0_viu_m, x )
l0_viu_m <- as.matrix( l0_viu_m[ , list( lx ) ] )

# Tasa mortalidad inválidos ------------------------------------------------------------------------
message( '\tPreparando tasa de mortalidad de pensionistas por invalidez' )
pi_f <- iess_mort_din[ sexo == 'F' & t >= fec_ini & t <= fec_fin & x <= x_max, 
                       list( t = t - fec_ini, x, pix ) ]
setorder( pi_f, t, x )
pi_f <- dcast.data.table( data = pi_f, x ~ t, value.var = 'pix' )
pi_f <- as.matrix( pi_f[ , 2:ncol( pi_f ) ] )

pi_m <- iess_mort_din[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max, 
                       list( t = t - fec_ini, x, pix ) ]
setorder( pi_m, t, x )
pi_m <- dcast.data.table( data = pi_m, x ~ t, value.var = 'pix' )
pi_m <- as.matrix( pi_m[ , 2:ncol( pi_m ) ] )

# Tasa de mortalidad de no afiliados ---------------------------------------------------------------
message( '\tPreparando tasa de mortalidad de montepios' )
p_f <- iess_mort_din[ sexo == 'F' & t >= fec_ini & t <= fec_fin & x <= x_max, 
                      list( t = t - fec_ini, x, px ) ]
setorder( p_f, t, x )
p_f <- dcast.data.table( data = p_f, x ~ t, value.var = 'px' )
p_f <- as.matrix( p_f[ , 2:ncol( p_f ) ] )

p_m <- iess_mort_din[ sexo == 'M' & t >= fec_ini & t <= fec_fin & x <= x_max, 
                      list( t = t - fec_ini, x, px ) ]
setorder( p_m, t, x )
p_m <- dcast.data.table( data = p_m, x ~ t, value.var = 'px' )
p_m <- as.matrix( p_m[ , 2:ncol( p_m ) ] )

# Estimando indemnizaciones, subsidios y accidentes fatales ----------------------------------------
pob_proy <- merge( pob_proy, sin_cau, by = c( "x", "sexo" ), all.x = TRUE )
pob_proy <- pob_proy[ is.na( tas_ind ), tas_ind := 0 ]
pob_proy <- pob_proy[ is.na( tas_sub ), tas_sub := 0 ]
pob_proy <- pob_proy[ is.na( tas_fat ), tas_fat := 0 ]

# Proyección de pensionistas de rentas vitalicias por incapacidad permanente parcial
pob_proy[ sexo == 'F', l9 := 0.015 * l4 ]
pob_proy[ sexo == 'M', l9 := 0.055 * l4 ]
pob_proy[ sexo == 'F', l9_5 := 0.015 * l4_5 ]
pob_proy[ sexo == 'M', l9_5 := 0.055 * l4_5 ]

message( '\tProyección de indemnizaciones de incapacidad permanente' )
pob_proy[, l10 := tas_ind * l2_cot ] # Indemnizaciones de incapacidad permanente parcial

message( '\tProyección de subsidios' )
pob_proy[, l11 := tas_sub * l2_cot ] # Subsidios

message( '\tProyección de accidentes laborales fatales' )
pob_proy[, l12 := tas_fat * l2_cot ] # Accidentes laborales fatales

# Se debe mantener el principio de conservación, si alguién entra en un estado salio de algún otro
# Se puede evitar restar de los cotizantes si cada individuo que sale es inmediatamente reemplazado
# por un cesante o una persona en la PEA, pero si se supone esto habrá que afectar en cambio estas 
# poblaciones.
pob_proy[, l1 := l1 - 0.3 * ( l10 + l11 + l12 ) ]
pob_proy[ l1 < 0, l1 := 0 ]
pob_proy[, l2_ces := l2_ces - 0.7 * ( l10 + l11 + l12 ) ]
pob_proy[ l2_ces < 0, l2_ces := 0 ]
pob_proy[, tas_ind := NULL ]
pob_proy[, tas_sub := NULL ]
pob_proy[, tas_fat := NULL ]

# Proyección de montepios por orfandad y viudez ----------------------------------------------------
message( '\tCargando estructura familiar' )
load( paste0( parametros$RData, 'INEC_censo_iess_fertilidad_alisado_2010.RData' ) )

message( '\tProyección montepíos por orfandad' )
aux <- cen_iess_hij_alis[ , list( x, z = y, sexo, sexo_dep, qh = q ) ]
aux[ , sexo := as.character( sexo ) ]
aux[ , sexo_dep := as.character( sexo_dep ) ]
aux[ sexo == 'M', sexo := 'F' ]
aux[ sexo == 'H', sexo := 'M' ]
aux[ sexo_dep == 'M', sexo_dep := 'F' ]
aux[ sexo_dep == 'H', sexo_dep := 'M' ]

pob_proy_dep <- merge( pob_proy[ , list( sexo, x, t, l12, l9_5 ) ], 
                       aux, 
                       by = c( 'sexo', 'x' ), 
                       all.x = TRUE, allow.cartesian = TRUE )

pob_proy_dep[ is.na( qh ), qh := 0 ]
pob_proy_dep <- pob_proy_dep[ !is.na( z ) ]

cal_orf <- 1.1
pob_proy_dep <- pob_proy_dep[ , list( l13 = cal_orf * sum( ( l12 + l9_5 ) * qh ) ),
                              by = list( t, sexo = sexo_dep, x = z ) ]
rm( cal_orf )
gc()

message( '\tProyección montepíos por viudedad' )
pob_proy_cony <- merge( pob_proy[ , list( sexo, x, t, l12, l9_5 ) ], 
                        cen_iess_cony_alis[ , list( x, y, qc = q ) ], by = c( 'x' ), 
                        all.x = TRUE, allow.cartesian = TRUE )

pob_proy_cony[ , aux := sexo ]
pob_proy_cony[ aux == 'M', sexo := 'F' ]
pob_proy_cony[ aux == 'F', sexo := 'M' ]
pob_proy_cony[ , aux := NULL ]
pob_proy_cony[ is.na( qc ), qc := 0 ]
pob_proy_cony <- pob_proy_cony[ !is.na( y ) ]
pob_proy_cony <- pob_proy_cony[ y <= 105, list( l14 = sum( ( l12 + l9_5 ) * qc ) ), 
                                by = list( t, sexo, x = y ) ]
cal_viu_f <- 1.6
cal_viu_m <- 1.2
pob_proy_cony[ sexo == 'F', l14 := cal_viu_f * l14 ]
pob_proy_cony[ sexo == 'M', l14 := cal_viu_m * l14 ]
rm( cal_viu_f, cal_viu_m )
pob_proy <- merge( pob_proy, pob_proy_dep, by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy <- merge( pob_proy, pob_proy_cony, by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy[ is.na( l13 ), l13 := 0 ]
pob_proy[ is.na( l14 ), l14 := 0 ]

l_pp_f <- matrix( 0, M, N )
l_pp_m <- matrix( 0, M, N )

l_pp_f[ , 1 ] <- l0_f
l_pp_m[ , 1 ] <- l0_m

l_orf_f <- matrix( 0, M, N )
l_orf_m <- matrix( 0, M, N )

l_orf_f <- pob_proy[ sexo == 'F', list( t, x, l13 ) ]
setorder( l_orf_f, t, x )
l_orf_f <- dcast.data.table( data = l_orf_f, x ~ t, value.var = 'l13' )
setorder( l_orf_f, x )
l_orf_f <- as.matrix( l_orf_f[ , 2:ncol( l_orf_f ) ] )
l_orf_f[ , 1 ] <- l_orf_f[ , 1 ] + l0_orf_f[ , 1 ]

l_orf_m <- pob_proy[ sexo == 'M', list( t, x, l13 ) ]
setorder( l_orf_m, t, x )
l_orf_m <- dcast.data.table( data = l_orf_m, x ~ t, value.var = 'l13' )
setorder( l_orf_m, x )
l_orf_m <- as.matrix( l_orf_m[ , 2:ncol( l_orf_m ) ] )
l_orf_m[ , 1 ] <- l_orf_m[ , 1 ] + l0_orf_m[ , 1 ]

l_viu_f <- matrix( 0, M, N )
l_viu_m <- matrix( 0, M, N )

l_viu_f <- pob_proy[ sexo == 'F', list( t, x, l14 ) ]
setorder( l_viu_f, t, x )
l_viu_f <- dcast.data.table( data = l_viu_f, x ~ t, value.var = 'l14' )
setorder( l_viu_f, x )
l_viu_f <- as.matrix( l_viu_f[ , 2:ncol( l_viu_f ) ] )
l_viu_f[ , 1 ] <- l_viu_f[ , 1 ] + l0_viu_f[ , 1 ]

l_viu_m <- pob_proy[ sexo == 'M', list( t, x, l14 ) ]
setorder( l_viu_m, t, x )
l_viu_m <- dcast.data.table( data = l_viu_m, x ~ t, value.var = 'l14' )
setorder( l_viu_m, x )
l_viu_m <- as.matrix( l_viu_m[ , 2:ncol( l_viu_m ) ] )
l_viu_m[ , 1 ] <- l_viu_m[ , 1 ] + l0_viu_m[ , 1 ]

# Proyecciones de poblaciones
for ( n in 1:(N-1) ) {
  for ( k in 1:(M-1) ) {
    l_orf_f[ k + 1, n + 1 ] <- p_f[ k, n ] %*% l_orf_f[ k, n ] + l_orf_f[ k + 1, n + 1 ] 
    l_orf_m[ k + 1, n + 1 ] <- p_m[ k, n ] %*% l_orf_m[ k, n ] + l_orf_m[ k + 1, n + 1 ] 
    
    l_viu_f[ k + 1, n + 1 ] <- p_f[ k, n ] %*% l_viu_f[ k, n ] + l_viu_f[ k + 1, n + 1 ] 
    l_viu_m[ k + 1, n + 1 ] <- p_m[ k, n ] %*% l_viu_m[ k, n ] + l_viu_m[ k + 1, n + 1 ] 
    
    l_pp_f[ k + 1, n + 1 ] <- pi_f[ k, n ] %*% l_pp_f[ k, n ]
    l_pp_m[ k + 1, n + 1 ] <- pi_m[ k, n ] %*% l_pp_m[ k, n ]
  }
}

pob_orf_f <- as.data.table( l_orf_f )
setnames( pob_orf_f, paste0( 't', 0:40 ) )
pob_orf_f <- cbind( data.table( x = x, sexo = 'F' ), pob_orf_f )
pob_orf_f <- melt.data.table( data = pob_orf_f, id.vars = c( 'x', 'sexo' ), 
                              measure.vars = 3:ncol( pob_orf_f ), 
                              variable.name = 't', value.name = 'l13' )
pob_orf_f[ , t := as.numeric( gsub( 't', '', t ) ) ]
pob_orf_f[ x > 18, l13 := 0 ]

pob_orf_m <- as.data.table( l_orf_m )
setnames( pob_orf_m, paste0( 't', 0:40 ) )
pob_orf_m <- cbind( data.table( x = x, sexo = 'M' ), pob_orf_m )
pob_orf_m <- melt.data.table( data = pob_orf_m, id.vars = c( 'x', 'sexo' ), 
                              measure.vars = 3:ncol( pob_orf_m ), 
                              variable.name = 't', value.name = 'l13' )
pob_orf_m[ , t := as.numeric( gsub( 't', '', t ) ) ]
pob_orf_m[ x > 18, l13 := 0 ]

pob_viu_f <- as.data.table( l_viu_f )
setnames( pob_viu_f, paste0( 't', 0:40 ) )
pob_viu_f <- cbind( data.table( x = x, sexo = 'F' ), pob_viu_f )
pob_viu_f <- melt.data.table( data = pob_viu_f, id.vars = c( 'x', 'sexo' ), 
                              measure.vars = 3:ncol( pob_viu_f ), 
                              variable.name = 't', value.name = 'l14' )
pob_viu_f[ , t := as.numeric( gsub( 't', '', t ) ) ]

pob_viu_m <- as.data.table( l_viu_m )
setnames( pob_viu_m, paste0( 't', 0:40 ) )
pob_viu_m <- cbind( data.table( x = x, sexo = 'M' ), pob_viu_m )
pob_viu_m <- melt.data.table( data = pob_viu_m, id.vars = c( 'x', 'sexo' ), 
                              measure.vars = 3:ncol( pob_viu_m ), 
                              variable.name = 't', value.name = 'l14' )
pob_viu_m[ , t := as.numeric( gsub( 't', '', t ) ) ]

pob_pp_f <- as.data.table( l_pp_f )
setnames( pob_pp_f, paste0( 't', 0:40 ) )
pob_pp_f <- cbind( data.table( x = x, sexo = 'F' ), pob_pp_f )
pob_pp_f <- melt.data.table( data = pob_pp_f, id.vars = c( 'x', 'sexo' ), 
                             measure.vars = 3:ncol( pob_pp_f ), 
                             variable.name = 't', value.name = 'l15' )
pob_pp_f[ , t := as.numeric( gsub( 't', '', t ) ) ]

pob_pp_m <- as.data.table( l_pp_m )
setnames( pob_pp_m, paste0( 't', 0:40 ) )
pob_pp_m <- cbind( data.table( x = x, sexo = 'M' ), pob_pp_m )
pob_pp_m <- melt.data.table( data = pob_pp_m, id.vars = c( 'x', 'sexo' ), 
                             measure.vars = 3:ncol( pob_pp_m ), 
                             variable.name = 't', value.name = 'l15' )
pob_pp_m[ , t := as.numeric( gsub( 't', '', t ) ) ]


pob_proy[ , l13 := NULL ]
pob_proy[ , l14 := NULL ]
pob_proy <- merge( pob_proy, 
                   rbind( pob_orf_f, pob_orf_m ), by = c( 't', 'sexo', 'x' ), all.x = TRUE )
pob_proy <- merge( pob_proy, 
                   rbind( pob_viu_f, pob_viu_m ), by = c( 't', 'sexo', 'x' ), all.x = TRUE ) 
pob_proy <- merge( pob_proy, 
                   rbind( pob_pp_f, pob_pp_m ), by = c( 't', 'sexo', 'x' ), all.x = TRUE )
pob_proy[ is.na( l13 ), l13 := 0 ]
pob_proy[ is.na( l14 ), l14 := 0 ]
pob_proy[ is.na( l15 ), l15 := 0 ]

# Agrupar proyecciones por año y sexo---------------------------------------------------------------
message( '\tAgrupando proyecciones por año y sexo' )
pob_proy_tot <- pob_proy[, list( l1 = sum( l1 ),
                                 l2 = sum( l2 ),
                                 l3 = sum( l3 ),
                                 l4 = sum( l4 ),
                                 l5 = sum( l5 ),
                                 l6 = sum( l6 ),
                                 l7 = sum( l7 ),
                                 l8 = sum( l8 ),
                                 l9 = sum( l9 ),
                                 l10 = sum( l10 ),
                                 l11 = sum( l11 ),
                                 l12 = sum( l12 ),
                                 l13 = sum( l13 ),
                                 l14 = sum( l14 ),
                                 l15 = sum( l15 ),
                                 l2_cot = sum( l2_cot ),
                                 l2_ces = sum( l2_ces ),
                                 l1_1 = sum( l1_1 ),
                                 l1_2 = sum( l1_2 ),
                                 l1_5 = sum( l1_5 ),
                                 l2_2 = sum( l2_2 ),
                                 l2_3 = sum( l2_3 ),
                                 l2_4 = sum( l2_4 ), 
                                 l2_5 = sum( l2_5 ),
                                 l3_3 = sum( l3_3 ),
                                 l3_5 = sum( l3_5 ),
                                 l4_4 = sum( l4_4 ),
                                 l4_5 = sum( l4_5 ) ),
                         by = list( t ) ]


pob_proy_tot_sex <- pob_proy[, list( l1 = sum( l1 ),
                                     l2 = sum( l2 ),
                                     l3 = sum( l3 ),
                                     l4 = sum( l4 ),
                                     l5 = sum( l5 ),
                                     l6 = sum( l6 ),
                                     l7 = sum( l7 ),
                                     l8 = sum( l8 ),
                                     l9 = sum( l9 ),
                                     l10 = sum( l10 ),
                                     l11 = sum( l11 ),
                                     l12 = sum( l12 ),
                                     l13 = sum( l13 ),
                                     l14 = sum( l14 ),
                                     l15 = sum( l15 ),
                                     l2_cot = sum( l2_cot ),
                                     l2_ces = sum( l2_ces ),
                                     l1_1 = sum( l1_1 ),
                                     l1_2 = sum( l1_2 ),
                                     l1_5 = sum( l1_5 ),
                                     l2_2 = sum( l2_2 ),
                                     l2_3 = sum( l2_3 ),
                                     l2_4 = sum( l2_4 ), 
                                     l2_5 = sum( l2_5 ),
                                     l3_3 = sum( l3_3 ),
                                     l3_5 = sum( l3_5 ),
                                     l4_4 = sum( l4_4 ),
                                     l4_5 = sum( l4_5 ) ), 
                             by = list( t, sexo ) ]

# Guaradar resultados ------------------------------------------------------------------------------
message("\tGuardando proyección de población")
save( lf, lm, ltf, ltm, pob_proy, pob_proy_tot_sex, pob_proy_tot,
      file = paste0( parametros$RData_seg, "IESS_RTR_proy_beneficarios_prestacion.RData" ) )

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !(ls() %in% c( "parametros" ) ) ] )
gc()
