message( paste( rep( '-', 100 ), collapse = '' ) )

load( paste0( parametros$RData_seg, 'IESS_proyeccion_salarios_escenario_1.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_proy_beneficarios_prestacion.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_proyeccion_beneficios.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_grupo_familiar.RData' ) )
load( paste0( parametros$RData, 'IESS_proy_masa_salarial.RData' ) )


# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros', 'pob_proy', 'ben_proy', 
                                 'sal_proy', 'pen_proy', 'rho', 'SALg', 'alpha' ) ) ] )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# 1.Balance corriente ------------------------------------------------------------------------------
message( '\tGenerando balance corriente' )
balance <- merge( pob_proy, 
                  sal_proy[ , list( t, sexo, x, sal ) ], by = c( 't', 'sexo', 'x' ) )
balance <- merge( balance, 
                  ben_proy, 
                  by = c( 't', 'sexo', 'x' ) )
balance <- merge( balance, 
                  esc$hip_esc, 
                  by = c( 't' ), all.x = TRUE )
setorder( balance, t, sexo, x )

# Pensiones nulas corrección
balance[ , aux := pen_mv ]
balance[ , aux := mean( aux, na.rm = TRUE ), by = list( t, sexo ) ]
balance[ is.na( pen_mv ), pen_mv := aux ]
balance[ , aux := NULL ]

message( '\tProyectando masa salarial' )

aux <- SALg %>%
  mutate( t = t - 2020,
          sexo = ifelse( sexo == 'H',
                         'M',
                         'F') ) %>%
  dplyr::select( t, x, sexo, M:=SALg ) %>%
  as.data.table( )

balance <- merge( balance, 
                  aux, 
                  by = c( 't', 'sexo', 'x' ) )

balance[ , M := cal_mas * M ]

# 1.1.  Beneficios de renta vitalicia---------------------------------------------------------------
message( '\tProyectando beneficios por pensiones de incapacidad' )
# 1.1.1.  Beneficio por pensiones de incapacidad PA y PT de RTR ------------------------------------
# por_inc<-(1200*0.8+146*1)/1346 #porcentaje de incapacidad promedio ponderado entre PT y PA a dic 2018
balance[ , B9 :=  cal_pen_pa_pt * pen_pa_pt * l9 ]
balance[ , B9_sbu := sbu * l9 ]
balance[ , B9_dec := ( B9 - B9_sbu ) / 13 + B9_sbu ]
balance[ , B9_nodec := 12 * ( B9 - B9_sbu ) / 13 ]

# 1.1.2.  Beneficio por pensiones de incapacidad PP de RTR -----------------------------------------
#No hay nuevas entradas de pensionistas de incapacidad PP
balance[ , B15 := cal_pen_pp * pen_pp * l15  ]
balance[ , B15_sbu := sbu * l15 ]
balance[ , B15_dec := ( B15 - B15_sbu ) / 13 + B15_sbu ]
balance[ , B15_nodec := 12 * ( B15 - B15_sbu ) / 13 ]

# 1.2.  Beneficios por indemnizaciones--------------------------------------------------------------
message( '\tProyectando beneficios por indemnizaciones' )
balance[ , B10 := cal_indm * pen_indemn * l10  ]

# 1.3.  Beneficios por subsidios--------------------------------------------------------------------
message( '\tProyectando beneficios por subsidios' )
balance[  , B11 := cal_subs * pen_sub * l11  ]

# 1.4.  Beneficios para orfandad--------------------------------------------------------------------
message( '\tProyectando beneficios por orfandad' )
#rho:= número de causantes con hijos menores / huerfanos beneficiarios
balance[ , B13 :=  cal_orf * pen_mo * l13  ]
balance[ , B13_sbu := 0.4 * rho * sbu * ( l13 ) ]
balance[ , B13_dec := ( B13 - B13_sbu ) / 13 + B13_sbu ]
balance[ , B13_nodec := 12 * ( B13 - B13_sbu ) / 13 ]

# 1.5.  Beneficios para viudedad--------------------------------------------------------------------
message( '\tProyectando beneficios por viudedad' )
balance[ , B14 :=  cal_viu * pen_mv * l14  ]
balance[ , B14_sbu := 0.6 * sbu * ( l14 ) ]
balance[ , B14_dec := ( B14 - B14_sbu ) / 13 + B14_sbu ]
balance[ , B14_nodec := 12 * ( B14 - B14_sbu ) / 13 ]

# 1.6.  Pago de prestaciones médico asistenciales---------------------------------------------------
message( '\tProyectando el pago de prestaciones médico asistenciales' )
balance[ , B_sal := apo_sal * M ]

# Beneficios por pensiones--------------------------------------------------------------------------
balance[ , B_pen := B9 + B13 + B14 + B15 ]
balance[ , B_sbu := B9_sbu + B13_sbu + B14_sbu + B15_sbu ]
balance[ , B_dec := B9_dec + B13_dec + B14_dec + B15_dec ]
balance[ , B_nodec := B9_nodec + B13_nodec + B14_nodec + B15_nodec ]



# 1.7. Proyecciones de aportes de afiliados --------------------------------------------------------
message( '\tProyectando aportes' )

# 1.7.1. Aportes de activos ------------------------------------------------------------------------
balance[ , A2_sal := apo_sal * M ]
balance[ , A2_cot := apo_cot * M ]
balance[ , A2 := A2_cot + A2_sal ]
balance[ t == 0 , A2_sal := 0 ]
balance[ t == 0 , A2_cot := 0 ]
balance[ t == 0 , A2 := 0 ]


# 1.7.1.1  Valor generado por RP--------------------------------------------------------------------
message( '\tProyectando valores generados por RP' )
balance[ , B_16 := rp * A2 ]

# Beneficios totales
balance[ , B := B_pen + B10 + B11 + B_sal + B_16]

# 1.7.2. Aportes de pensionistas de PA y PT sin décimos --------------------------------------------
balance[ , A9 := apo_jub * B9_nodec ] 
balance[ t == 0 , A5 := 0 ]

# 1.7.4. Aportes de beneficiarios de orfandad sin décimos ------------------------------------------
balance[ , A13 := apo_jub * B13_nodec ]
balance[ t == 0 , A9 := 0 ]

# 1.7.5. Aportes de beneficiarios de viudedad sin décimos ------------------------------------------
balance[ , A14 := apo_jub * B14_nodec ]
balance[ t == 0 , A10 := 0 ]

# 1.7.3. Aportes de pensionistas de PP sin décimos -------------------------------------------------
balance[ , A15 := apo_jub * B15_nodec ]
balance[ t == 0 , A7 := 0 ]

# 1.8. Resumen aportes y gastos --------------------------------------------------------------------
# Aportes de pensionistas
balance[ , A_pen := A9 + A13 + A14 + A15 ]

# Aportes de afiliados
balance[ , A_afi := A2 + A_pen ]

# Gasto administrativo
balance[ , G := por_gas * M ]
balance[ t == 0 , G := 0 ]

# Contribución del estado 
balance[ , A_est := apo_est * B_pen ]
balance[ t == 0 , A_est := 0 ]

# Aportes totales
balance[ , A := A_afi + A_est ] 

# Activos
balance[ , Act := A_afi + A_est ]

# Pasivo
balance[ , Pas := B + G ]

# 1.9. Calculo Balance corriente -------------------------------------------------------------------
balance_anual <- balance[ , list( M = sum( M , na.rm = TRUE ),
                                  
                                  Act = sum( Act, na.rm = TRUE ),
                                  Pas = sum( Pas , na.rm = TRUE ),
                                  
                                  A = sum( A , na.rm = TRUE ),
                                  A2 = sum( A2 , na.rm = TRUE ),
                                  A2_cot = sum( A2_cot , na.rm = TRUE ),
                                  A2_sal = sum( A2_sal , na.rm = TRUE ),
                                  A9 = sum( A9, na.rm = TRUE ),
                                  A13 = sum( A13 , na.rm = TRUE ),
                                  A14 = sum( A14 , na.rm = TRUE ),
                                  A15 = sum( A15 , na.rm = TRUE ),
                                  A_afi = sum( A_afi, na.rm = TRUE ),
                                  A_pen = sum( A_pen, na.rm = TRUE ),
                                  A_est = sum( A_est, na.rm = TRUE ),
                                  
                                  B = sum( B , na.rm = TRUE ),
                                  B_pen = sum( B_pen, na.rm = TRUE ),
                                  B_sbu = sum( B_sbu , na.rm = TRUE ),
                                  B_dec = sum( B_dec, na.rm = TRUE ),
                                  B_nodec = sum( B_nodec, na.rm = TRUE ),
                                  
                                  B9 = sum( B9 , na.rm = TRUE ),
                                  B9_sbu = sum( B9_sbu , na.rm = TRUE ),
                                  B9_dec = sum( B9_dec, na.rm = TRUE ),
                                  B9_nodec = sum( B9_nodec , na.rm = TRUE ),
                                  
                                  B10 = sum( B10, na.rm = TRUE ),
                                  
                                  B11 = sum( B11 , na.rm = TRUE ),
                                  
                                  B13 = sum( B13, na.rm = TRUE ),
                                  B13_sbu = sum( B13_sbu , na.rm = TRUE ),
                                  B13_dec = sum( B13_dec , na.rm = TRUE ),
                                  B13_nodec = sum( B13_nodec, na.rm = TRUE ),
                                  
                                  B14 = sum( B14, na.rm = TRUE ),
                                  B14_sbu = sum( B14_sbu, na.rm = TRUE ),
                                  B14_dec = sum( B14_dec , na.rm = TRUE ),
                                  B14_nodec = sum( B14_nodec , na.rm = TRUE ),
                                  
                                  B15 = sum( B15, na.rm = TRUE ),
                                  B15_sbu = sum( B15_sbu, na.rm = TRUE ),
                                  B15_dec = sum( B15_dec , na.rm = TRUE ),
                                  B15_nodec = sum( B15_nodec, na.rm = TRUE ),
                                  
                                  B_sal = sum( B_sal , na.rm = TRUE ),
                                  
                                  B_16 = sum( B_16, na.rm = TRUE ),
                                  
                                  G = sum( G, na.rm = TRUE ) ), 
                          by = list( t ) ]

balance_anual <- merge( balance_anual, esc$hip_esc[ , list( t, i_a ) ], by = 't' )
setorder( balance_anual, t )
balance_anual[ , r := i_a ]
balance_anual[ t == 0, r := 0 ]
balance_anual[ , r := 1 + r ]
balance_anual[ , r := cumprod( r ) ]
balance_anual[ , v := 1 / r  ]
balance_anual[ , V_cor := A - B - G ]
balance_anual[ t == 0, `:=`( M = 0, 
                             A = 0, A2 = 0, A2_cot = 0, A2_sal = 0, 
                             A9 = 0, A13 = 0, A14 = 0, A15 = 0, A_afi = 0, A_pen = 0, A_est = 0,
                             B = 0, B_pen = 0, B_sbu = 0, B_dec = 0, B_nodec = 0,
                             B9 = 0, B9_sbu = 0, B9_dec = 0, B9_nodec = 0,
                             B10 = 0, 
                             B11 = 0,
                             B13 = 0, B13_sbu = 0, B13_dec = 0, B13_nodec = 0,
                             B14 = 0, B14_sbu = 0, B14_dec = 0, B14_nodec = 0,
                             B15 = 0, B15_sbu = 0, B15_dec = 0, B15_nodec = 0,
                             B_sal = 0,
                             B_16 = 0,
                             G = 0, 
                             V_cor = 0 , 
                             Act = 0, Pas = 0 ) ]
balance_anual[ , V_cap := V_cor ]
balance_anual[ t == 0, V_cap := esc$V0 ]
balance_anual[ t == 0, Act := esc$V0 ]
balance_anual[ , Act := r * cumsum( v * Act ) ]
balance_anual[ , Pas := r * cumsum( v * Pas ) ]
balance_anual[ , V_cap := r * cumsum( v * V_cap ) ]

# 2. Balance actuarial -----------------------------------------------------------------------------
balance_anual[ , M_vap := cumsum( v * M ) ]

balance_anual[ , Act_vap := v * Act ]
balance_anual[ , Pas_vap := v * Pas ]

balance_anual[ , A_vap := cumsum( v * A ) ]
balance_anual[ , A2_vap := cumsum( v * A2 ) ]
balance_anual[ , A2_cot_vap := cumsum( v * A2_cot ) ]
balance_anual[ , A2_sal_vap := cumsum( v * A2_sal ) ]
balance_anual[ , A9_vap := cumsum( v * A9 ) ]
balance_anual[ , A13_vap := cumsum( v * A13 ) ]
balance_anual[ , A14_vap := cumsum( v * A14 ) ]
balance_anual[ , A15_vap := cumsum( v * A15 ) ]
balance_anual[ , A_afi_vap := cumsum( v * A_afi ) ]
balance_anual[ , A_pen_vap := cumsum( v * A_pen ) ]
balance_anual[ , A_est_vap := cumsum( v * A_est ) ]

balance_anual[ , B_vap := cumsum( v * B ) ]
balance_anual[ , B_pen_vap := cumsum( v * B_pen ) ]
balance_anual[ , B_sbu_vap := cumsum( v * B_sbu ) ]
balance_anual[ , B_dec_vap := cumsum( v * B_dec ) ]
balance_anual[ , B_nodec_vap := cumsum( v * B_nodec ) ]

balance_anual[ , B9_vap := cumsum( v * B9 ) ]
balance_anual[ , B9_sbu_vap := cumsum( v * B9_sbu ) ]
balance_anual[ , B9_dec_vap := cumsum( v * B9_dec ) ]
balance_anual[ , B9_nodec_vap := cumsum( v * B9_nodec ) ]

balance_anual[ , B13_vap := cumsum( v * B13 ) ]
balance_anual[ , B13_sbu_vap := cumsum( v * B13_sbu ) ]
balance_anual[ , B13_dec_vap := cumsum( v * B13_dec ) ]
balance_anual[ , B13_nodec_vap := cumsum( v * B13_nodec ) ]

balance_anual[ , B10_vap := cumsum( v * B10 ) ]

balance_anual[ , B11_vap := cumsum( v * B11 ) ]

balance_anual[ , B_sal_vap := cumsum( v * B_sal ) ]

balance_anual[ , B14_vap := cumsum( v * B14 ) ]
balance_anual[ , B14_sbu_vap := cumsum( v * B14_sbu ) ]
balance_anual[ , B14_dec_vap := cumsum( v * B14_dec ) ]
balance_anual[ , B14_nodec_vap := cumsum( v * B14_nodec ) ]

balance_anual[ , B15_vap := cumsum( v * B15 ) ]
balance_anual[ , B15_sbu_vap := cumsum( v * B15_sbu ) ]
balance_anual[ , B15_dec_vap := cumsum( v * B15_dec ) ]
balance_anual[ , B15_nodec_vap := cumsum( v * B15_nodec ) ]

balance_anual[ , B16_vap := cumsum( v * B_16 ) ]

balance_anual[ , G_vap := cumsum( v * G ) ]
balance_anual[ , V := v * V_cap ]
balance_anual[ , V0 := esc$V0 ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( balance, balance_anual,
      file = paste0( parametros$RData_seg, 'IESS_RTR_balances_', esc$nombre, '.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()

