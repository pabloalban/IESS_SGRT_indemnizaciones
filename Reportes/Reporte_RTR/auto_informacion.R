message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tEstableciendo información para la configuración del reporte' )
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_modelo_rp.RData' ) )
load( paste0(parametros$RData_seg, "IESS_RTR_modelo_rp.RData") )

REP <- new.env()

# Tasas y parámetros generales----------------------------------------------------------------------
REP$i_s <- format( Hipotesis[4,2],
                   digits = 2, nsmall = 2, big.mark = '.',
                   decimal.mark = ',', format = 'f' )

REP$i_sbu <- format( Hipotesis[5,2],
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )

REP$i_p <- format( Hipotesis[6,2],
                   digits = 2, nsmall = 2, big.mark = '.',
                   decimal.mark = ',', format = 'f' )

REP$i_a <- format( 6.25,
                   digits = 2, nsmall = 2, big.mark = '.',
                   decimal.mark = ',', format = 'f' )

REP$i_rp <- format( coeficiente * 100,
                    digits = 2, nsmall = 2, big.mark = '.',
                    decimal.mark = ',', format = 'f' )
# Escenario 1 --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_balances_', esc$nombre, '.RData' ) )

REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )


REP$cap_ini <- format( esc$V0,
                       digits = 2, nsmall = 2, big.mark = '.',
                       decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
                                 digits = 4, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

REP$apo_est_esc_1 <- format( 100 * esc$hip_esc$apo_est[2],
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$tasa_act_esc_1 <- format( 100 * esc$hip_esc$i_a[2],
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

REP$tasa_apo_jub_esc_1 <- format( 100 * esc$hip_esc$apo_jub[2],
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' )

REP$tasa_por_gas_esc_1 <- format( 100 * esc$hip_esc$por_gas[2],
                                  digits = 2, nsmall = 2, big.mark = '.',
                                  decimal.mark = ',', format = 'f' )

REP$tasa_aporte_salud_esc_1 <- format( 100 * esc$hip_esc$apo_sal[2],
                                       digits = 2, nsmall = 2, big.mark = '.',
                                       decimal.mark = ',', format = 'f' )


REP$tasa_aporte_esc_1_2021 <- format( 100 * ( esc$hip_esc$apo_cot[4] + esc$hip_esc$apo_sal[4] ),
                                      digits = 2, nsmall = 2, big.mark = '.',
                                      decimal.mark = ',', format = 'f' )

REP$bal_sum_act_1 <- format( balance_anual[ t == parametros$horizonte ]$Act_vap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_sum_pas_1 <- format( balance_anual[ t == parametros$horizonte ]$Pas_vap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$ing_jubilados_1 <- format( balance_anual[ t == parametros$horizonte ]$A_pen_vap,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' )

REP$ing_apo_est_1 <- format( balance_anual[ t == parametros$horizonte ]$A_est_vap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$egre_rp_1 <- format( balance_anual[ t == parametros$horizonte ]$B16_vap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )
