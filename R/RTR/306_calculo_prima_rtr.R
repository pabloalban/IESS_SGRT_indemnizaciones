message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

escenarios <- paste0( 'escenario_', 1:1 )

gamma <- 0# porcentaje de V_0 que se incluye en el calculo de la prima media nivelada 

for ( escenario in escenarios ) {
  message( '\tCálculo de la prima para el ', escenario )
  load( paste0( parametros$RData_seg, 'IESS_RTR_balances_', escenario, '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_RTR_configuracion_', escenario, '.RData' ) )
  
  prima <- balance_anual[ t > 0,  ]
  
  # Porcentaje de contribución por décimos
  delta <- 0.5
  
  # Prima de reparto puro ----------------------------------------------------------------------------
  prima[ , pri_rep_pur := ( B ) /  M ] # sin aporte estatal
  prima[ , pri_rep_pur_apo_est := ( B + G - A_est ) /  M ] # con aporte estatal AE
  prima[ , pri_rep_pur_apo_est_pen := ( B + G  - A_est - A9 - A13 - A14 - A15 ) /  M ] # con aporte estatal AE y aporte de pensionistas
  prima[ , pri_rep_pur_apo_pen := ( B + G  - A9 - A13 - A14 - A15 ) / M ] # sin aporte estatal AE y con aporte de pensionistas
  prima[ , pri_rep_pur_delta := ( B + G - delta * B_dec - A_est ) / M ] 
  prima[ , pri_rep_pur_delta_pen := delta * B_dec / M ]
  
  # Prima nivelada en cada horizonte -----------------------------------------------------------------
  prima[ , pri_med_niv := (  B_vap + G_vap - gamma * V0 ) /  M_vap ] # sin aporte estatal
  prima[ , pri_med_niv_apo_est := (  B_vap + G_vap - A_est_vap - gamma * V0 ) /  M_vap ] # con aporte estatal AE
  prima[ , pri_med_niv_apo_est_pen := (  B_vap + G_vap - A_est_vap - A9_vap - A13_vap - A14_vap - A15_vap - gamma * V0 ) / M_vap ] # con aporte estatal AE y aporte de pensionistas
  prima[ , pri_med_niv_apo_pen := (  B_vap + G_vap - A9_vap - A13_vap - A14_vap - A15_vap - gamma * V0 ) / M_vap ] # sin aporte estatal AE y con aporte de pensionistas
  prima[ , pri_med_niv_delta := (  B_vap + G_vap - delta * B_dec_vap - A_est_vap - gamma * V0 ) / M_vap ] 
  prima[ , pri_med_niv_delta_pen := delta * B_dec_vap / M_vap ]
  
  save( prima, 
        file = paste0( parametros$RData_seg, 'IESS_RTR_primas_', escenario, '.RData' ) )  
}

# Limpiando memoria RAM-----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

