message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGraficando resultados del balance' )

source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

escenarios_lista <- paste0( 'escenario_', 1:1 ) # 4 escenarios 
# Escenarios corrientes ----------------------------------------------------------------------------
for ( i in 1:length( escenarios_lista ) ) {
  escenario <- escenarios_lista[i]
  #escenario <- escenarios_lista[1]
  load( paste0( parametros$RData_seg, 'IESS_RTR_balances_', escenario, '.RData' ) )
  
  num_anios <- length( unique( balance_anual$t ) )
  cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
  cols_graf <- cols_fun( num_anios )
  
  balance_anual[ , t := t + parametros$anio_ini ]
  # grafico balance capitalizado -------------------------------------------------------------------
  
  x_lim <- c( parametros$anio_ini, parametros$anio_ini + parametros$horizonte )
  x_brk <- seq( x_lim[1], x_lim[2], 5 )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  y_lim <- c( min( balance_anual$V_cap ), max( balance_anual$V_cap ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC( y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
  
  plt_bal_act <- ggplot() +
    geom_line( data = balance_anual, aes( x = t, y = V_cap ), 
               size = graf_line_size, color = parametros$iess_blue ) +
    geom_hline( aes( yintercept = 0 ), size = 0.5 * graf_line_size, color = parametros$iess_green,
                linetype = 2 ) +
    xlab( 'Año') +
    ylab( 'Balance Capitalizado (millones)' ) +
    scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
    scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl  ) +
    theme_bw() +
    plt_theme
  
  # plt_bal_act
  
  ggsave( plot = plt_bal_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_balance_capitalizado_', 
                             escenario, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  
  # Gráfico del balance actuarial con barras--------------------------------------------------------
  x_lim <- c( parametros$anio_ini, parametros$anio_ini + parametros$horizonte )
  x_brk <- seq( x_lim[1], x_lim[2], 5 )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  y_lim <- c( 0, max( balance_anual$V_cap ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC( y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
  
  plt_bal_act <-  ggplot() +
    geom_bar( data = balance_anual[ V_cap >= 0 ], aes( x = t, y = V_cap ), 
              stat = 'identity', fill = parametros$iess_blue, width = 0.5) +
    geom_bar( data = balance_anual[ V_cap < 0 ], aes( x = t, y = V_cap ), 
              stat = 'identity', fill = 'red', width = 0.5) +
    geom_hline( aes( yintercept = 0 ), 
                size = 0.5 * graf_line_size, 
                color = parametros$iess_green,
                linetype = 2 ) +
    xlab( 'Año') +
    ylab( 'Balance actuarial dinámico (millones)' ) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl ) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl  ) +
    theme_bw() +
    plt_theme
  
  # plt_bal_act
  
  ggsave( plot = plt_bal_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_balance_capitalizado_rv_',
                             escenario, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  # Gráfico aportes y beneficios ---------------------------------------------------------------------
  y_lim <- c( min( balance_anual[ , list(A, B) ] ), max( balance_anual[ , list(A, B) ] ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC( y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
  
  aux1 <- balance_anual[ , list(item = 'Aportes', t = t, valor = A) ]
  aux2 <- balance_anual[ , list(item = 'Beneficios', t = t, valor = B) ] 
  
  aux <- rbind(aux1, aux2)
  plt_apo_ben_act <- ggplot( aux, aes( x = t, y = valor ) ) +
    geom_line( aes(colour = item), size = graf_line_size ) +
    xlab( 'Año' ) +
    ylab( 'Aportes y Beneficios Balance Capitalizado (millones)' ) +
    scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
    theme_bw() +
    scale_colour_manual( values = c( 'Aportes' = parametros$iess_blue,
                                     'Beneficios' = parametros$iess_green ) ) +
    plt_theme +
    theme(legend.position="bottom") +
    labs( x = '', y = '' )+
    theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )
  # plt_apo_ben_act
  
  ggsave( plot = plt_apo_ben_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_apo_ben_bal_capitalizado_', 
                             escenario, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
}


# Escenarios dinámicos -----------------------------------------------------------------------------
for ( i in 1:length( escenarios_lista ) ) {
  escenario <- escenarios_lista[i]
  #escenario <- escenarios_lista[1]
  load( paste0( parametros$RData_seg, 'IESS_RTR_balances_', escenario, '.RData' ) )
  
  num_anios <- length( unique( balance_anual$t ) )
  cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
  cols_graf <- cols_fun( num_anios )
  
  balance_anual[ , t := t + parametros$anio_ini ]
  # grafico balance actuarial ----------------------------------------------------------------------
  x_lim <- c( parametros$anio_ini, parametros$anio_ini + parametros$horizonte )
  x_brk <- seq( x_lim[1], x_lim[2], 5 )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  y_lim <- c( min( balance_anual$V ), max( balance_anual$V ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC( y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
  
  plt_bal_act <-  ggplot() +
    geom_line( data = balance_anual, aes( x = t, y = V ), 
               size = graf_line_size, color = parametros$iess_blue ) +
    geom_hline( aes( yintercept = 0 ), 
                size = 0.5 * graf_line_size, 
                color = parametros$iess_green, 
                linetype = 2 ) +
    xlab( 'Año' ) +
    ylab( 'Balance actuarial dinámico (millones)' ) +
    scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
    scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl  ) +
    theme_bw() +
    plt_theme
  
  # plt_bal_act
  
  ggsave( plot = plt_bal_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_balance_vap_', escenario, 
                             parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  #Gráfico del balance actuarial con barras-----------------------------------------------------------
  x_lim <- c( parametros$anio_ini, parametros$anio_ini + parametros$horizonte )
  x_brk <- seq( x_lim[1], x_lim[2], 5 )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  y_lim <- c( 0, max( balance_anual$V ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC( y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
  
  plt_bal_act <-  ggplot() +
    geom_bar( data = balance_anual[ V >= 0 ], aes( x = t, y = V ) , stat = 'identity', 
              fill=parametros$iess_blue, width = 0.5) +
    geom_bar( data = balance_anual[ V < 0 ], aes( x = t, y = V ) , stat = 'identity', 
              fill='red', width = 0.5) +
    geom_hline( aes( yintercept = 0 ), 
                size = 0.5 * graf_line_size, 
                color = parametros$iess_green,
                linetype = 2 ) +
    xlab( 'Año') +
    ylab( 'Balance actuarial dinámico (millones)' ) +
    scale_x_continuous(  breaks = x_brk, labels = x_lbl ) +
    scale_y_continuous(  breaks = y_brk, labels = y_lbl  ) +
    theme_bw() +
    plt_theme
  
  # plt_bal_act
  
  ggsave( plot = plt_bal_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_balance_vap_rv_', escenario, 
                             parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  
  # Gráfico aportes y beneficios ---------------------------------------------------------------------
  y_lim <- c( min( balance_anual[ , list(A_vap, B_vap) ] ), 
              max( balance_anual[ , list(A_vap, B_vap) ] ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC( y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
  
  aux1 <- balance_anual[ , list(item = 'Aportes', t = t, valor = A_vap) ]
  aux2 <- balance_anual[ , list(item = 'Beneficios', t = t, valor = B_vap) ] 
  
  aux <- rbind(aux1, aux2)
  plt_apo_ben_act <- ggplot( aux, aes( x = t, y = valor ) ) +
    geom_line( aes(colour = item), size = graf_line_size ) +
    xlab( 'Año' ) +
    ylab( 'Aportes y Beneficios Balance Dinámico (millones)' ) +
    scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
    theme_bw() +
    scale_colour_manual( values = c('Aportes' = parametros$iess_blue,
                                    'Beneficios' = parametros$iess_green) ) +
    plt_theme +
    theme( legend.position = "bottom" ) +
    labs( x = '', y = '' )+
    theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )
  
  # plt_apo_ben_act
  
  ggsave( plot = plt_apo_ben_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_apo_ben_bal_dinamico_', escenario,
                             parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
}

# Limpieza -----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
