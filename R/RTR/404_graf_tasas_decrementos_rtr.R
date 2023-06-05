message( paste( rep('-', 100 ), collapse = '' ) )

#load( paste0( parametros$RData, 'IESS_tabla_decrementos.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_RTR_tabla_decrementos.RData' ) ) 
load( paste0( parametros$RData, 'IESS_estimacion_tasa_entradas.RData' ) )

source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Fuerza de entrada de no afiliado a afiliado-------------------------------------------------------
message( '\tGraficando tasa de entradas' )

x_lim <- c( 0, 110 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- tasa_ent_esp[ sexo == 'F' ]
plt_ent_f <-  ggplot( data = aux_f ) + 
              geom_point( aes( x = x, y = ue_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = ue, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$mujeres \\; \\mu_{1,x}^{1,2}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_ent_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_ent_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

x_lim <- c( 0, 110 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_m <- tasa_ent_esp[ sexo == 'M' ]
plt_ent_m <-  ggplot( data = aux_m ) + 
              geom_point( aes( x = x, y = ue_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = ue, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$hombres \\; \\mu_{2,x}^{1,2}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_ent_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_ent_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Fuerza de vejez afiliados --------------------------------------------------------------------------
message( '\tGraficando fuerza transición afiliado a pensionista por vejez' )
x_lim <- c( 50, 100 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( -10, 0 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  tab_dec[ sexo == 'F' & is.finite( log_uv ) & is.finite( log_uv_est ) ]

plt_vej_f  <- ggplot( data = aux_f ) + 
              geom_point( aes( x = x, y = log_uv_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_uv, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$mujeres \\; \\log \\, \\mu_{1,x}^{2,3}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_vej_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_vej_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  tab_dec[ sexo == 'M' & is.finite( log_uv ) & is.finite( log_uv_est ) ]

plt_vej_m  <- ggplot( data = aux_m  ) + 
              geom_point( aes( x = x, y = log_uv_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_uv, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$hombres \\; \\log \\, \\mu_{2,x}^{2,3}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_vej_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_vej_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Fuerza de mortalidad afiliados ---------------------------------------------------------------------
message( '\tGraficando fuerza transición afiliado a muerto' )
x_lim <- c( 0, 100 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -8.5, 0 )
y_brk <- seq( round(y_lim[1],0), y_lim[2], length.out = 5 )
y_lbl <- y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  tab_dec[ sexo == 'F' ]

plt_mort_f <- ggplot( data = aux_f  ) + 
              geom_point( aes( x = x, y = log_ud_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_ud, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$mujeres \\; \\log \\, \\mu_{1,x}^{2,6}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_mort_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_mort_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  tab_dec[ sexo == 'M' ]

plt_mort_m <- ggplot( data = aux_m  ) + 
              geom_point( aes( x = x, y = log_ud_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_ud, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$hombres \\; \\log \\, \\mu_{2,x}^{2,6}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_mort_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_mort_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Fuerza de invalidez afiliados --------------------------------------------------------------------------
message( '\tGraficando fuerza transición afiliado a pensionista por invalidez' )
x_lim <- c( 20, 100 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -14, 0 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  tab_dec[ sexo == 'F' & is.finite( log_ui ) & is.finite( log_ui_est ) ]

plt_inv_f <-  ggplot( data = aux_f  ) + 
              geom_point( aes( x = x, y = log_ui_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_ui, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$mujeres \\; \\log \\, \\mu_{1,x}^{2,4}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_inv_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_inv_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  tab_dec[ sexo == 'M' & is.finite( log_ui ) & is.finite( log_ui_est ) ]

plt_inv_m <-  ggplot( data = aux_m  ) + 
              geom_point( aes( x = x, y = log_ui_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_ui, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$hombres \\; \\log \\, \\mu_{2,x}^{2,4}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_inv_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_inv_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Fuerza de incapacidad afiliados ------------------------------------------------------------------
message( '\tGraficando fuerza transición afiliado a pensionista por incapacidad' )
x_lim <- c( 20, 80 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk


aux_f <-  tab_dec[ sexo == 'F' & is.finite( log_uin_est ) & is.finite( log_uin ) ]

y_lim <- c(-14,-8 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- y_lbl <- formatC( y_brk, digits =0, format = 'f', big.mark = '.', decimal.mark = ',' )



plt_inc_f <-  ggplot( data = aux_f  ) + 
              geom_point( aes( x = x, y = log_uin_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_uin, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$mujeres \\; \\log \\, \\mu_{1,x}^{2,5}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_inc_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_inc_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  tab_dec[ sexo == 'M' & is.finite( log_uin_est ) & is.finite( log_uin ) ]

plt_inc_m <-  ggplot( data = aux_m  ) + 
              geom_point( aes( x = x, y = log_uin_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_uin, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$hombres \\; \\log \\, \\mu_{2,x}^{2,5}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_inc_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_inc_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Fuerza de mortalidad de pensionistas de incapacidad-----------------------------------------------
message( '\tGraficando fuerza de mortalidad de pensionistas de incapacidad' )
x_lim <- c( 20, 80 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk


aux_f <-  tab_dec[ sexo == 'F' & is.finite( log_duin_est ) & is.finite( log_duin ) ]

y_lim <- c(-8,-2 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- y_lbl <- formatC( y_brk, digits =0, format = 'f', big.mark = '.', decimal.mark = ',' )



plt_dinc_f <- ggplot( data = aux_f  ) + 
              geom_point( aes( x = x, y = log_duin_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_duin, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$mujeres \\; \\log \\, \\mu_{1,x}^{5,6}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_dinc_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_dinc_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  tab_dec[ sexo == 'M' & is.finite( log_duin_est ) & is.finite( log_duin ) ]

plt_dinc_m <- ggplot( data = aux_m  ) + 
              geom_point( aes( x = x, y = log_duin_est, color = parametros$iess_green ), size = graf_point_size ) + 
              geom_line( aes( x = x, y = log_duin, color = parametros$iess_blue ), size = graf_line_size ) + 
              xlab(TeX("edad $x$"))+
              ylab(TeX("$hombres \\; \\log \\, \\mu_{2,x}^{5,6}$")) +
              scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                  labels = c( 'alisado', 'estimado' ) ) +
              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
              theme_bw() +
              plt_theme

ggsave( plot = plt_dinc_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_fuer_dinc_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Limpiando -----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
