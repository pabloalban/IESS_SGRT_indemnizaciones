message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )

# Activo Fondo--------------------------------------------------------------------------------------
message( '\tGraficando activo del fondo' )
unidad<-1e6
aux <- as.data.table(activo_del_fondo)
aux[,Activo:=Activo/unidad]
x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2000 )
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_activo_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Activo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_activo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_activo_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Cuentas por cobrar del Fondo de RT ----------------------------------------------------------------
message( '\tGraficando Cuentas por cobrar del Fondo de RT ' )
unidad<-1e6
aux <- as.data.table( cuentas_cobrar_fondo )
aux <- aux[,`Cuentas por Cobrar`:=`Cuentas por Cobrar`/unidad]
x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 90)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Cuentas por Cobrar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_cuentas_cobrar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_cobrar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pasivo del Fondo de RT-----------------------------------------------------------------------------
message( '\tGraficando Pasivo del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( pasivos_fondo )
aux <- aux[,Pasivo:=Pasivo/unidad]
x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_fondo <-ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Pasivo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_pasivo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivo_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Cuentas por pagar del  Fondo de RT-----------------------------------------------------------------
message( '\tGraficando cuentas por pagar del Fondo de RT' )
unidad<-1e6
aux <- as.data.table( cuentas_pagar_fondo )
aux <- aux[,`Cuentas por Pagar`:=`Cuentas por Pagar`/unidad]
x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_pagar_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Cuentas por Pagar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_cuentas_pagar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_pagar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Patrimonio del  Fondo de RT------------------------------------------------------------------------
message( '\tGraficando patrimonio del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( patrimonio_fondo )
aux <- aux[,Patrimonio:=Patrimonio/unidad]
x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1500)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6  )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Patrimonio, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_patrimonio_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Ingresos del  Fondo de RT--------------------------------------------------------------------------
message( '\tGraficando ingrensos del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( ingresos_fondo )
aux <- aux[, Ingresos:=Ingresos/unidad]
x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 300 )
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Ingresos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_ingresos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución del gasto del   Fondo de RT--------------------------------------------------------------
message( '\tGraficando evolución del gasto del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gastos )
aux <- aux[, `Gastos`:=`Gastos`/unidad]

x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 100)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Gastos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5) )

ggsave( plot = iess_gastos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución del resultado del ejercicio Fondo de RT--------------------------------------------------
message( '\tGraficando resultado del ejercicio del Fondo de RT.' )
unidad<-1e6
aux <- ingresos_vs_gastos %>%
  clean_names( )

x_lim <- c( 2013, 2022 )
x_brk <- 2013:2022
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 220000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 12 )
y_lbl <- formatC( y_brk / unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_resultado_del_ejercicio  <-  ggplot( data = aux ) + 
  geom_line( aes( x = ano, 
                  y = resultado_del_ejercicio, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5) )

ggsave( plot = iess_resultado_del_ejercicio, 
        filename = paste0( parametros$resultado_graficos, 'iess_resultado_del_ejercicio_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()