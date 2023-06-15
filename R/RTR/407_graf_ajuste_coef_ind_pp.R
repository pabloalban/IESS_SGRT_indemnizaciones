message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_coef_indemnizacion_int.RData' ) )

#Gráfico del alisado de tasa de uso del seguro hombres ---------------------------------------------
a<-(0)
b<-0.5
c<-(0.5)
d<-(0)

#Hombres--------------------------------------------------------------------------------------------
message( '\tGraficando el coeficiente de indemnizaciones ajustado hombres' )

aux <- coef_ind_pp_int %>% filter( sexo == 'M')

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.05 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


coef_ind_pp_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = coef_ind_pp, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = coef_ind_pp_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, c_{1,x}^{10}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = coef_ind_pp_m, 
        filename = paste0( parametros$resultado_graficos, 'coef_ind_pp_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Mujeres--------------------------------------------------------------------------------------------
message( '\tGraficando el coeficiente de indemnizaciones ajustado mujeres' )

aux <- coef_ind_pp_int %>% filter( sexo == 'F')

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.05 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


coef_ind_pp_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = coef_ind_pp, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = coef_ind_pp_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\beta_{2,x}^{10}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = coef_ind_pp_f, 
        filename = paste0( parametros$resultado_graficos, 'coef_ind_pp_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# ------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()