message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando demografía del SGRT' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_mora_patronal.RData' ) )

#Evolución histórica de la emisión de rp y montos glosados------------------------------------------
message( '\tGraficando evolución histórica de las rp' )

aux <- evo_rp_montos_anio %>%
  filter( anio > 2009 ) %>%
  mutate( periodo = as.Date( paste0( "01/01/",anio), "%d/%m/%Y" ),
          instrumento = 'Montos de RP') %>%
  dplyr::select( periodo,
                 rp,
                 monto,
                 instrumento )

df_bar <- aux %>% select(-rp)
df_line = aux %>% select(periodo, rp)

scl = 1000  # escala de millones
hmts = 2.3 #homotecia

y_lim <- c( 0, 7000000 )
y_brk <- seq( y_lim[1], y_lim[2], 1000000 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim_dual <- c( 0, 2600 )
ydual_brk <- seq( 0, 3000, 500 )
ydual_lbl <- paste0(formatC( ydual_brk, 
                             digits = 0, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ) )


iess_evo_rp_montos_anio <- ggplot( data = df_bar,
                                   aes( x = periodo,
                                        y = monto,
                                        fill = instrumento ) ) +
  geom_bar( stat = 'identity',
           colour = 'black') +
  geom_line( data = df_line,
             aes(x = periodo,
                 y = rp*hmts*scl,
                 group = 1,
                 linetype = 'Afiliados' ),
             inherit.aes = FALSE,
             size = 1 ) +
  scale_linetype_manual( NULL, values = 1 ) +
  scale_x_date( date_breaks = '1 year', date_labels = '%Y' )+
  scale_y_continuous( name = 'Valores causados por RP del SGRT (millones USD)',
                      labels = y_lbl, breaks = y_brk, limits = y_lim,
                      sec.axis = sec_axis( ~./( scl * hmts ),
                                           name = 'Afiliados',
                                           labels = ydual_lbl,
                                           breaks = ydual_brk ) ) +
  scale_fill_manual( values = c( parametros$iess_green,
                                 parametros$iess_blue ) )+
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = "right", 
                               label.hjust = 0 ) )+
  theme( legend.position = 'bottom' ) +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect( fill = 'transparent' ),
        legend.box.background = element_rect( fill = 'transparent', 
                                             colour = NA ),
        legend.key = element_rect( fill = 'transparent' ), 
        legend.spacing = unit( -1, 'lines' ) )


ggsave( plot = iess_evo_rp_montos_anio, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_rp_montos_anio', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Pirámide por edad y sexo de los afiliados----------------------------------------------------------
message( '\tGraficando Pirámide de beneficiarios de SGRT' )

aux <- pir_rp_edad_sexo %>%
  mutate( fdp = if_else( sexo == 'H',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 90 ) %>%
  arrange( sexo, edad )

salto_y <- 10
salto_x <- 0.005
brks_y <- seq( -0.04, 0.04, salto_x )
lbls_y <- paste0( as.character( c( seq( 0.04, 0, -salto_x )*100, seq( salto_x, 0.04, salto_x )*100) ), "%")
brks_x <- seq( 15, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_rp_edad_sexo <- ggplot( aux, aes( x = edad, y = fdp, fill=sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux %>% filter( sexo == 'H' ), stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend( title = NULL,label.position = "right",
                              label.hjust = 0, label.vjust = 0.5 ) )+
  theme( legend.position="bottom" ) +
  scale_fill_manual( values = c( parametros$male, parametros$female ),
                     labels = c( "Hombres", "Mujeres") )

ggsave( plot = iess_pir_rp_edad_sexo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_rp_edad_sexo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Limpiar Memoria RAM-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
