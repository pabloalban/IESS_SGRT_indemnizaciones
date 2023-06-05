message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando causas de desfinanciamiento' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tGráfica de cooparación entre primas de la CD501 y CD261' )
file_prima <- paste0( parametros$RData_seg, 'IESS_RTR_causas_desfinanciamiento.RData' )
load( file = file_prima )

#Gráfico de comparación de tasas de aportación------------------------------------------------------
com_pri_apo <- (comparacion_primas)
com_pri_apo$fecha <- as.Date(com_pri_apo$fecha,"%d/%m/%Y") 
com_pri_apo <- com_pri_apo %>% 
  dplyr::arrange(fecha) %>%
  mutate( cd_261= cd_261 / 100,
          cd_501_privado = cd_501_privado / 100,
          cd_501_publico = cd_501_publico /100 )

message( '\tGrafico: Comparación de las primas de aportes' )
x_lim <- c( 2015, 2022 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
y_brk<- seq(0.001,0.006,0.001)
y_lbl <- paste0(formatC(100 * y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ),"%")

plt_com_pri_apo <-  ggplot(com_pri_apo, aes(fecha)) + 
                    geom_line(aes(y = cd_261 ,colour ="C.D.261")) +
                    geom_line(aes(y = cd_501_privado ,colour ="C.D.501 privados"), size=1.5) +
                    geom_line(aes(y = cd_501_publico ,colour ="C.D.501 públicos"),
                              linetype = "dashed",
                              alpha=0.8,
                              size=1.1) +
                    geom_point(aes(y = cd_501_privado,colour ="C.D.501 privados"), 
                               shape = 15, 
                               # size = graf_line_size,
                               size = 3,
                               color = parametros$iess_green)+
                    geom_point(aes(y = cd_501_publico,colour ="C.D.501 públicos"), 
                               shape = 20, 
                               # size = graf_line_size,
                               size = 3,
                               color = 'red' ) +
                    geom_point( aes(y = cd_261 ,colour ="C.D.261"), 
                                shape = 15, 
                                # size = graf_line_size,
                                size = 2,
                                color = parametros$iess_blue ) +
                    scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
                    scale_y_continuous(breaks = y_brk,
                                       labels = y_lbl,
                                       limits = c(y_brk[1], max(y_brk))) +
                    scale_colour_manual("", 
                                        breaks = c("C.D.501 privados", "C.D.501 públicos" ,"C.D.261"), 
                                        values = c("C.D.501 privados" = parametros$iess_green ,
                                                   "C.D.501 públicos" = 'red',
                                                   "C.D.261" = parametros$iess_blue))+
                    geom_text_repel(aes(fecha, cd_501_publico, label =etiqueta ),
                                    point.padding = unit(0.19, 'lines'),
                                    arrow = arrow(length = unit(0.01, 'npc')),
                                    segment.size = 0.1,
                                    segment.color = '#cccccc'
                    ) +
                    theme_bw() +
                    plt_theme +
                    theme(legend.position="bottom") +
                    labs( x = '', y = '' )+
                    theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )


#Guaradando gráfica en formato png------------------------------------------------------------------
ggsave( plot = plt_com_pri_apo, 
        filename = paste0( parametros$resultado_graficos, 'iess_grafico_tasas_aporte', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()