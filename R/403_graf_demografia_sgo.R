message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_SGO_masa_afiliados.RData' ) )
load( file = paste0( parametros$RData, 'IESS_pir_salarios.RData' ) )

#Serie afiliados mensuales--------------------------------------------------------------------------
message( '\tGraficando afiliados en el tiempo del SGO del IESS' )

aux <- afi_mensual %>%
  filter( anio <= 2022 ) %>%
  mutate( periodo = as.Date( paste0("01/",mes,"/",anio), "%d/%m/%Y"))

y_lim <- c( 0, 4000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_afi <- ggplot( data = aux, aes( x = periodo, y = afiliados ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Cotizantes al SGO' ) +
  scale_x_date( breaks = seq(as.Date("2005-12-01"), as.Date("2022-12-01"), by="12 months"),
                date_labels = '%b %Y',
                limits = as.Date( c("2005-12-01", "2022-12-01" ), "%Y-%m-%d")  ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_afi, 
        filename = paste0( parametros$resultado_graficos, 'iess_afi', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Masa salarial--------------------------------------------------------------------------------------
message( '\tGraficando masa salarial del SGO del IESS' )

aux <- masa_salarial %>%
  mutate( periodo = as.Date( paste0("01/","01","/",anio), "%d/%m/%Y")) %>%
  mutate( id = 'masa salarial')

x_lim <- c( 2005, 2022 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 30000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial <- ggplot() +
  geom_bar( data = aux, 
            aes( x = anio, 
                 y = masa_salarial, 
                 fill = id ),
            stat = 'identity', colour = 'black', size = 0.1 ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl  ) +
  xlab( 'Año' ) +
  ylab( 'Masa Salarial (millones de dólares)' ) +
  theme_bw( ) +
  plt_theme +
  guides( fill = guide_legend( title = NULL,
                               label.position = "right", 
                               label.hjust = 0, 
                               label.vjust = 0.5,
                               reverse = TRUE) )+
  scale_fill_manual( values = c( parametros$iess_green ),
                     labels = c( "masa salarial") ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave( plot = iess_masa_salarial, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide de edades de los afiliados----------------------------------------------------------------
message( '\tGraficando población afiliada activa inicial por edad y sexo SGO del IESS' )

aux <- afi_edad_sexo %>%
  mutate( fdp_m = -fdp_m ) %>%
  dplyr::select( edad,
                 H:= fdp_m,
                 M:= fdp_f ) %>%
  gather( .,
          key = "sexo",
          value = "n",
          H,
          M ) %>%
  filter( edad >= 15,
          edad <= 90 ) %>%
  arrange( sexo, edad )

salto_y<-10
salto_x<-0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_afiliados<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter(sexo == 'M'), stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux %>% filter(sexo == 'H'), stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom") +
  scale_fill_manual(values = c( parametros$male, parametros$female ),
                    labels = c( "Hombres", "Mujeres")) +
  guides( color = guide_colorbar(order = 1),
          fill = guide_legend(order = 1) )

ggsave( plot = iess_pir_afiliados, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámide de Salarios------------------------------------------------------------------------------
message( '\tGraficando masa salarial por monto y sexo SGO del IESS' )
aux<-copy( masa_sal_edad_monto_ini )
aux<-aux[cat=="afi"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE),by=sexo]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,2]]
aux[sexo=="M", n:=n/N[2,2]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x
salto_y<-200
salto_x<-0.05
brks_y <- seq(-0.40,0.40,salto_x)
lbls_y <- paste0(as.character(c(seq(0.40, 0, -salto_x)*100, seq(salto_x, 0.40, salto_x)*100)), "%")
brks_x <- seq(100,1600,salto_y)
nb <- length(brks_x)-1   
lbls_x <- c(paste0(formatC(c(brks_x[1:nb]),
                           digits = 0, 
                           format = 'f', 
                           big.mark = '.', 
                           decimal.mark = ',')),"mayor a 1.500")

iess_pir_masa_salarial<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Salario (USD)' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ],
            stat = 'identity',
            colour="white",  
            size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], 
            stat = 'identity',
            colour="white",  
            size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_masa_salarial, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

