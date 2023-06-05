message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
message( '\tCargando datos' )
load( paste0( parametros$RData, 'ONU_proyeccion_poblacion.RData' ) )
load( paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )
load( paste0( parametros$RData, 'IESS_contexto_economico.RData' ) )
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )

#1. Evolución histórica-----------------------------------------------------------------------------
##Evolución histórica del índice de precios (IPC)----------------------------------------------------
message( '\tGraficando análisis de contexto' )
aux <- inflacion %>%
  filter( anio >= '2003', 
          anio <= '2022' ) %>%
  dplyr::select( periodo,
                 ipc,
                 inflacion_mensual,
                 inflacion_variacion_anual ) %>%
  arrange( periodo )

y_lim <- c( 0, 125 )
y_brk <- seq( 0, y_lim[2], by = 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_brk_dual <- seq( -2, 10, by = 2 )
y_lbl_dual <- paste0( y_brk_dual, "%" )

iess_inflacion  <- ggplot( data = aux, aes( x = periodo ) ) +
  geom_line( aes( y = 10 * inflacion_variacion_anual + 20 ,
                  group = 1L,
                  color="Inflación Acumulada" ),
             size = graf_line_size ) +
  geom_line( aes( y = ipc,
                  group = 1L,
                  color="aIPC" ), 
             size = graf_line_size ) +
  scale_x_date( breaks = seq(as.Date("2002-12-01"), as.Date("2022-12-01"), by="24 months"),
                date_labels = '%b %Y',
                limits = as.Date( c("2002-12-01", "2022-12-01" ), "%Y-%m-%d")  ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim,
                      sec.axis = sec_axis( ~.*(1/10) - 2,
                                           name = "Inflación Acumulada",
                                           breaks = y_brk_dual,
                                           labels = y_lbl_dual ) ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ), 
                      labels = c( 'IPC', 'Inflación Acumulada' ) ) +
  theme_bw( ) +
  plt_theme +
  labs( x = 'Año', y = 'IPC') +
  theme( legend.position = "bottom" )  +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) )



ggsave( plot = iess_inflacion,
        filename = paste0( parametros$resultado_graficos, 'iess_inflacion', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



##Evolución histórica del desempleo-----------------------------------------------------------------
message( '\tGraficando análisis de desempleo' )

aux <- desempleo %>%
  filter( anio >= '2007', 
          anio <= '2022' ) %>%
  dplyr::select( periodo,
                 desempleo_nacional,
                 empleo_adecuado_pleno_n ) %>%
  arrange( periodo )

scl = 0.1  # escala de millones
hmts = 1.1 #homotecia

y_lim <- c( 3, 7 )
y_brk <- seq( 0, y_lim[2], by = 0.5 )
y_lbl <- paste0( y_brk, "%" )

y_brk_dual <- seq( 20, 60, by = 5 )
y_lbl_dual <- paste0( y_brk_dual, "%" )

iess_desempleo  <- ggplot( data = aux, aes( x = periodo ) ) +
  geom_line( aes( y = scl * hmts * empleo_adecuado_pleno_n + 0.9,
                  group = 1L,
                  color="empleo_adecuado_pleno_n" ),
             size = graf_line_size ) +
  geom_line( aes( y = desempleo_nacional,
                  group = 1L,
                  color="adesempleo_nacional" ), 
             size = graf_line_size ) +
  scale_x_date( breaks = seq(as.Date("2007-12-01"), as.Date("2022-12-01"), by="18 months"),
                date_labels = '%b %Y',
                limits = as.Date( c("2007-12-01", "2022-12-01" ), "%Y-%m-%d")  ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim,
                      sec.axis = sec_axis( ~./( scl*hmts ) - 8.181818,
                                           name = "Empleo Pleno",
                                           breaks = y_brk_dual,
                                           labels = y_lbl_dual ) ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'Tasa Desempleo', 'Tasa de empleo pleno' ) ) +
  theme_bw( ) +
  plt_theme +
  labs( x = 'Año', y = 'Tasa Desempleo') +
  theme( legend.position = "bottom" ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) )


ggsave( plot = iess_desempleo,
        filename = paste0( parametros$resultado_graficos, 'iess_desempleo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


##Evolución del salario básico unificado (SBU)------------------------------------------------------

aux <- sbu %>%
  filter( anio <= '2022')

x_lim <- c( 2000, 2022 )
x_brk <- seq( x_lim[1] , x_lim[2], 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 450 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sbu <- ggplot( data = aux ) +
  geom_line( aes(x = anio,
                 y = sbu,
                 color = parametros$iess_green),
             size = graf_line_size ) +
  labs( x = 'Año', y = 'Salario Básico Unificado (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green ),
                      labels = c( '' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 0, hjust = 0.5, vjust=0.5 ), legend.position = 'none' )

ggsave( plot = iess_sbu,
        filename = paste0( parametros$resultado_graficos, 'iess_sbu', parametros$graf_ext ),
        width = graf_width , height = graf_height, units = graf_units, dpi = graf_dpi )

##Evolución histórica del salario promedio----------------------------------------------------------

aux <- salarios %>%
  na.omit(.) %>%
  filter( periodo >= as.Date("01/12/2006","%d/%m/%Y") )

x_lim <- c( 2005, 2022 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 800 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_salarios <- ggplot( data = aux,
                         aes( x = periodo ) ) +
  geom_line( aes( y = sal_prom ), 
             color = parametros$iess_green,
             size = graf_line_size ) +
  labs( x = 'Año', y = 'Salario promedio (USD)' ) +
  scale_x_date( breaks = seq(as.Date("2006-12-01"), as.Date("2022-12-01"), by="12 months"),
                date_labels = '%b %Y',
                limits = as.Date( c("2006-12-01", "2022-12-01" ), "%Y-%m-%d")  ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) )

ggsave( plot = iess_salarios,
        filename = paste0( parametros$resultado_graficos, 'iess_salarios', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Evolución histórica del PIB del Ecuador-----------------------------------------------------------

aux <- pib_real %>%
  filter( anio >= '2000', 
          anio <= '2022' ) %>%
  mutate( periodo = ymd( paste0(anio, '/01/01') ) ) %>%
  mutate( apib_constantes = pib_constantes  / 1000000) %>%
  mutate( var = 'PIB a precios constantes')

scl = 1000  # escala de millones
hmts = 3 #homotecia
  
x_lim <- c( 2000, 2022 )
x_brk <- seq( x_lim[1], x_lim[2], by = 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 0, 80000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_brk_dual <- seq( -9, 15, by = 3 )
y_lbl_dual <- paste0( y_brk_dual, "%" )


iess_pib_real <- ggplot(data = aux, 
                        aes( x = periodo,
                             y = apib_constantes ,
                             fill = var) ) +
  geom_bar( stat='identity',
            colour='black' ) +
  geom_line(data = aux,
            aes(x = periodo,
                y = crecimiento_pib*hmts*scl + 35000,
                group = 1,
                linetype = 'Tasa de crecimiento PIB'),
            inherit.aes = FALSE,
            size = 1 ) +
  scale_linetype_manual( NULL, values = 1 ) +
  scale_x_date( breaks = seq( as.Date("2000-01-01"), as.Date("2022-01-01"), by = "24 months"), 
                date_labels = '%Y',
                limits = as.Date( c("2000-0-01", "2022-12-31" ), "%Y-%m-%d") )+
  scale_y_continuous( name = 'PIB a precios constantes (millones de USD)',
                      labels = y_lbl, breaks = y_brk, limits = y_lim,
                      sec.axis = sec_axis(~./( scl*hmts ) - 11.66667,
                                          name = 'Tasa de crecimiento PIB real',
                                          labels = y_lbl_dual,
                                          breaks = y_brk_dual ) ) + 
  scale_color_manual( values =  c(  parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'PIB a precios constantes', 'Tasa de crecimiento PIB real' ) ) + 
  scale_fill_manual(values = c( parametros$iess_green, parametros$iess_blue ) )+
  theme_bw() +
  plt_theme+
  guides( color = guide_colorbar(order = 0),
          fill = guide_legend( order = 1 ) ) +
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', 
                                             colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines') )


ggsave( plot = iess_pib_real,
        filename = paste0( parametros$resultado_graficos, 'iess_pib_real', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


##Evolución histórica de las tasas de interés-------------------------------------------------------
message( '\tGraficando tasas de interés' )

aux <- tasas_interes %>%
  filter( anio >= '2003', 
          anio <= '2022' ) %>%
  arrange( periodo )

y_lim <- c( 2, 16 )
y_brk <- seq( 0, y_lim[2], by = 2 )
y_lbl <- paste0( formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ), "%")

iess_tasas_interes  <- ggplot( data = aux, aes( x = periodo ) ) +
  geom_line( aes( y = tasa_activa,
                  group = 1L,
                  color="tasa_activa" ),
             size = graf_line_size) +
  geom_line( aes( y = tasa_pasiva        ,
                  group = 1L,
                  color="tasa_pasiva" ), 
             size = graf_line_size ) +
  scale_x_date(date_breaks = '2 year', date_labels = "%B-%Y") +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_color_manual( values =  c( parametros$iess_blue, parametros$iess_green ), 
                      labels = c( 'Tasa activa referencial', 'Tasa pasiva referencial' ) ) +
  theme_bw( ) +
  plt_theme +
  labs( x = 'Año', y = '') +
  theme( legend.position = "bottom" )  +
  theme( axis.text.x=element_text( angle = 90, hjust = 0 ) )


ggsave( plot = iess_tasas_interes,
        filename = paste0( parametros$resultado_graficos, 'iess_tasas_interes', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Portafolio de inversiones del BIESS----------------------------------------------------------------
message( '\tGraficando evolución histórica de las inversiones del BIESS' )

aux <- rendimiento_biess %>%
  #filter( fecha <= as.Date("01/12/2022", "%d/%m/%Y" ) ) %>%
  mutate( rendimiento = rendimiento * 100,
          instrumento = 'Fondos administrados BIESS') %>%
  dplyr::select( fecha,
                 mes,
                 f_adm,
                 rendimiento,
                 instrumento ) %>%
  na.omit( . )

df_bar <- aux %>% select(-rendimiento)
df_line = aux %>% select(fecha, rendimiento)

scl = 1000000  # escala de millones
hmts = 30 #homotecia

y_lim <- c( 0, 25000000000 )
y_brk <- seq( y_lim[1], y_lim[2], 3000000000 )
y_lbl <- formatC( y_brk/scl, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim_dual <- c( 0, 18 )
ydual_brk <- seq( 0, 18, 2)
ydual_lbl <- paste0(formatC( ydual_brk, 
                             digits = 0, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")


biess_rendimiento <- ggplot( data = df_bar, 
                            aes( x = fecha,
                                 y = f_adm,
                                 fill = instrumento ) ) +
  geom_area( alpha = 0.7, size = 0.5, colour = "black" ) + 
  geom_line( data = df_line,
            aes( x = fecha,
                 y = rendimiento*hmts*scl*45,
                 group = 1,
                 linetype = 'Rendimiento Neto'),
            inherit.aes = FALSE,
            size = graf_line_size ) +
  scale_linetype_manual( NULL, values = 1) +
  scale_x_date( breaks = seq(as.Date("2011-12-01"), as.Date("2022-12-01"), by="12 months"),
                date_labels = '%b %Y',
                limits = as.Date( c("2011-12-01", "2022-12-01" ), "%Y-%m-%d")  ) +
  scale_y_continuous( name = 'Fondos administrados BIESS (millones USD)',
                      labels = y_lbl, breaks = y_brk, limits = y_lim,
                      sec.axis = sec_axis( ~./( scl*hmts*45 ),
                                          name = 'Rendimiento Neto',
                                          labels = ydual_lbl,
                                          breaks = ydual_brk ) ) + 
  scale_fill_manual( values = c( parametros$iess_green,
                                 parametros$iess_blue ) )+
  theme_bw( ) +
  plt_theme+
  guides( fill = guide_legend( title = NULL,
                               label.position = "right", 
                               label.hjust = 0 ) )+
  theme( legend.position = 'bottom' ) +
  labs( x = '', y = '' )+
  theme( legend.background = element_rect( fill = 'transparent' ),
         legend.box.background = element_rect( fill = 'transparent', 
                                               colour = NA ),
         legend.key = element_rect( fill = 'transparent' ), 
         legend.spacing = unit( -1, 'lines' ) ) +
  theme( axis.text.x=element_text( angle = 90, hjust = 0 ) )


ggsave( plot = biess_rendimiento, 
        filename = paste0( parametros$resultado_graficos, 'biess_rendimiento', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#2. Predicciones------------------------------------------------------------------------------------
##Predicciones Salario básico unificado-------------------------------------------------------------

aux <- tasas_macro_pred
aux_his <- aux %>% filter( anio <= 2022 )
aux_pred <- aux %>% filter( anio >= 2022 )

lim_y<- c( 0 ,300000000 )
salto_y = 50000000
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- formatC( brks_y, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pib_pred <- ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = pib_nominal  ,group=1 ),
             size = graf_line_size,colour= parametros$iess_green) +
  geom_line( data = aux_pred, aes( x = anio, y = pib_nominal  , group=1 ),
             size = graf_line_size,colour = parametros$iess_green,linetype = "dashed") +
  xlab("Años")+
  ylab("PIB nominal (miles USD)") +
  scale_y_continuous( breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_pib_pred,
        filename = paste0( parametros$resultado_graficos, 'iess_pib_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Predicciones Salario promedio anual---------------------------------------------------------------

aux <- tasas_macro_pred
aux_his <- aux %>% filter( anio <= 2022 )
aux_pred <- aux %>% filter( anio >= 2022 )

lim_y<- c(0,25000)
salto_y = 5000
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- formatC( brks_y, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sal_pred <- ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = sal_anual,group=1 ),
             size = graf_line_size,colour= parametros$iess_green) +
  geom_line( data = aux_pred, aes( x = anio, y = sal_anual, group=1 ),
             size = graf_line_size,colour = parametros$iess_green,linetype = "dashed") +
  xlab("Años")+
  ylab("Salario promedio anual (USD)") +
  scale_y_continuous( breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_sal_pred,
        filename = paste0( parametros$resultado_graficos, 'iess_sal_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



##Predicciones  del salario básico unificado--------------------------------------------------------

message( '\tGraficando salario básico unificado' )
aux<- tasas_macro_pred
aux_his <- aux %>% filter( anio <= 2022 )
aux_pred <- aux %>% filter( anio >= 2022 )

lim_y<- c( 0, 1200 )
salto_y <- 100
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- formatC( brks_y, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_sbu_pred <- ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = sbu, group=1),
             size = graf_line_size, colour = parametros$iess_green ) +
  geom_line( data = aux_pred, aes( x = anio, y = sbu, group=1),
             size = graf_line_size,  colour = parametros$iess_green,linetype = "dashed") +
  xlab("Años")+
  ylab("SBU (USD)") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_sbu_pred,
        filename = paste0( parametros$resultado_graficos, 'iess_sbu_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


##Predicciones tasa activa--------------------------------------------------------------------------

message( '\tGraficando tasa activa' )
aux <- tasas_macro_pred
aux_his <- aux %>% filter( anio <= 2022 )
aux_pred <- aux %>% filter( anio >= 2022 )

lim_y<- c(0.07,0.11)
salto_y = 0.005
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), abs(lim_y[2]), salto_y)*100) ), "%" )

iess_ta_pred <- ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = tasa_activa, group=1 ),
             size = graf_line_size,colour = parametros$iess_green ) +
  geom_line( data = aux_pred, aes( x = anio, y = tasa_activa, group=1),
             size = graf_line_size, colour = parametros$iess_green, linetype = "dashed") +
  xlab("Años")+
  ylab("Tasa activa referencial") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_ta_pred,
        filename = paste0( parametros$resultado_graficos, 'iess_ta_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Predicciones tasa pasiva--------------------------------------------------------------------------
message( '\tGraficando tasa pasiva' )
aux <- tasas_macro_pred
aux_his <- aux %>% filter( anio <= 2022 )
aux_pred <- aux %>% filter( anio >= 2022 )

lim_y<- c(0.03,0.07)
salto_y = 0.01
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), abs(lim_y[2]), salto_y)*100) ), "%" )

iess_tp_pred <- ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = tasa_pasiva, group=1 ),
             colour = parametros$iess_green,size = graf_line_size ) +
  geom_line( data = aux_pred, aes( x = anio, y = tasa_pasiva, group=1 ),
             size = graf_line_size,colour = parametros$iess_green,size = graf_line_size, linetype = "dashed") +
  xlab("Años")+
  ylab("Tasa pasiva referencial") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_tp_pred,
        filename = paste0( parametros$resultado_graficos, 'iess_tp_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

##Predicciones inflación----------------------------------------------------------------------------
message( '\tGraficando inflación' )
aux <- tasas_macro_pred
aux_his <- aux %>% filter( anio <= 2022 )
aux_pred <- aux %>% filter( anio >= 2022 )
lim_y<- c(-0.01,0.09)
salto_y = 0.01
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(seq(lim_y[1], lim_y[2], salto_y)*100), "%")

iess_inf_pred <- ggplot() +
  geom_line( data = aux_his, aes( x = anio, y = inflación_prom, group=1),colour = parametros$iess_green,
             size = graf_line_size ) +
  geom_line( data = aux_pred, aes( x = anio, y = inflación_prom, group=1 ),
             size = graf_line_size, colour = parametros$iess_green,linetype = "dashed") +
  xlab("Años")+
  ylab("Tasa inflación promedio") +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = lim_y) +
  scale_x_continuous( labels = seq(2006, 2060, 9), breaks = seq(2006, 2060, 9)) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_inf_pred,
        filename = paste0( parametros$resultado_graficos, 'iess_inf_pred', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

## Prueba de Independencia de errores de Box-Ljung--------------------------------------------------
aux <- as_tibble( box_ljung )

lim_y<- c( 0, 0.9 )
salto_y = 0.1
brks_y <- seq(lim_y[1],lim_y[2],salto_y)
lbls_y <- paste0(as.character(seq(lim_y[1], lim_y[2], salto_y)*100), "%")


iess_box_ljung <- ggplot( aux, aes( x = m, y = p_valor )) +
  geom_segment( aes( x = m, xend = m,
                     y = 0, yend = p_valor ), 
                color = parametros$iess_blue, 
                linetype = 6 ) +
  geom_hline( yintercept = 0,  color = "black", size = 0.5 ) + 
  geom_hline( yintercept = 0.05,  color = "red", size = 0.5, linetype="dashed" ) + 
  geom_point( color = parametros$iess_green, size = 2) +
  scale_y_continuous( breaks = brks_y, labels = brks_y, limits = lim_y ) +
  scale_x_continuous( labels = seq( 1, 8, 1 ), breaks = seq( 1, 8, 1 ) ) +
  theme_bw() +
  plt_theme +
  xlab("m") +
  ylab("p-valor")


ggsave( plot = iess_box_ljung,
        filename = paste0( parametros$resultado_graficos, 'iess_box_ljung', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3. Las pirámides poblacionales de la población nacional -------------------------------------------

poblacion <- as_tibble( ONU_proyeccion_poblacion ) %>%
  filter( year %in% c( 2022, 2042, 2062 ),
          x != 'total' ) %>%
  group_by( year ) %>%
  mutate( rx = lx / sum( lx ) ) %>%
  ungroup( ) %>%
  mutate( rx = ifelse( sex == 'M',
                        ( -1 ) * rx,
                        rx ) ) %>%
  mutate( x := factor( x, levels = c( ' 0 - 4', ' 5 - 9', '10 - 14', '15 - 19', '20 - 24', 
                                      '25 - 29', '30 - 34', '35 - 39', '40 - 44', '45 - 49', '50 - 54', 
                                      '55 - 59', '60 - 64', '65 - 69', '70 - 74', '75 - 79', '80 - 84', 
                                      '85 - 89', '90 - 94', '95 - 99', '100+' ), ordered = TRUE ) ) %>%
  dplyr::select( sex, x, year, lx, rx ) %>%
  na.omit( . )

##Pirámide población -------------------------------------------------------------------------------
years <- c( 2022, 2042, 2062 )


for( yr in years ) {
  if ( yr == 2022 ) {
    y_lim <- c( -0.05, 0.05 )
    y_brk <- seq( y_lim[1], y_lim[2], 0.01 )
    y_lbl <- paste0( formatC( 100 * abs(y_brk), digits = 0, format = 'f', big.mark = '.', 
                              decimal.mark = ',' ), "%" )
  } else {
    y_lim <- c( -0.04, 0.04 )
    y_brk <- seq( y_lim[1], y_lim[2], 0.01 )
    y_lbl <- paste0( formatC( 100 * abs(y_brk), digits = 0, format = 'f', big.mark = '.', 
                              decimal.mark = ',' ), "%" )
  }
  aux <- poblacion %>% filter( year == yr )
  
  iess_pir_poblacion <- ggplot( aux, aes( x = x, y = rx, fill = sex ) ) +
    geom_bar( data = filter( aux, sex ==  'M' ), stat = 'identity', colour = "white", size = 0.1 ) +
    geom_bar( data = filter( aux, sex ==  'F' ), stat = 'identity', colour = "white", size = 0.1 ) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
    # scale_x_continuous( breaks = brks_x, labels = lbls_x, limits = x_lim ) +
    xlab( 'Edad' ) +
    ylab( '' ) +
    coord_flip( ) +
    theme_bw( ) +
    plt_theme +
    guides( fill = guide_legend( title = NULL,
                                 label.position = "right", 
                                 label.hjust = 0, 
                                 label.vjust = 0.5,
                                 reverse = TRUE) )+
    theme( legend.position = "bottom" ) +   #legend.position = c( 0.8, 0.2 )
    scale_fill_manual( values = c( parametros$female, parametros$male ),
                       labels = c( "Mujeres","Hombres" ) )
  
  ggsave( plot = iess_pir_poblacion,
          filename = paste0( parametros$resultado_graficos, 'iess_pir_poblacion_', yr, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
}

##Pirámide pea -------------------------------------------------------------------------------------
y <- c( 2022, 2042, 2058 )

pob_pea <- as_tibble( onu_pea_tot_int ) %>%
  filter( year %in% y,
          sex %in% c( 'F', 'M' ) ) %>%
  dplyr::select( year, sex, x, lx := pea_int ) %>%
  group_by( year ) %>%
  mutate( rx = lx / sum( lx ) ) %>%
  ungroup( ) %>%
  mutate( rx = if_else( sex == 'M',
                        (-1) * rx,
                        rx ) ) %>%
  ungroup( )


y_lim <- c( -0.0175, 0.0175 )
y_brk <- seq( -0.015, 0.015, 0.005 )
y_lbl <- paste0( formatC( 100 * abs(y_brk), digits = 1, format = 'f', big.mark = '.',
                          decimal.mark = ',' ), "%" )

x_lim <- c( 15, 100 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

for( yr in y ) {
  aux <- filter( pob_pea, year == yr )

  iess_pir_pea <- ggplot( aux, aes( x = x, y = rx, fill = sex ) ) +
    geom_bar( data = filter( aux, sex ==  'M' ), stat = 'identity',colour = "white", size = 0.1 ) +
    geom_bar( data = filter( aux, sex ==  'F' ), stat = 'identity',colour = "white", size = 0.1 ) +
    xlab( 'Edad' ) +
    ylab( '' ) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
    coord_flip( ) +
    theme_bw( ) +
    plt_theme +
    guides( fill = guide_legend( title = NULL,label.position = "right",
                                 label.hjust = 0, label.vjust = 0.5,reverse = TRUE ) )+
    theme( legend.position = "bottom" )+   #legend.position = c( 0.8, 0.2 )
    scale_fill_manual( values = c( parametros$female,parametros$male ),
                       labels = c( "Mujeres","Hombres" ) )

  ggsave( plot = iess_pir_pea,
          filename = paste0( parametros$resultado_graficos, 'iess_pir_pea_', yr, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
}
##Limpiar RAM---------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )