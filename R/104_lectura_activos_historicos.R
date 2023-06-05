message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de las proyecciones de la masa salarial y afiliados del SGO' )

#Path a la carpeta de inputs------------------------------------------------------------------------
path <- "Y:/IESS_2020/Data/IVM/INPUT/ESCENARIO_BASE/DEMOGRAPHIC/TRANSITION_PROBABILITIES/"

#Definir codificación de mujer y hombre-------------------------------------------------------------
male <- 'H'
female <- 'M'

#Probabilidad de transición-------------------------------------------------------------------------
## Lectura de probabilidad de transición q^{s}_{g,x,t}-----------------------------------------------

file <- paste0( path, 'activos_edades_2021_2020_IVM.csv')
matriz_actividad <- read.table(file,
                       sep = ",",
                       dec = ".",
                       na = "NA",
                       header = TRUE,
                       #nrows = 100000,
                       skip = 0 ) %>% clean_names() %>%
  mutate_at( c( 6:ncol(.) ), as.integer ) %>%
  mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%d/%m/%Y" ),
          fecha_muerte = as.Date( fecha_muerte, "%d/%m/%Y" ) ) %>%
  mutate( sexo = if_else( sexo == '1', 
                          male,
                          female ) ) 

#Cotizantes por al menos una vez al año-------------------------------------------------------------

# actgxc_hist <- matriz_actividad %>%
#   mutate_if( is.numeric, replace_na, 1 ) %>%
#   mutate( x2012 = apply(.[grep("2012", names(.))], 1, prod ) ) %>%
#   mutate( x2012 = ifelse( x2012 == '1',
#                            NA,
#                           round( ( as.Date("2012-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2013 = apply(.[grep("2013", names(.))], 1, prod ) ) %>%
#   mutate( x2013 = ifelse( x2013 == '1',
#                           NA,
#                           round( ( as.Date("2013-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2014 = apply(.[grep("2014", names(.))], 1, prod ) ) %>%
#   mutate( x2014 = ifelse( x2014 == '1',
#                           NA,
#                           round( ( as.Date("2014-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2015 = apply(.[grep("2015", names(.))], 1, prod ) ) %>%
#   mutate( x2015 = ifelse( x2015 == '1',
#                           NA,
#                           round( ( as.Date("2015-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2016 = apply(.[grep("2016", names(.))], 1, prod ) ) %>%
#   mutate( x2016 = ifelse( x2016 == '1',
#                           NA,
#                           round( ( as.Date("2016-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2017 = apply(.[grep("2017", names(.))], 1, prod ) ) %>%
#   mutate( x2017 = ifelse( x2017 == '1',
#                           NA,
#                           round( ( as.Date("2017-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2018 = apply(.[grep("2018", names(.))], 1, prod ) ) %>%
#   mutate( x2018 = ifelse( x2018 == '1',
#                           NA,
#                           round( ( as.Date("2018-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2019 = apply(.[grep("2019", names(.))], 1, prod ) ) %>%
#   mutate( x2019 = ifelse( x2019 == '1',
#                           NA,
#                           round( ( as.Date("2019-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   mutate( x2020 = apply(.[grep("2020", names(.))], 1, prod ) ) %>%
#   mutate( x2020 = ifelse( x2020 == '1',
#                           NA,
#                           round( ( as.Date("2020-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
#   dplyr::select( sexo,
#                  x2012,
#                  x2013,
#                  x2014,
#                  x2015,
#                  x2016,
#                  x2017,
#                  x2018,
#                  x2019,
#                  x2020 ) %>%
#   gather( ., key = 't', value = 'x', x2012:x2020, factor_key = FALSE ) %>%
#   mutate( t = as.integer( gsub('x','', t ) ) ) %>%
#   filter( !is.na( x ) ) %>%
#   group_by( t, sexo, x ) %>%
#   mutate( actgxc = n() ) %>%
#   ungroup( ) %>%
#   distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
#   dplyr::select( t, sexo, x, actgxc )
# 
# 

actgxc_hist_1 <- matriz_actividad %>%
  mutate_if( is.numeric, replace_na, 1 ) %>%
  mutate( x2012 = apply(.[grep("2012", names(.))], 1, prod ) ) %>%
  mutate( x2012 = ifelse( x2012 == '1',
                          NA,
                          round( ( as.Date("2012-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  mutate( x2013 = apply(.[grep("2013", names(.))], 1, prod ) ) %>%
  mutate( x2013 = ifelse( x2013 == '1',
                          NA,
                          round( ( as.Date("2013-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  mutate( x2014 = apply(.[grep("2014", names(.))], 1, prod ) ) %>%
  mutate( x2014 = ifelse( x2014 == '1',
                          NA,
                          round( ( as.Date("2014-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  dplyr::select( sexo,
                 x2012,
                 x2013,
                 x2014 ) %>%
  gather( ., key = 't', value = 'x', x2012:x2014, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( actgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, actgxc )




actgxc_hist_2 <- matriz_actividad %>%
  mutate_if( is.numeric, replace_na, 1 ) %>%
  mutate( x2015 = apply(.[grep("2015", names(.))], 1, prod ) ) %>%
  mutate( x2015 = ifelse( x2015 == '1',
                          NA,
                          round( ( as.Date("2015-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  mutate( x2016 = apply(.[grep("2016", names(.))], 1, prod ) ) %>%
  mutate( x2016 = ifelse( x2016 == '1',
                          NA,
                          round( ( as.Date("2016-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  mutate( x2017 = apply(.[grep("2017", names(.))], 1, prod ) ) %>%
  mutate( x2017 = ifelse( x2017 == '1',
                          NA,
                          round( ( as.Date("2017-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  dplyr::select( sexo,
                 x2015,
                 x2016,
                 x2017 ) %>%
  gather( ., key = 't', value = 'x', x2015:x2017, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( actgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, actgxc )




actgxc_hist_3 <- matriz_actividad %>%
  dplyr::select( c(1:5), c(70:113) ) %>%
  mutate_if( is.numeric, replace_na, 1 ) %>%
  mutate( x2018 = apply(.[grep("2018", names(.))], 1, prod ) ) %>%
  mutate( x2018 = ifelse( x2018 == '1',
                          NA,
                          round( ( as.Date("2018-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  mutate( x2019 = apply(.[grep("2019", names(.))], 1, prod ) ) %>%
  mutate( x2019 = ifelse( x2019 == '1',
                          NA,
                          round( ( as.Date("2019-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  mutate( x2020 = apply(.[grep("2020", names(.))], 1, prod ) ) %>%
  mutate( x2020 = ifelse( x2020 == '1',
                          NA,
                          round( ( as.Date("2020-12-31") - fecha_nacimiento ) / 360 ) ) ) %>%
  dplyr::select( sexo,
                 x2018,
                 x2019,
                 x2020 ) %>%
  gather( ., key = 't', value = 'x', x2018:x2020, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( actgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, actgxc )



actgxc_hist <- rbind( actgxc_hist_1,
                      actgxc_hist_2,
                      actgxc_hist_3 )


#Salidas de cotizantes al año, sin fallecidos-------------------------------------------------------

# inactgxc_hist <- matriz_actividad %>%
#   mutate_if( is.numeric, replace_na, 1 ) %>%
#   mutate( fecha_muerte = if_else( is.na( fecha_muerte ),
#                                  as.Date("2099-12-31" ),
#                                  fecha_muerte ) ) %>% 
#   mutate( x2012 = apply(.[grep("2012", names(.))], 1, prod ) ) %>%
#   mutate( x2012 = ifelse( x2012 == '1' & (fecha_muerte < as.Date("2012-01-01") | fecha_muerte > as.Date("2012-12-31")),
#                           round( ( as.Date("2012-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2013 = apply(.[grep("2013", names(.))], 1, prod ) ) %>%
#   mutate( x2013 = ifelse( x2013 == '1' & (fecha_muerte < as.Date("2013-01-01") | fecha_muerte > as.Date("2013-12-31")),
#                           round( ( as.Date("2013-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2014 = apply(.[grep("2014", names(.))], 1, prod ) ) %>%
#   mutate( x2014 = ifelse( x2014 == '1' & (fecha_muerte < as.Date("2014-01-01") | fecha_muerte > as.Date("2014-12-31")),
#                           round( ( as.Date("2014-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2015 = apply(.[grep("2015", names(.))], 1, prod ) ) %>%
#   mutate( x2015 = ifelse( x2015 == '1' & (fecha_muerte < as.Date("2015-01-01") | fecha_muerte > as.Date("2015-12-31")),
#                           round( ( as.Date("2015-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2016 = apply(.[grep("2016", names(.))], 1, prod ) ) %>%
#   mutate( x2016 = ifelse( x2016 == '1' & (fecha_muerte < as.Date("2016-01-01") | fecha_muerte > as.Date("2016-12-31")),
#                           round( ( as.Date("2016-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2017 = apply(.[grep("2017", names(.))], 1, prod ) ) %>%
#   mutate( x2017 = ifelse( x2017 == '1' & (fecha_muerte < as.Date("2017-01-01") | fecha_muerte > as.Date("2017-12-31")),
#                           round( ( as.Date("2017-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2018 = apply(.[grep("2018", names(.))], 1, prod ) ) %>%
#   mutate( x2018 = ifelse( x2018 == '1' & (fecha_muerte < as.Date("2018-01-01") | fecha_muerte > as.Date("2018-12-31")),
#                           round( ( as.Date("2018-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2019 = apply(.[grep("2019", names(.))], 1, prod ) ) %>%
#   mutate( x2019 = ifelse( x2019 == '1' & (fecha_muerte < as.Date("2019-01-01") | fecha_muerte > as.Date("2019-12-31")),
#                           round( ( as.Date("2019-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   mutate( x2020 = apply(.[grep("2020", names(.))], 1, prod ) ) %>%
#   mutate( x2020 = ifelse( x2020 == '1' & (fecha_muerte < as.Date("2020-01-01") | fecha_muerte > as.Date("2020-12-31")),
#                           round( ( as.Date("2020-12-31") - fecha_nacimiento ) / 360 ),
#                           NA ) ) %>%
#   dplyr::select( sexo,
#                  x2012,
#                  x2013,
#                  x2014,
#                  x2015,
#                  x2016,
#                  x2017,
#                  x2018,
#                  x2019,
#                  x2020 ) %>%
#   gather( ., key = 't', value = 'x', x2012:x2020, factor_key = FALSE ) %>%
#   mutate( t = as.integer( gsub('x','', t ) ) ) %>%
#   filter( !is.na( x ) ) %>%
#   group_by( t, sexo, x ) %>%
#   mutate( inactgxc = n() ) %>%
#   ungroup( ) %>%
#   distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
#   dplyr::select( t, sexo, x, inactgxc )

  

inactgxc_hist_1 <- matriz_actividad %>%
  dplyr::select( c(1:5), c(6:45) ) %>%
  mutate_if( is.numeric, replace_na, 1 ) %>%
  mutate( fecha_muerte = if_else( is.na( fecha_muerte ),
                                  as.Date("2099-12-31" ),
                                  fecha_muerte ) ) %>% 
  mutate( x2012 = apply(.[grep("2012", names(.))], 1, prod ) ) %>%
  mutate( x2012 = ifelse( x2012 == '1' & (fecha_muerte < as.Date("2012-01-01") | fecha_muerte > as.Date("2012-12-31")),
                          round( ( as.Date("2012-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2013 = apply(.[grep("2013", names(.))], 1, prod ) ) %>%
  mutate( x2013 = ifelse( x2013 == '1' & (fecha_muerte < as.Date("2013-01-01") | fecha_muerte > as.Date("2013-12-31")),
                          round( ( as.Date("2013-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2014 = apply(.[grep("2014", names(.))], 1, prod ) ) %>%
  mutate( x2014 = ifelse( x2014 == '1' & (fecha_muerte < as.Date("2014-01-01") | fecha_muerte > as.Date("2014-12-31")),
                          round( ( as.Date("2014-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  dplyr::select( sexo,
                 x2012,
                 x2013,
                 x2014 ) %>%
  gather( ., key = 't', value = 'x', x2012:x2014, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( inactgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, inactgxc )



inactgxc_hist_2 <- matriz_actividad %>%
  dplyr::select( c(1:5), c(40:80) ) %>%
  mutate_if( is.numeric, replace_na, 1 ) %>%
  mutate( fecha_muerte = if_else( is.na( fecha_muerte ),
                                  as.Date("2099-12-31" ),
                                  fecha_muerte ) ) %>% 
  mutate( x2015 = apply(.[grep("2015", names(.))], 1, prod ) ) %>%
  mutate( x2015 = ifelse( x2015 == '1' & (fecha_muerte < as.Date("2015-01-01") | fecha_muerte > as.Date("2015-12-31")),
                          round( ( as.Date("2015-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2016 = apply(.[grep("2016", names(.))], 1, prod ) ) %>%
  mutate( x2016 = ifelse( x2016 == '1' & (fecha_muerte < as.Date("2016-01-01") | fecha_muerte > as.Date("2016-12-31")),
                          round( ( as.Date("2016-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2017 = apply(.[grep("2017", names(.))], 1, prod ) ) %>%
  mutate( x2017 = ifelse( x2017 == '1' & (fecha_muerte < as.Date("2017-01-01") | fecha_muerte > as.Date("2017-12-31")),
                          round( ( as.Date("2017-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  dplyr::select( sexo,
                 x2015,
                 x2016,
                 x2017 ) %>%
  gather( ., key = 't', value = 'x', x2015:x2017, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( inactgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, inactgxc )



inactgxc_hist_3 <- matriz_actividad %>%
  dplyr::select( c(1:5), c(70:113) ) %>%
  mutate_if( is.numeric, replace_na, 1 ) %>%
  mutate( fecha_muerte = if_else( is.na( fecha_muerte ),
                                  as.Date("2099-12-31" ),
                                  fecha_muerte ) ) %>% 
  mutate( x2018 = apply(.[grep("2018", names(.))], 1, prod ) ) %>%
  mutate( x2018 = ifelse( x2018 == '1' & (fecha_muerte < as.Date("2018-01-01") | fecha_muerte > as.Date("2018-12-31")),
                          round( ( as.Date("2018-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2019 = apply(.[grep("2019", names(.))], 1, prod ) ) %>%
  mutate( x2019 = ifelse( x2019 == '1' & (fecha_muerte < as.Date("2019-01-01") | fecha_muerte > as.Date("2019-12-31")),
                          round( ( as.Date("2019-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2020 = apply(.[grep("2020", names(.))], 1, prod ) ) %>%
  mutate( x2020 = ifelse( x2020 == '1' & (fecha_muerte < as.Date("2020-01-01") | fecha_muerte > as.Date("2020-12-31")),
                          round( ( as.Date("2020-12-31") - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  dplyr::select( sexo,
                 x2018,
                 x2019,
                 x2020 ) %>%
  gather( ., key = 't', value = 'x', x2018:x2020, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( inactgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, inactgxc )


inactgxc_hist <- rbind( inactgxc_hist_1,
                        inactgxc_hist_2,
                        inactgxc_hist_3 )


#Nuevos fallecidos al año---------------------------------------------------------------------------
  
ndeathactgxc_hist <- matriz_actividad %>%
  dplyr::select( -cedula ) %>%
  filter( !is.na( fecha_muerte ) ) %>%
  mutate( x2012 = ifelse( fecha_muerte >= as.Date("2012-01-01") & fecha_muerte <= as.Date("2012-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2013 = ifelse( fecha_muerte >= as.Date("2013-01-01") & fecha_muerte <= as.Date("2013-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2014 = ifelse( fecha_muerte >= as.Date("2014-01-01") & fecha_muerte <= as.Date("2014-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2015 = ifelse( fecha_muerte >= as.Date("2015-01-01") & fecha_muerte <= as.Date("2015-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2016 = ifelse( fecha_muerte >= as.Date("2016-01-01") & fecha_muerte <= as.Date("2016-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2017 = ifelse( fecha_muerte >= as.Date("2017-01-01") & fecha_muerte <= as.Date("2017-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2018 = ifelse( fecha_muerte >= as.Date("2018-01-01") & fecha_muerte <= as.Date("2018-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2019 = ifelse( fecha_muerte >= as.Date("2019-01-01") & fecha_muerte <= as.Date("2019-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  mutate( x2020 = ifelse( fecha_muerte >= as.Date("2020-01-01") & fecha_muerte <= as.Date("2020-12-31"),
                          round( ( fecha_muerte - fecha_nacimiento ) / 360 ),
                          NA ) ) %>%
  dplyr::select( sexo,
                 x2012,
                 x2013,
                 x2014,
                 x2015,
                 x2016,
                 x2017,
                 x2018,
                 x2019,
                 x2020 ) %>%
  gather( ., key = 't', value = 'x', x2012:x2020, factor_key = FALSE ) %>%
  mutate( t = as.integer( gsub('x','', t ) ) ) %>%
  filter( !is.na( x ) ) %>%
  group_by( t, sexo, x ) %>%
  mutate( ndeathactgxc = n() ) %>%
  ungroup( ) %>%
  distinct( ., sexo, t, x, .keep_all = TRUE ) %>%
  dplyr::select( t, sexo, x, ndeathactgxc )

  

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en un solo data.frame' )

save( actgxc_hist,
      ndeathactgxc_hist,
      inactgxc_hist,
      file = paste0( parametros$RData, 'IESS_act_ces__fall_hist.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
