message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de pagos de indemnizaciones de RTR' )


#Cargando información ------------------------------------------------------------------------------
file<-paste0(parametros$Data_seg, 'IESS_RTR_indemnizaciones_prestaciones.xlsx' )


#Carga de indemnizaciones pagadas de rtr mensualmente-----------------------------------------------
indemnizaciones_rtr <- read_excel(file,
                                  sheet = 1,
                                  col_names = TRUE,
                                  col_types = NULL,
                                  na = "",
                                  skip = 0) %>% clean_names()


indemnizaciones_rtr<-indemnizaciones_rtr%>%
  select(-cod_sexo,
         -nombres,
         -fecha_derecho,
         -fecha_nacimiento,
         -fecha_acuerdo) %>%
  mutate(fecha_nacimiento=as.Date(as.character(indemnizaciones_rtr$fecha_nacimiento),
                           "%Y-%m-%d"),
         fecha_acu=as.Date(as.character(indemnizaciones_rtr$fecha_acuerdo), 
                           "%Y-%m-%d")) %>%
  mutate(anio=year(fecha_acu),
         mes = month( fecha_acu ) ) %>%
  mutate( sexo = if_else(sexo == "FEMENINO", "M", "H") ) %>%
  mutate( fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                      as.Date( "1976-03-05", "%Y-%m-%d"),
                                      fecha_nacimiento ) ) %>%
  filter( anio <= 2018, anio >= 2012 )



indemnizaciones_rt_2018 <- indemnizaciones_rtr
#Lectura de Indemnizaciones a 2022------------------------------------------------------------------

#Cargando base del RC-------------------------------------------------------------------------------

load(paste0(parametros$RData, "IESS_Reg_Civil.RData"))
rc <- rc %>%
  dplyr::select( cedula,
                 fecha_nacimiento )

message( '\tLeyendo Indemnizaciones de SGRT' )

file_indemnizaciones <- paste0( parametros$Data_seg, 'IESS-DSGRT-2022-1882-M/indemnizaciones.csv' )

indemnizaciones_rt_2022 <- (read.table(file_indemnizaciones,
                           skip=0,
                           dec = ".",
                           header = TRUE,
                           sep = ";",
                           na.strings = "NA",
                           encoding="UTF-8",
                           row.names = NULL )) %>% clean_names() %>%
  filter( tipo_prestacion == 'ID' ) %>%
  dplyr::select( anio,
                 mes,
                 cedula,
                 sexo := des_sexo,
                 -fecha_nacimiento,
                 provincia,
                 promedio_sueldo_teorico,
                 valor_pension_teorica_ajustada,
                 valor_pension_concedida,
                 liquido_a_pagar ) %>%
  mutate( sexo = if_else(sexo == "FEMENINO", "M", "H") ) %>%
  mutate( cedula = as.character( cedula ) ) %>%
  mutate( cedula = ifelse( nchar( cedula )==9,
                           paste0( "0",cedula ),
                           cedula ) ) %>%
  left_join(., rc, by = 'cedula' ) %>%
  mutate( fecha_nacimiento = if_else( is.na( fecha_nacimiento ),
                                     as.Date( "1982-01-29", "%Y-%m-%d"),
                                     fecha_nacimiento ) ) %>%
  filter( anio >= 2019, anio <= 2021 )
  




#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando data.frame' )

save( indemnizaciones_rt_2018,
      indemnizaciones_rt_2022,
      file = paste0( parametros$RData_seg, 'IESS_RTR_indemnizaciones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
 