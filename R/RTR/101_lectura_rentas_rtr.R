message( paste( rep('-', 100 ), collapse = '' ) )

#Lectura de nomina----------------------------------------------------------------------------------

message( '\tLeyendo nomina de pensiones de SGRT desde 2019 a 2022' )

file_nomina <- paste0( parametros$Data_seg, 'IESS-DSGRT-2022-1882-M/nomina.csv' )

nomina_sgrt <- (read.table(file_nomina,skip=0,
                           dec = ".",
                           header = TRUE,
                           sep = ";",
                           na.strings = "NA",
                           encoding="UTF-8",
                           row.names = NULL )) %>% clean_names() %>%
  dplyr::select( anio,
                 mes,
                 cedula,
                 sexo,
                 fecha_nacimiento,
                 provincia,
                 cod_tipo_prestacion,
                 tipo_beneficiario,
                 rubro,
                 descripcion,
                 valor ) %>%
  mutate( fecha_nacimiento = as.Date( fecha_nacimiento, "%Y-%m-%d" ) ) %>%
  mutate( sexo = if_else(sexo == "Femenino", "M", "H") ) %>%
  filter( anio < 2022)

# Filtrar prestación permanente parcial-------------------------------------------------------------
prestaciones_pp_2022 <- nomina_sgrt %>%
  filter(cod_tipo_prestacion == "PP")

# Filtrar prestación permanente total---------------------------------------------------------------
prestaciones_pt_2022 <- nomina_sgrt %>%
  filter(cod_tipo_prestacion == "PT")

# Filtrar prestaciones permanente absoluta----------------------------------------------------------
prestaciones_pa_2022 <- nomina_sgrt %>%
  filter(cod_tipo_prestacion == "PA")

# Filtrar prestaciones de montepío viudez-----------------------------------------------------------
prestaciones_viudez_2022 <- nomina_sgrt %>%
  filter(cod_tipo_prestacion == 'VO') %>%
  filter( tipo_beneficiario %in% c( "Conviviente",
                                     "Conviviente Hombre",
                                     "Conviviente Mujer",
                                     "Viuda",
                                     "Viudo",
                                    "Hermano/a Soltero",
                                    "Madre",
                                    "MADRE",
                                    "Madre Invalida",
                                    "Padre Invalido") )

# Filtrar prestaciones de montepío orfandad---------------------------------------------------------
prestaciones_orfandad_2022 <- nomina_sgrt %>%
  filter(cod_tipo_prestacion == 'VO') %>%
  filter( tipo_beneficiario %in% c( "Hijo Invalido",
                                    "Hijo Normal",
                                    "Hijo/a" ) )

# Filtrar prestaciones de montepío padres-----------------------------------------------------------
prestaciones_padres_2022 <- nomina_sgrt %>%
  filter(cod_tipo_prestacion == 'VO') %>%
  filter( tipo_beneficiario %in% c( "Hermano/a Soltero",
                                    "Madre",
                                    "MADRE",
                                    "Madre Invalida",
                                    "Padre Invalido") )


nomina_rt_pre_2022 <- nomina_sgrt

# Guardando en un Rdata-----------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(nomina_rt_pre_2022,
     prestaciones_pp_2022,
     prestaciones_pt_2022,
     prestaciones_pa_2022,
     prestaciones_viudez_2022,
     prestaciones_orfandad_2022,
     prestaciones_padres_2022,
     file = paste0(parametros$RData_seg, "IESS_RTR_rentas_2022.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()

