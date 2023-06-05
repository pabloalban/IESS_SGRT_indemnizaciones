message(paste(rep("-", 100), collapse = ""))

message("\tLectura de pagos de pensiones de permanetes anbsolutas y total, y montepío de RTR")

# Carga información registro civil-------------------------------------------------------------------
#load("Z:/IESS/Data/rtr/RC_fecha_defuncion.Rdata")

# Carga de archivos de rentas de RTR----------------------------------------------------------------
base <- as.data.frame(1)

for (anio in 2012:2017) {
  for (mes in 1:12) {
    archivo <- paste0("nomina_", anio, "_", mes, ".txt")
    file <- paste0(parametros$Data_seg, "IESS_RTR_rentas/", archivo)
    # Carga del pago de pensiones mensuales de las rentas de RTR----------------------------------
    aux <- read.table(file,
      sep = "\t",
      dec = ".",
      na = "",
      header = TRUE,
      skip = 0
    ) %>%
      clean_names() %>%
      select(
        "anio",
        "mes",
        "cedula",
        "fecha_nacimiento",
        "sexo",
        "prov_bene",
        "tipo_seguro",
        "tipo_prestacion",
        "beneficiario",
        "renta_mensual",
        "decimo_tercero_mensualizado",
        "decimo_cuarto_mensualizado",
        "decimo_tercero",
        "decimo_cuarto",
        "aumentos",
        "financiamiento_decimas_pensiones",
        "financiamiento_fondo_mortuorio",
        "retencion_judicial",
        #"retencion_adicional",
        "retencion_prestamo",
        "seguro_medico",
        "liq_pagar",
        "tot_ingr",
        "tot_desc",
        "tot_pension"
      )
    if (dim(base)[1] == 1) {
      base <- aux
    } else if (dim(aux)[1] > 0) {
      base <- rbind(base, aux)
    }
  }
}

for (anio in 2018:2018) {
  for (mes in 1:6) {
    archivo <- paste0("nomina_", anio, "_", mes, ".txt")
    file <- paste0(parametros$Data_seg, "IESS_RTR_rentas/", archivo)
    # Carga del pago de pensiones mensuales de las rentas de RTR----------------------------------
    aux <- read.table(file,
                      sep = "\t",
                      dec = ".",
                      na = "",
                      header = TRUE,
                      skip = 0
    ) %>%
      clean_names() %>%
      select(
        "anio",
        "mes",
        "cedula",
        "fecha_nacimiento",
        "sexo",
        "prov_bene",
        "tipo_seguro",
        "tipo_prestacion",
        "beneficiario",
        "renta_mensual",
        "decimo_tercero_mensualizado",
        "decimo_cuarto_mensualizado",
        "decimo_tercero",
        "decimo_cuarto",
        "aumentos",
        "financiamiento_decimas_pensiones",
        "financiamiento_fondo_mortuorio",
        "retencion_judicial",
        #"retencion_adicional",
        "retencion_prestamo",
        "seguro_medico",
        "liq_pagar",
        "tot_ingr",
        "tot_desc",
        "tot_pension"
      )
    if (dim(base)[1] == 1) {
      base <- aux
    } else if (dim(aux)[1] > 0) {
      base <- rbind(base, aux)
    }
  }
}    
#Mes de agosto de 2018------------------------------------------------------------------------------
    anio = 2018
    mes = 8
    archivo <- paste0("nomina_", anio, "_", mes, ".txt")
    file <- paste0(parametros$Data_seg, "IESS_RTR_rentas/", archivo)
    aux <- read.table(file,
                      sep = "\t",
                      dec = ".",
                      na = "",
                      header = TRUE,
                      skip = 0
    ) %>%
      clean_names() %>%
      mutate( decimo_tercero = 0 ) %>%
      select(
        "anio",
        "mes",
        "cedula",
        "fecha_nacimiento",
        "sexo",
        "prov_bene",
        "tipo_seguro",
        "tipo_prestacion",
        "beneficiario",
        "renta_mensual",
        "decimo_tercero_mensualizado",
        "decimo_cuarto_mensualizado",
        "decimo_tercero",
        "decimo_cuarto",
        "aumentos",
        "financiamiento_decimas_pensiones",
        "financiamiento_fondo_mortuorio",
        "retencion_judicial",
        #"retencion_adicional",
        "retencion_prestamo",
        "seguro_medico",
        "liq_pagar",
        "tot_ingr",
        "tot_desc",
        "tot_pension"
      )
    base <- rbind(base, aux)   

#Mes de diciembre de 2018---------------------------------------------------------------------------
anio = 2018
mes = 12
archivo <- paste0("nomina_", anio, "_", mes, ".txt")
file <- paste0(parametros$Data_seg, "IESS_RTR_rentas/", archivo)
aux <- read.table(file,
                  sep = "\t",
                  dec = ".",
                  na = "",
                  header = TRUE,
                  skip = 0
) %>%
  clean_names() %>%
  mutate( decimo_cuarto = 0 ) %>%
  select(
    "anio",
    "mes",
    "cedula",
    "fecha_nacimiento",
    "sexo",
    "prov_bene",
    "tipo_seguro",
    "tipo_prestacion",
    "beneficiario",
    "renta_mensual",
    "decimo_tercero_mensualizado",
    "decimo_cuarto_mensualizado",
    "decimo_tercero",
    "decimo_cuarto",
    "aumentos",
    "financiamiento_decimas_pensiones",
    "financiamiento_fondo_mortuorio",
    "retencion_judicial",
    #"retencion_adicional",
    "retencion_prestamo",
    "seguro_medico",
    "liq_pagar",
    "tot_ingr",
    "tot_desc",
    "tot_pension"
  )
base <- rbind(base, aux)  
#Mes de agosto a diciembre de 2018------------------------------------------------------------------
for (anio in c(2018)) {
  for (mes in c(7,9,10,11)) {
    archivo <- paste0("nomina_", anio, "_", mes, ".txt")
    file <- paste0(parametros$Data_seg, "IESS_RTR_rentas/", archivo)
    aux <- read.table(file,
                      sep = "\t",
                      dec = ".",
                      na = "",
                      header = TRUE,
                      skip = 0
    ) %>%
      clean_names() %>%
      mutate( decimo_tercero = 0,
              decimo_cuarto = 0 ) %>%
      select(
        "anio",
        "mes",
        "cedula",
        "fecha_nacimiento",
        "sexo",
        "prov_bene",
        "tipo_seguro",
        "tipo_prestacion",
        "beneficiario",
        "renta_mensual",
        "decimo_tercero_mensualizado",
        "decimo_cuarto_mensualizado",
        "decimo_tercero",
        "decimo_cuarto",
        "aumentos",
        "financiamiento_decimas_pensiones",
        "financiamiento_fondo_mortuorio",
        "retencion_judicial",
        #"retencion_adicional",
        "retencion_prestamo",
        "seguro_medico",
        "liq_pagar",
        "tot_ingr",
        "tot_desc",
        "tot_pension"
      )
    if (dim(base)[1] == 1) {
      base <- aux
    } else if (dim(aux)[1] > 0) {
      base <- rbind(base, aux)
    }
  }
}

# Depuración de base bruta---------------------------------------------------------------------------
base$mes <- gsub("Enero", "1", base$mes)
base$mes <- gsub("Febrero", "2", base$mes)
base$mes <- gsub("Marzo", "3", base$mes)
base$mes <- gsub("Abril", "4", base$mes)
base$mes <- gsub("Mayo", "5", base$mes)
base$mes <- gsub("Junio", "6", base$mes)
base$mes <- gsub("Julio", "7", base$mes)
base$mes <- gsub("Agosto", "8", base$mes)
base$mes <- gsub("Septiembre", "9", base$mes)
base$mes <- gsub("Octubre", "10", base$mes)
base$mes <- gsub("Noviembre", "11", base$mes)
base$mes <- gsub("Diciembre", "12", base$mes)
base$mes <- as.integer(base$mes)

base <- base %>%
  mutate( sexo = if_else(sexo == "Femenino", "M", "H"),
          fecha_nacimiento = as.Date(fecha_nacimiento, "%d/%m/%Y"),
          fecha_nacimiento = if_else(is.na(fecha_nacimiento),
                               as.Date("1967/07/07", "%Y/%m/%d"),
                               fecha_nacimiento ) ) %>%
  arrange( anio,
           mes )


# Filtrar prestación permanete parcial--------------------------------------------------------------
prestaciones_pp_2018 <- base %>%
  filter(tipo_prestacion == "PP")

# Filtrar prestación permanete total----------------------------------------------------------------
prestaciones_pt_2018 <- base %>%
  filter(tipo_prestacion == "PT")

# Filtrar prestaciones permanente absoluta----------------------------------------------------------
prestaciones_pa_2018 <- base %>%
  filter(tipo_prestacion == "PA")
# left_join(.,calculo_rentas_rtr,by=c('cedula'='asegurado'))

# Filtar prestaciones de montepío viudez------------------------------------------------------------
prestaciones_viudez_2018 <- base %>%
  filter(tipo_prestacion == "VO", beneficiario == "VIUDEDAD")

# Filtar prestaciones de montepío viudez------------------------------------------------------------
prestaciones_orfandad_2018 <- base %>%
  filter(tipo_prestacion == "VO", beneficiario == "ORFANDAD")

nomina_sgrt_pre_2018 <- base

# Guardando en un Rdata-----------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(nomina_sgrt_pre_2018,
  prestaciones_pp_2018,
  prestaciones_pt_2018,
  prestaciones_pa_2018,
  prestaciones_viudez_2018,
  prestaciones_orfandad_2018,
  file = paste0(parametros$RData_seg, "IESS_RTR_rentas_2018.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()
