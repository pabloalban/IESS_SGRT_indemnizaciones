message( paste( rep('-', 100 ), collapse = '' ) )
col_nom <- c( 'fecha_carga', 'cod_provincia', 'provincia', 'cod_canton','canton','cod_parroquia','parroquia',
              'rucemp','cod_suc','dessuc','aniper','mesper','cod_tipo','tipo','num_obligacion','valor',
              'interes','honorarios','gastos_administrativos','total','cod_estado','estado',
              'fecha_creacion','tipo_obligacion','destipo_obligacion','periodo_desde ','periodo_hasta','codtipemp',
              'destipemp','codsec','dessec','codtipacu','cedrepleg','apenomrepleg','fecdefper','fecha_suspension_definitiva',
              'estado_persona_natural','estado_sociedad','fecha_notificacion','ultima_fecha_actualizacion',
              'valnorpla', 'valadipla', 'valcesadipla', 'valiecpla', 'valsecpla', 'valssiftp', 'vallodpla', 'valcco', 'numero_obligaciones',
              'actividad_economica_pri','nomemp','fecsor', 'desactsec','codactsec', 'estado_anterior', 'provincia_sri', 'cedrepleg_sri',
              'nomrepleg', 'afiliados_fecha',' tipo_empresa','numero_glosa', 'cedula_nombre_abogado','numgui', 'conceptos',
              'dias_mora', 'rango_dias_mora', 'fecpagpla',' x_dias_mora_ant')

col_tip <- c( 'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'numeric',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'numeric',
              'numeric',
              'numeric',
              'numeric',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character',
              'character')



message( '\tLectura de mora patronal' )
#Ubicación------------------------------------------------------------------------------------------
file1 <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/2_4.txt')
file2 <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/2_5.txt')
file3 <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/2_6.txt')

#Carga----------------------------------------------------------------------------------------------
colTypes <- as.data.frame(sapply(mora_patronal_2, class))


mora_patronal_2_4 <- read.table(file1,
                                header = F,
                                sep=";",
                                dec = ",",
                                fill=TRUE,
                                col.names = col_nom,
                                colClasses =col_tip,
                                skip=1) %>% clean_names()   #primera hoja

mora_patronal_2_5 <- read.table(file2,
                                header = F,
                                sep=";",
                                dec = ",",
                                fill=TRUE,
                                col.names = col_nom,
                                skip=1) %>% clean_names()   #segunda hoja
mora_patronal_2_6 <- read.table(file3, 
                                header = F,
                                sep=";",
                                dec = ",",
                                fill=TRUE,
                                col.names = col_nom,
                                skip=1) %>% clean_names()   #tercera hoja

mora_patronal_2 <- rbind(mora_patronal_2_4,
                         mora_patronal_2_5,
                         mora_patronal_2_6)  #unimos las 3 hojas de excel

aux <- mora_patronal_2 %>%
  mutate(aniper = as.integer( gsub(",00", "", aniper) ),
         mesper = as.integer( gsub(",00", "", mesper ) ),
         valor = as.numeric( gsub(",", ".", valor ) ),
         interes = as.numeric( interes ),
         honorarios = as.numeric( honorarios ),
         gastos_administrativos = as.numeric( gsub(",", ".", gastos_administrativos ) ),
         total = as.numeric( gsub(",", ".", total ) ),
         valnorpla  = as.numeric( gsub(",", ".", valnorpla  ) ),
         valadipla  = as.numeric( gsub(",", ".", valadipla  ) ),
         numero_obligaciones   = as.numeric( gsub(",", ".", numero_obligaciones  ) ),
         dias_mora  = as.numeric( gsub(",", ".", dias_mora  ) ),
         x_x_dias_mora_ant = as.numeric( gsub(",", ".", x_x_dias_mora_ant  ) ),
         fecha_carga = as.Date(  gsub(" 0:00:00", "", fecha_carga ), "%d/%m/%Y" ) ) %>%
  dplyr::select( fecha_carga,
                 aniper,
                 mesper,
                 tipo,
                 valor,
                 interes,
                 honorarios,
                 gastos_administrativos,
                 total,
                 tipo_obligacion,
                 destipo_obligacion,
                 valnorpla,
                 periodo_desde,
                 periodo_hasta,
                 dias_mora,
                 x_x_dias_mora_ant)


from <- c('Ã“','Ã‰','Ã', 'Ãš','Ã‘','Í\u0081','Í‘','\u008d')
to <- c('Ó','É','Í','Ú','Ñ','Á','Ñ','')
#gsub para provincia
a1 <- mora_patronal_2$provincia
for (i in 1:length(from)) {
  
  a1 <- gsub(from[i],to[i],a1)
  
}
mora_patronal_2$provincia <- a1

#gsub para canton 
a2 <- mora_patronal_2$canton
for (i in 1:length(from)) {
  
  a2 <- gsub(from[i],to[i],a2)
  
}
mora_patronal_2$canton <- a2

#gsub para parroquia
a3 <- mora_patronal_2$parroquia
for (i in 1:length(from)) {
  
  a3 <- gsub(from[i],to[i],a3)
  
}
mora_patronal_2$parroquia <- a3

#gsub para dessuc
a4 <- mora_patronal_2$dessuc
for (i in 1:length(from)) {
  
  a4 <- gsub(from[i],to[i],a4)
  
}
mora_patronal_2$dessuc <- a4


#gsub para provincia sri
a5 <- mora_patronal_2$provincia_sri
for (i in 1:length(from)) {
  
  a5 <- gsub(from[i],to[i],a5)
  
}
mora_patronal_2$provincia_sri <- a5


#gsub para desactsec

a6 <- mora_patronal_2$desactsec
for (i in 1:length(from)) {
  
  a6 <- gsub(from[i],to[i],a6)
  
}
mora_patronal_2$desactsec <- a6

#gsub para nomemp

a7 <- mora_patronal_2$nomemp
for (i in 1:length(from)) {
  
  a7 <- gsub(from[i],to[i],a7)
  
}
mora_patronal_2$nomemp <- a7


#gsub para nomrepleg

a8 <- mora_patronal_2$nomrepleg
for (i in 1:length(from)) {
  
  a8 <- gsub(from[i],to[i],a8)
  
}
mora_patronal_2$nomrepleg <- a8

#gsub para apenomrepleg

a9 <- mora_patronal_2$apenomrepleg
for (i in 1:length(from)) {
  
  a9 <- gsub(from[i],to[i],a9)
  
}
mora_patronal_2$apenomrepleg <- a9

#gsub para destipemp

a10 <- mora_patronal_2$destipemp
for (i in 1:length(from)) {
  
  a10 <- gsub(from[i],to[i],a10)
  
}
mora_patronal_2$destipemp <- a10

#gsub para estado
a11 <- mora_patronal_2$estado
for (i in 1:length(from)) {
  
  a11 <- gsub(from[i],to[i],a11)
  
}
mora_patronal_2$estado <- a11

#gsub para conceptos
a12 <- mora_patronal_2$conceptos
for (i in 1:length(from)) {
  
  a12 <- gsub(from[i],to[i],a12)
  
}
mora_patronal_2$conceptos <- a12

#gsub para x_tipo_empresa
a13 <- mora_patronal_2$x_tipo_empresa
for (i in 1:length(from)) {
  
  a13 <- gsub(from[i],to[i],a13)
  
}
mora_patronal_2$x_tipo_empresa <- a13




# # Guardando mortalidad -----------------------------------------------------------------------------
# message("\tGuardando tablas")
save(mora_patronal_2,
     file = paste0(parametros$RData_seg, "IESS_RTR_mora_patronal_2.RData"))


# # Borrando Dataframes--------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
