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
col_tip <- c( 'character', 'numeric', 'character', 'numeric', 'character', 'numeric','character', 'numeric', 'numeric','character', 
              'numeric','numeric','character','character','numeric','numeric ','numeric', 'character','numeric', 'numeric','character','character', 
              'character','character','character','character','character','character','character','character' , 'character', 'character',        
              'numeric', 'character', 'character','character','character','character','character','character','numeric','numeric','numeric', 
              'numeric','numeric','numeric','numeric','numeric', 'numeric','character','character','character','character','numeric', 
              'character','character','character','character','character','character','character','character',
              'numeric','character','numeric','character','character','numeric')
#col_tip2 <- c( 'character', 'factor', 'character', 'character', 'character', 'character','character', 'character', 'character','character', 
             #'character', 'character', 'character', 'character', 'character', 'character','character', 'character', 'character','character', 
            # 'character', 'character', 'character', 'character', 'character', 'character','character', 'character', 'character','character', 
            # 'character', 'character', 'character', 'character', 'character', 'character','character', 'character', 'character','character', 
            # 'character', 'character', 'character', 'character', 'character', 'character','character', 'character', 'character','character', 
            #  'character', 'character', 'character', 'character', 'character', 'character','character', 'character', 'character','character', 
             # 'character','character','character','character','character','character')
message( '\tLectura de mora patronal' )
#parametros$Dat <- "D:\\Pasantes\\Darlyn\\IESS_sentecia_1024_19_sgrt\\Data\\SENTENCIA CORTE CONST 1024\\IESS-DSGRT-2022-0409-M\\"

file1 <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/1_1.txt')
file2 <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/1_2.txt')
file3 <- paste0( parametros$Data, 'RTR/SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/1_3.txt')
mora_patronal_1_1 <- read.table(file1, header=F, sep=";",fill=TRUE, dec=",",col.names =col_nom,skip=1) %>% clean_names()   #subimos primera hoja
mora_patronal_1_2 <- read.table(file2, header = F,sep=";",fill=TRUE,col.names = col_nom,skip=1 ) %>% clean_names()   #subimos segunda hoja
mora_patronal_1_3 <- read.table(file3, header = F,sep=";",fill=TRUE,col.names = col_nom,skip=1 ) %>% clean_names()   #subimos tercera hoja 
mora_patronal_1 <- rbind(mora_patronal_1_1,mora_patronal_1_2,mora_patronal_1_3)  #unimos las 3 hojas de excel
mora_patronal_1 <- mora_patronal_1 %>% distinct()
#as.numeric(factor(mora_patronal_1$cod_provincia))
#apply(mora_patronal_1_2, 2, function(y) gsub("Ã‰", "É", y))

from <- c('Ã“','Ã‰','Ã', 'Ãš','Ã‘','Í\u0081','Í‘','\u008d')
to <- c('Ó','É','Í','Ú','Ñ','Á','Ñ','')
#gsub para provincia
a1 <- mora_patronal_1$provincia
for (i in 1:length(from)) {
  
  a1 <- gsub(from[i],to[i],a1)

}
mora_patronal_1$provincia <- a1

#gsub para canton 
a2 <- mora_patronal_1$canton
for (i in 1:length(from)) {
  
  a2 <- gsub(from[i],to[i],a2)
  
}
mora_patronal_1$canton <- a2

#gsub para parroquia
a3 <- mora_patronal_1$parroquia
for (i in 1:length(from)) {
  
  a3 <- gsub(from[i],to[i],a3)
  
}
mora_patronal_1$parroquia <- a3

#gsub para dessuc
a4 <- mora_patronal_1$dessuc
for (i in 1:length(from)) {
  
  a4 <- gsub(from[i],to[i],a4)
  
}
mora_patronal_1$dessuc <- a4


#gsub para provincia sri
a5 <- mora_patronal_1$provincia_sri
for (i in 1:length(from)) {
  
  a5 <- gsub(from[i],to[i],a5)
  
}
mora_patronal_1$provincia_sri <- a5


#gsub para desactsec

a6 <- mora_patronal_1$desactsec
for (i in 1:length(from)) {
  
  a6 <- gsub(from[i],to[i],a6)
  
}
mora_patronal_1$desactsec <- a6

#gsub para nomemp

a7 <- mora_patronal_1$nomemp
for (i in 1:length(from)) {
  
  a7 <- gsub(from[i],to[i],a7)
  
}
mora_patronal_1$nomemp <- a7


#gsub para nomrepleg

a8 <- mora_patronal_1$nomrepleg
for (i in 1:length(from)) {
  
  a8 <- gsub(from[i],to[i],a8)
  
}
mora_patronal_1$nomrepleg <- a8

#gsub para apenomrepleg

a9 <- mora_patronal_1$apenomrepleg
for (i in 1:length(from)) {
  
  a9 <- gsub(from[i],to[i],a9)
  
}
mora_patronal_1$apenomrepleg <- a9

#gsub para destipemp

a10 <- mora_patronal_1$destipemp
for (i in 1:length(from)) {
  
  a10 <- gsub(from[i],to[i],a10)
  
}
mora_patronal_1$destipemp <- a10

#gsub para estado
a11 <- mora_patronal_1$estado
for (i in 1:length(from)) {
  
  a11 <- gsub(from[i],to[i],a11)
  
}
mora_patronal_1$estado <- a11

#gsub para conceptos
a12 <- mora_patronal_1$conceptos
for (i in 1:length(from)) {
  
  a12 <- gsub(from[i],to[i],a12)
  
}
mora_patronal_1$conceptos <- a12

#gsub para x_tipo_empresa
a13 <- mora_patronal_1$x_tipo_empresa
for (i in 1:length(from)) {
  
  a13 <- gsub(from[i],to[i],a13)
  
}
mora_patronal_1$x_tipo_empresa <- a13



# # Guardando mortalidad -----------------------------------------------------------------------------
# message("\tGuardando tablas")
 save(mora_patronal_1,
      file = paste0(parametros$RData_seg, "IESS_RTR_mora_patronal_1.RData"))

# # Borrando Dataframes--------------------------------------------------------------------------------
 message(paste(rep("-", 100), collapse = ""))
 rm(list = ls()[!(ls() %in% c("parametros"))])
 gc()
