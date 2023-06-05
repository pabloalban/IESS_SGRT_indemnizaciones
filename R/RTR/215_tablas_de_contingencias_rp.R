message(paste(rep("-", 100), collapse = ""))

message("\tCargando responsabilidades patronales RTR")
load(paste0(parametros$RData_seg, "IESS_RTR_responsabilidad_patronal.RData"))
#Período de estudio
anio_ini<-2012
anio_fin<-2020



#1.Tabla por rangos de edad----------------------------------------------------- 

cortes_edad<-c(15,seq(20,70,10),81)  #elaborado por Darlyn 
etiquetas_edad<-c( paste0("(",formatC( c(15,seq(20,70,10)), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-",formatC( c(seq(20,70,10),81), 
                                          digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))


aux <- reporte_resp_patronal %>%  filter(!is.na(edad),edad!=922.00) %>% filter(!is.na(sexo)) %>% 
 # distinct(cedula,.keep_all = TRUE) %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                         labels = etiquetas_edad,
                         #include.lowest = TRUE,
                         right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  select(sexo,rango_edad,afiliados,dist) %>%
  arrange(sexo,rango_edad)#ordenar por sexo y edad

aux_h <- aux %>% filter(sexo=="M") %>% select(-dist,-sexo)
aux_m <- aux %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux_t <- left_join(aux_h,aux_m,by="rango_edad") %>% select(rango_edad,hombres:=afiliados.x,mujeres:=afiliados.y)
aux_t <- aux_t%>% mutate(Total= rowSums(aux_t[,2:3])) 
aux_total_edad <- aux_t%>%  
            add_row(rango_edad = "Total", hombres = sum(aux_t$hombres), mujeres = sum(aux_t$mujeres), Total=sum(aux_t$Total))  
  
edad_sexo_total <- aux_total_edad  

#2. Tabla por rangos del monto del valor pendiente-------------------------------------------------------------------


deciles <- quantile(reporte_resp_patronal$valor_pendiente,probs = seq(0,1,1/8))


cortes_monto<-deciles[1:length(deciles)]
etiquetas_monto<-c( paste0("[\\$",formatC(deciles[1:length(deciles)-1], 
                                       digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ),
                          "-\\$",formatC( deciles[2:length(deciles)], 
                                       digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ),")"))




aux1 <- reporte_resp_patronal %>%  filter(!is.na(valor_pendiente),edad!=922.00) %>%   filter(!is.na(sexo)) %>% 
  # distinct(cedula,.keep_all = TRUE) %>%
  mutate(rango_monto=cut(valor_pendiente, breaks = cortes_monto,
                        labels = etiquetas_monto,
                        include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(rango_monto,sexo) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_monto,.keep_all = TRUE) %>%
  select(rango_monto,sexo,afiliados,dist) %>%
  arrange(sexo,rango_monto)#ordenar por sexo y rango del monto

aux_h_monto <- aux1 %>% filter(sexo=="M") %>% select(-dist,-sexo)
aux_m_monto <- aux1 %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux_t_monto <- left_join(aux_h_monto,aux_m_monto,by="rango_monto") %>% select(rango_monto,hombres:=afiliados.x,mujeres:=afiliados.y)
aux_t_monto[is.na(aux_t_monto)] <- 0
aux_t_monto <- aux_t_monto%>% mutate(Total= rowSums(aux_t_monto[,2:3])) 
aux_total_monto <- aux_t_monto%>%  
  add_row(rango_monto = "Total", hombres = sum(aux_t_monto$hombres), mujeres = sum(aux_t_monto$mujeres), Total=sum(aux_t_monto$Total))  

monto_rangos_sexo <- aux_total_monto

#3. Tabla del monto del valor pendiente  por tipo de empresa y sexo-------------------------------------------

deciles <- quantile(reporte_resp_patronal$valor_pendiente,probs = seq(0,1,1/8))




cortes_monto<-deciles[1:length(deciles)]
etiquetas_monto<-c( paste0("[\\$",formatC(deciles[1:length(deciles)-1], 
                                          digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ),
                           "-\\$",formatC( deciles[2:length(deciles)], 
                                           digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ),")"))


aux2 <- reporte_resp_patronal %>%  filter(!is.na(valor_pendiente),edad!=922.00) %>%   filter(!is.na(sexo)) %>% 
  mutate(tipo_de_empresa=as.character(tipo_de_empresa)) %>% 
  mutate(tipo_de_empresa=if_else(tipo_de_empresa == "SEGURO SOCIAL CAMPESINO","PRIVADA",tipo_de_empresa)) %>% 
  mutate(tipo_de_empresa=if_else(tipo_de_empresa == "VOLUNTARIO","PRIVADA",tipo_de_empresa)) %>% 
  filter(tipo_de_empresa=="PRIVADA") %>% 
  # distinct(cedula,.keep_all = TRUE) %>%
  mutate(rango_monto=cut(valor_pendiente, breaks = cortes_monto,
                         labels = etiquetas_monto,
                         include.lowest = TRUE,
                         right = TRUE)) %>%
  group_by(rango_monto,sexo) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_monto,.keep_all = TRUE) %>%
  select(rango_monto,tipo_de_empresa,sexo,afiliados,dist) %>%
  arrange(sexo,rango_monto)#ordenar por sexo y rango del monto

aux2_h_monto <- aux2 %>% filter(sexo=="M") %>% select(-dist,-sexo)
aux2_m_monto <- aux2 %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux2_t_monto <- left_join(aux2_h_monto,aux2_m_monto,by="rango_monto") %>% select(rango_monto,hombres_priv:=afiliados.x,mujeres_priv:=afiliados.y)
aux2_t_monto[is.na(aux2_t_monto)] <- 0 
aux2_t_monto <- aux2_t_monto%>% mutate(Total_priv= rowSums(aux2_t_monto[,2:3])) 




aux3 <- reporte_resp_patronal %>%  filter(!is.na(valor_pendiente),edad!=922.00) %>%   filter(!is.na(sexo)) %>% 
  filter(tipo_de_empresa=="PUBLICA") %>% 
  # distinct(cedula,.keep_all = TRUE) %>%
  mutate(rango_monto=cut(valor_pendiente, breaks = cortes_monto,
                         labels = etiquetas_monto,
                         #include.lowest = TRUE,
                         right = TRUE)) %>%
  group_by(sexo,rango_monto) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_monto,.keep_all = TRUE) %>%
  select(sexo,rango_monto,afiliados,dist,tipo_de_empresa) %>%
  arrange(sexo,rango_monto)#ordenar por sexo y rango del monto

aux3_h_monto <- aux3 %>% filter(sexo=="M") %>% select(-dist,-sexo)
aux3_m_monto <- aux3 %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux3_t_monto <- left_join(aux3_h_monto,aux3_m_monto,by="rango_monto") %>% select(rango_monto,hombres_publ:=afiliados.x,mujeres_publ:=afiliados.y)
aux3_t_monto[is.na(aux3_t_monto)] <- 0
aux3_t_monto <- aux3_t_monto%>% mutate(Total_publ= rowSums(aux3_t_monto[,2:3])) 

aux_total=merge(aux2_t_monto,aux3_t_monto,by="rango_monto",all=TRUE)


aux_total_monto1 <- aux_total %>% add_row(rango_monto="Total",hombres_priv=sum(aux_total[,2]),
                                           mujeres_priv=sum(aux_total[,3]),Total_priv=sum(aux_total[,4]),
                                           hombres_publ=sum(aux_total[,5]), mujeres_publ=sum(aux_total[,6]),
                                           Total_publ=sum(aux_total[,7]))

monto_rangos_sexo_empresa <- aux_total_monto1

save( reporte_resp_patronal,
      edad_sexo_total,
      monto_rangos_sexo,
      monto_rangos_sexo_empresa,
      file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_rp.RData' ) )
# #-----------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()