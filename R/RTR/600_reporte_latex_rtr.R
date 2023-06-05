# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Gráficos genéricos---------------------------------------------------------------------------------
source( 'R/402_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/403_graf_demografia_sgo.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de RTR ----------------------------------------------------------------------
source( 'R/rtr/400_graf_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/401_graf_inversiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/402_graf_analisis_financiero_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/403_graf_comp_primas_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/404_graf_tasas_decrementos_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/405_graf_responsabilidad_patronal_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/406_graf_balance_actuarial_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
#source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/504_tab_demografia.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/505_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de RTR ------------------------------------------------------------------------
source( 'R/rtr/500_tab_analisis_financiero_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/501_tab_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/502_tab_responsabilidad_patronal_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/503_tab_causas_desfinanciamientos_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/504_tab_inversiones_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/505_tab_balance_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/508_tab_reg_rtr_.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

