# Detalle de secciones -----------------------------------------------------------------------------
# 0X Configuraciones iniciales
# 1X carga de información e imputación
# 2X Estimaciones y estadísticas
# 3X Selección proyecciones y cálculo reservas
# 4X Resultados y gráficos
# 5X Tablas
# 6X Reportes LaTeX y excel

# Parámetros y carga -------------------------------------------------------------------------------
source( 'R/002_cargar_paquetes.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/003_configurar_proyecto.R', encoding = 'UTF-8', echo = FALSE )

# Análisis demográfico -----------------------------------------------------------------------------
if ( parametros$hacer_ana_dem ) {
  source( parametros$ana_dem, encoding = 'UTF-8', echo = FALSE )
}

# Calcular balance ---------------------------------------------------------------------------------
if ( parametros$calcular_balance ) {
  source( parametros$calculo_balance, encoding = 'UTF-8', echo = FALSE )
}

# Generación reportes ------------------------------------------------------------------------------
source( parametros$reporte_genera, encoding = 'UTF-8', echo = FALSE )
