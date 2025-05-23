# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

#Lectura de archivos--------------------------------------------------------------------------------

#Mineria de datos-----------------------------------------------------------------------------------
#source( 'R/COESCOP/200_mineria_datos_cargos.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos genéricos--------------------------------------------------------------------------------
source( 'R/macro/400_graf_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de COESCOP ------------------------------------------------------------------
source( 'R/CTE/400_graf_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
source( 'R/macro/500_tab_contexto_economico.R', encoding = 'UTF-8', echo = FALSE )

# Tablas específicas de COESCOP --------------------------------------------------------------------
source( 'R/CTE/500_tab_analisis_demografico.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/CTE/501_tab_resultados.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------
