
message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas de los resultados y balance actuarial' )

#Carga de función tildes a latex--------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Carga de datos-------------------------------------------------------------------------------------
load( paste0( parametros$RData, "IESS_balance.RData" ) )
load( file = paste0( parametros$RData_macro, 'IESS_macro_estudio.RData' ) )
load( file = paste0( parametros$RData, 'COESCOP_esquema_pensional.RData' ) )

#Tabla del impacto al IVM y Salud-------------------------------------------------------------------

message( '\tTabla del impacto al IVM y Salud' )

aux <- impacto %>%
 dplyr::select( -r, -v ) %>%
 mutate( anio = as.character( anio ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 2, 6 ) ) )

aux_xtable <- tildes_a_latex( aux_xtable )
print( aux_xtable,
    file = paste0( parametros$resultado_tablas, 'iess_impacto', '.tex' ),
    type = 'latex',
    include.colnames = FALSE,
    include.rownames = FALSE,
    format.args = list( decimal.mark = ',', big.mark = '.' ),
    only.contents = TRUE,
    hline.after = nrow( aux ),
    sanitize.text.function = identity )

#Tabla del Balance Corriente------------------------------------------------------------------------

message( '\tTabla del Balance Corriente' )

aux <- balance_anual %>%
 dplyr::select( anio,
         n_old,
         aporte_coescop,
         reserva_a_pagar,
         ingresos_total,
         reserva_matematica,
         montepio,
         gastos_administrativos,
         egreso_total,
         V_cor,
         V_cap ) %>%
 mutate( anio = as.character( anio ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 2, 10 ) ) )

aux_xtable <- tildes_a_latex( aux_xtable )
print( aux_xtable,
    file = paste0( parametros$resultado_tablas, 'iess_balance_cap', '.tex' ),
    type = 'latex',
    include.colnames = FALSE,
    include.rownames = FALSE,
    format.args = list( decimal.mark = ',', big.mark = '.' ),
    only.contents = TRUE,
    hline.after = nrow( aux ),
    sanitize.text.function = identity )


#Tabla del Balance Actuarial------------------------------------------------------------------------

message( '\tTabla del Balance Actuarial' )

aux <- balance_anual %>%
 dplyr::select( anio,
         n_old,
         aporte_coescop_vap,
         reserva_a_pagar_vap,
         ingresos_total_vap,
         reserva_matematica_vap,
         montepio_vap,
         gastos_administrativos_vap,
         egreso_total_vap,
         V ) %>%
 mutate( anio = as.character( anio ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 2, 9 ) ) )

aux_xtable <- tildes_a_latex( aux_xtable )
print( aux_xtable,
    file = paste0( parametros$resultado_tablas, 'iess_balance_vap', '.tex' ),
    type = 'latex',
    include.colnames = FALSE,
    include.rownames = FALSE,
    format.args = list( decimal.mark = ',', big.mark = '.' ),
    only.contents = TRUE,
    hline.after = nrow( aux ),
    sanitize.text.function = identity )


#Tabla del Valor a pagar por institución------------------------------------------------------------

message( '\tTabla Valor a pagar por institución' )

aux <- valor_a_pagar_ins

aux_xtable <- xtable( aux, digits = c( 0, 0, 2 ) )

aux_xtable <- tildes_a_latex( aux_xtable )

print( aux_xtable,
    file = paste0( parametros$resultado_tablas, 'iess_valor_a_pagar_ins', '.tex' ),
    type = 'latex',
    include.colnames = FALSE,
    include.rownames = FALSE,
    format.args = list( decimal.mark = ',', big.mark = '.' ),
    only.contents = TRUE,
    hline.after = c( nrow( aux ) - 1,
            nrow( aux ) ),
    sanitize.text.function = identity )


# Tabla resumen de hipótesis macro -----------------------------------------------------------------

message( '\tGenerando tablas de la evolución de las hipótesis macroeconómicas' )

var_nom <- c( 'Tasa variaci\\\'{o}n PIB',
       #'Tasa activa referencial',
       'Tasa pasiva referencial', 
       'Tasa actuarial',
       'Tasa variaci\\\'{o}n salarial', 
       'Tasa variaci\\\'{o}n SBU',
       'Tasa inflaci\\\'{o}n' )
aux <- 
 data.table( nom = var_nom,
       val = c( paste0( formatC( hipotesis$tasas[1], decimal.mark = ",", format = 'f',
                   digits = 3 ), "\\%" ),
            paste0( formatC( hipotesis$tasas[2], decimal.mark = ",", format = 'f',
                   digits = 3 ), "\\%" ),
            paste0( formatC( 0.0625*100, decimal.mark = ",", format = 'f',
                   digits = 3 ), "\\%" ),
            paste0( formatC( hipotesis$tasas[3], decimal.mark = ",", format = 'f',
                   digits = 3 ), "\\%" ),
            paste0( formatC( hipotesis$tasas[4], decimal.mark = ",", format = 'f',
                   digits = 3 ), "\\%" ),
            paste0( formatC( hipotesis$tasas[5], decimal.mark = ",", format = 'f',
                   digits = 3 ), "\\%" ) 
        ) )
xtb_aux <- xtable( aux, digits = c( 0, 0, 3 ) )

print( xtb_aux,
    file = paste0( parametros$resultado_tablas, 'iess_hipotesis_macro.tex' ),
    type = 'latex', 
    include.colnames = FALSE, include.rownames = FALSE, 
    format.args = list( decimal.mark = ',', big.mark = '.' ), 
    only.contents = TRUE, 
    hline.after = NULL, sanitize.text.function = identity )

#Tabla de la tasa de aportación---------------------------------------------------------------------

aux <- tab_tasa %>% 
  mutate( personal = 100 * personal,
          patronal = 100 * patronal,
          total = 100 * total )

aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 0, 3 ) ) )

aux_xtable <- tildes_a_latex( aux_xtable )

print( aux_xtable,
    file = paste0( parametros$resultado_tablas, 'coescop_tab_tasa', '.tex' ),
    type = 'latex',
    include.colnames = FALSE,
    include.rownames = FALSE,
    format.args = list( decimal.mark = ',', big.mark = '.' ),
    only.contents = TRUE,
    hline.after = c( nrow( aux ) - 1,
            nrow( aux ) ),
    sanitize.text.function = identity )


#Tabla de la tasa de aportación---------------------------------------------------------------------

aux <- tab_coeficientes

aux_xtable <- xtable( aux, digits = c( 0, 0, 0 ) )

aux_xtable <- tildes_a_latex( aux_xtable )

print( aux_xtable,
  file = paste0( parametros$resultado_tablas, 'coescop_tab_coeficientes', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) ),
    sanitize.text.function = identity )

#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )

