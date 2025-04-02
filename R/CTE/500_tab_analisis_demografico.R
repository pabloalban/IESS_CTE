message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tCreación de las tablas del análisis demográfico' )

#Carga de función tildes a latex--------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R',
       encoding = 'UTF-8',
       echo = FALSE )

#Carga de datos-------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_tablas_contingencia_cte.RData' ) )

#Tabla CTE rangos de edad y sexo--------------------------------------------------------------------
message( '\tTabla de servidores publicos del CTE' )

aux <- tabla_rangos_edad_cargos %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) )

print( aux,
       file = paste0( parametros$resultado_tablas, 'iess_rangos_edad_cargos_cte', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow( aux ),
                        nrow( aux ) - 1 ),
       sanitize.text.function = identity
)
#Tabla CTE de sexo y cargos-------------------------------------------------------------------------

aux  <- tabla_cte_cargo_sexo %>%
  mutate_at( c( 2, 4, 6 ), as.integer ) %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) ) %>% 
  tildes_a_latex( . )

print( 
  aux,
  file = paste0( parametros$resultado_tablas, 'iess_cargo_sexo_cte', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ),
                   nrow( aux ) - 1 ),
  sanitize.text.function = identity
)

#Tabla salario promedio-----------------------------------------------------------------------------
aux  <- tabla_cte_salario %>%
  xtable( ., digits = c( 0, 0, 2 ) ) %>% 
  tildes_a_latex( . )

print( 
  aux,
  file = paste0( parametros$resultado_tablas, 'iess_salario_cte', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) ),
  sanitize.text.function = identity
)

#Tabla imposiciones por sexo------------------------------------------------------------------------
aux  <- tabla_imp_sexo %>%
  xtable( ., digits = c( 0, 0, rep( c( 0, 2 ), 3 ) ) ) %>% 
  tildes_a_latex( . )

print( 
  aux,
  file = paste0( parametros$resultado_tablas, 'iess_imp_sexo_cte', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ),
                   nrow( aux ) - 1 ),
  sanitize.text.function = identity
)

#Tabla de salidas y entradas------------------------------------------------------------------------
aux <- tabla_avisos %>%
  mutate_at( c( 2:ncol( . ) ), as.integer ) %>%
  mutate_at( c( 1 ), as.character ) %>%
  xtable( ., digits = c( 0, 0, 0, 0 ) ) %>% 
  tildes_a_latex( . )

print( 
  aux,
  file = paste0( parametros$resultado_tablas, 'iess_avisos_cte', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( nrow( aux ) ),
  sanitize.text.function = identity
)

#Limpiando memoria RAM------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )
