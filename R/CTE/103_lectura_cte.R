message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura del cuerpo de vigilancia de la comisión de tránsito' )

#Cargando información financiera--------------------------------------------------------------------
file_2022 <- paste0(parametros$Data, 'COESCOP_cte_2022.xlsx' )
file_2023 <- paste0(parametros$Data, 'CTE_31_12_2023.xlsx' )

#Carga del cte 2022---------------------------------------------------------------------------------
transito <- read_excel( file,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names( )


#Carga del cte 31/12/2023---------------------------------------------------------------------------
transito <- read_excel( file,
                        sheet = 1,
                        col_names = TRUE,
                        col_types = NULL,
                        na = "",
                        skip = 0 ) %>% clean_names( )


#Cargando base del RC-------------------------------------------------------------------------------

load( paste0( parametros$RData, "IESS_Reg_Civil.RData" ) )

#Cruce con base del Registro civil------------------------------------------------------------------

transito <- left_join( transito , rc, by = c( "nombre"="nombre" ) )

#Guardando en un Rdata----------------------------------------------------------
message( '\tGuardando en data.frame' )

save( transito,
      file = paste0( parametros$RData, 'COESCOP_cte.RData' ) )

#Borrando data.frames-----------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
