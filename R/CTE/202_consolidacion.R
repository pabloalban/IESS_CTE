message(paste(rep("-", 100), collapse = ""))

message("\tCargando servidores COESCOP")

# Carga de bases------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'CTE_servidores.RData' ) )

#Selección de variables-----------------------------------------------------------------------------

cte <- cte_servidores %>%
  mutate( ciudad = NA ) %>%
  mutate( tipo = "cte") %>% 
  dplyr::select( cedula,
                 fecha_nacimiento,
                 sexo,
                 nombre,
                 cargo := cargo_coescop,
                 sueldo,
                 cargo_coescop,
                 ciudad,
                 tipo,
                 numimp := imp )

#Factor de correción--------------------------------------------------------------------------------

factor <- 1 #10561 / 6596

#Consolidación en una sola tabla--------------------------------------------------------------------

coescop <- rbind( cte ) %>%
  #filter( cargo_coescop != "Administrativo" ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date( "31/12/2024","%d/%m/%Y" ),
                                  units = "years",
                                  precise = TRUE ) ) )

#Guardar en Rdata-----------------------------------------------------------------------------------
save( coescop,
      factor,
      file = paste0( parametros$RData, 'IESS_consolidado_coescop.RData' ) )

# #-------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
