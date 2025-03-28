message( '\tEstableciendo función tildes a latex' )


# Función---------- --------------------------------------------------------------------------------
#Recibe de entrada y salida objeto xtable evitando errores con al codificación UTF-8

tildes_a_latex <- function(xtb_aux) {
  
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Á", "A", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("É", "E", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Í", "I", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ó", "O", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ú", "U", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ñ", "Ñ", x, fixed = TRUE) else x }))
  #xtb_aux <- xtable(xtb_aux)
  return(xtb_aux)
}
