message( paste( rep('-', 100 ), collapse = '' ) )
load( paste0( parametros$RData, 'IESS_checksums_archivos_proyecto.RData' ) )

script_files <- list.files( path = 'R', pattern = '[0-9]{3}.' )
sha256_script_files_new <- sapply( paste0( 'R/', script_files ), 
                               FUN = function( f ) digest( f, algo = 'sha256', file = TRUE ) )
checksums_scripts_new <- data.table( archivo = script_files, sha256_new = sha256_script_files_new )

check <- merge( checksums_scripts, checksums_scripts_new, by = c( 'archivo' ) )
check[ , check := identical( sha256, sha256_new ) ]
if( nrow( check ) != nrow( checksums_scripts ) ) {
  message( '\tSe ha incluido nuevos scipts R' )  
}
for ( i in 1:nrow( check ) ) {
  if( !check$check[i] ) {
    message( '\tProblemas con el archivo: ', check$archivo[i] )
  }
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
