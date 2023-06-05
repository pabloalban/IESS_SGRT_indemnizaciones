message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCalculando reducción criptográfica hash SHA256 para scripts R' )
script_files <- list.files( path = 'R', pattern = '[0-9]{3}.' )
sha256_script_files <- sapply( paste0( 'R/', script_files ), 
                               FUN = function( f ) digest( f, algo = 'sha256', file = TRUE ) )
checksums_scripts <- data.table( archivo = script_files, sha256 = sha256_script_files )

message( '\tCalculando reducción criptográfica hash SHA256 para RData' )
rdata_files <- list.files( path = parametros$RData )
sha256_rdata_files <- sapply( paste0( parametros$RData, rdata_files ), 
                              FUN = function( f ) digest( f, algo = 'sha256', file = TRUE ) )
checksums_rdata <- data.table( archivo = rdata_files, sha256 = sha256_rdata_files )

message( '\tCalculando reducción criptográfica hash SHA256 para Data' )
data_files <- list.files( path = parametros$Data )
sha256_data_files <- sapply( paste0( parametros$Data, data_files ), 
                             FUN = function( f ) digest( f, algo = 'sha256', file = TRUE ) )
checksums_data <- data.table( archivo = data_files, sha256 = sha256_data_files )

message( '\tCalculando reducción criptográfica hash SHA256 para Reporte' )
reporte_files <- list.files( path = paste0( parametros$reportes ), recursive = TRUE )
sha256_reporte_files <- sapply( paste0( parametros$reportes, reporte_files ), 
                             FUN = function( f ) digest( f, algo = 'sha256', file = TRUE ) )
checksums_reporte <- data.table( archivo = reporte_files, sha256 = sha256_reporte_files )

message( '\tGuardando reducción criptográfica hash SHA256' )
save( checksums_scripts, checksums_rdata, checksums_data, checksums_reporte,
      file = paste0( parametros$RData, 'IESS_checksums_archivos_proyecto.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
