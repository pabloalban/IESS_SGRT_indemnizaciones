message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de las proyecciones de la masa salarial y afiliados del SGO' )

#Path a la carpta de inputs-------------------------------------------------------------------------
path <- "Y:/IESS_2020/Data/IVM/OUTPUT/ILO_OUT_ALL/"

#Cargando información del contexto económico--------------------------------------------------------
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) )

#Nomenclatura---------------------------------------------------------------------------------------
#Mathematical formulation of the ILO Actuarial Pension Model

##Probabilidades de transición----------------------------------------------------------------------
#q^{s}_{g,x,t} : Probability of death in the interval from t to t+1 for an individual of the group g of sex s and age x at time t.
#ir^{s}_{g,x,t} : Probability of incapacitating disability arriving in the interval from t to t+1 for an individual of the group g of sex s and age x at time t (input by the user).
#er^{s}_{g,x,t} : Probability of leaving the active contributing population for any reason other than death or disability in the interval from t to t+1, for an individual of group g of sex s and age x at time t
#qi^{s}_{g,x,t} :  Probability of death in the interval from t to t+1 for an inactive contributor, an old-age pensioner or a disability pensioner of sex s and age x at time t.


##Estados-------------------------------------------------------------------------------------------
# LF:  Labour force of sex s in period t.
# actgx : Total number of active contributors of sex s, group g and age x at time t
# inactx : Inactive contributors of sex s, age x at time t
# nentx : Entries of new contributors without past periods of contribution (new entries, entries of new people) to group g of sex s and age x from the period t-1 to t.
# rentx : Entries of contributors with past periods of contribution (re-entries) from group g, with sex s and age x from the period t-1 to t.
# nret : New old-age beneficiaries of sex s and age x at time t.
# nretfactgx : New old-age beneficiaries coming from active members of sex s, group g, age x and with c contributions, at time t that started the period t-1 to t, as activ
# nretfinactx : New old-age beneficiaries coming from inactive members of sex s, age x and with c contributions, at time t that started the period t-1 to t, as inactive.
# oldage : Number of old-age pensioners of sex s and age x at time t, obtained from previous iterations.
# oldage_ben : Benefits of old-age pensioners of sex s and age x at time t, obtained from previous iterations.
# wid : Widow(er) pensioners of sex s and age x receiving pension at time t. Obtained from previous iterations.
# orph : Number of orphans’ pensions of sex s and age x at time t. Obtained from previous iterations.
# dis : Disability pensioners of sex s and age x at time t, obtained from previous iterations.
# csact: Number of average contributions from active members of sex s, group g and age x at time t.
# ls_oldage : Number of individuals of sex s receiving old-age lump sum benefits at time t.
# ls_death : Number of individuals of sex s and age x receiving death lump sum benefits at time t.
# ICact: Initial active contributors of sex s, group g, age x and c past contributions. 
# csinact: Number of contributions from inactive members of sex s and age x over the year t-1 to t.
# csact: Number of average contributions from active members of sex s, group g and age x at time t.

##Masa Salarial-------------------------------------------------------------------------------------
#sal : average salary of individuals from group g, with sex s and age x according to the corresponding theoretical salary curve (provided by the user for the initial period).
#SALM : Total salary mass of sex s and time t.


#Definir codificación de mujer y hombre-------------------------------------------------------------
male <- 'H'
female <- 'M'

#Probabilidad de transición-------------------------------------------------------------------------
## Lectura de probabilidad de transición q^{s}_{g,x,t}-----------------------------------------------

file <- paste0( path, 'BASE,er,Female,SGO.csv')
q_female <- read.table(file,
                       sep = ",",
                       dec = ".",
                       na = "",
                       header = FALSE,
                       skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'q', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                sexo,
                x := v2,
                q )


file <- paste0( path, 'BASE,er,Male,SGO.csv')
q_male <- read.table(file,
                       sep = ",",
                       dec = ".",
                       na = "",
                       header = FALSE,
                       skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'q', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 q )

q <- rbind( q_female, q_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

## Lectura de probabilidad de transición ir^{s}_{g,x,t}-----------------------------------------------

file <- paste0( path, 'BASE,ir,Female,SGO.csv')
ir_female <- read.table(file,
                       sep = ",",
                       dec = ".",
                       na = "",
                       header = FALSE,
                       skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'ir', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 ir )


file <- paste0( path, 'BASE,ir,Male,SGO.csv')
ir_male <- read.table(file,
                     sep = ",",
                     dec = ".",
                     na = "",
                     header = FALSE,
                     skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'ir', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 ir )

ir <- rbind( ir_female, ir_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

## Lectura de probabilidad de transición er^{s}_{g,x,t}-----------------------------------------------

file <- paste0( path, 'BASE,er,Female,SGO.csv')
er_female <- read.table(file,
                        sep = ",",
                        dec = ".",
                        na = "",
                        header = FALSE,
                        skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'er', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 er )


file <- paste0( path, 'BASE,er,Male,SGO.csv')
er_male <- read.table(file,
                      sep = ",",
                      dec = ".",
                      na = "",
                      header = FALSE,
                      skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'er', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 er )

er <- rbind( er_female, er_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )


## Lectura de probabilidad de transición qi^{s}_{g,x,t}-----------------------------------------------

file <- paste0( path, 'BASE,qi,Female.csv')
qi_female <- read.table(file,
                        sep = ",",
                        dec = ".",
                        na = "",
                        header = FALSE,
                        skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'qi', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 106 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 qi )


file <- paste0( path, 'BASE,qi,Male.csv')
qi_male <- read.table(file,
                      sep = ",",
                      dec = ".",
                      na = "",
                      header = FALSE,
                      skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'edad', value = 'qi', v3:v43, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2020:2060), 106 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 qi )

qi <- rbind( qi_female, qi_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

#Lectura de la fuerza laboral-----------------------------------------------------------------------

file <- paste0( path, 'BASE,LF,Female.csv')
LF_female <- read.table(file,
                           sep = ",",
                           dec = ".",
                           na = "",
                           header = FALSE,
                           skip = 2 ) %>% clean_names() %>%
  mutate( sexo = female ) %>%
  dplyr::select( sexo,
                 t := v2,
                 LF:= v3 )

file <- paste0( path, 'BASE,LF,Male.csv')
LF_male <- read.table(file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  mutate( sexo = male ) %>%
  dplyr::select( sexo,
                 t := v2,
                 LF:= v3 )


LF <- rbind( LF_female, LF_male ) %>%
  arrange( t, sexo ) %>%
  as_tibble( )



#Afiliados------------------------------------------------------------------------------------------
##Lectura de afiliados activos al SGO por edad, sexo y año------------------------------------------

actgx_male <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,actgxc,Male,SGO,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( actgx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = male,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   actgx )
  
    if (dim(actgx_male)[1] == 1) {
      actgx_male <- aux
    } else if (dim(aux)[1] > 0) {
      actgx_male <- rbind( actgx_male, aux )
    }
  }

actgx_female <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,actgxc,Female,SGO,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( actgx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = female,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   actgx )
  
  if (dim(actgx_female)[1] == 1) {
    actgx_female <- aux
  } else if (dim(aux)[1] > 0) {
    actgx_female <- rbind( actgx_female, aux )
  }
}

actgx <- rbind( actgx_male, actgx_female ) %>%
  as_tibble( )


##Lectura de inactivos al SGO por edad, sexo y año---------------------------------------------------

inactx_male <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,inactxc,Male,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( inactx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = male,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   inactx )
  
  if (dim(inactx_male)[1] == 1) {
    inactx_male <- aux
  } else if (dim(aux)[1] > 0) {
    inactx_male <- rbind( inactx_male, aux )
  }
}

inactx_female <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,inactxc,Female,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( inactx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = female,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   inactx )
  
  if (dim( inactx_female )[1] == 1) {
    inactx_female <- aux
  } else if ( dim( aux )[1] > 0) {
    inactx_female <- rbind( inactx_female, aux )
  }
}

inactx <- rbind( inactx_male, inactx_female ) %>%
  as_tibble( )

##Lectura de nuevas entradas-------------------------------------------------------------------------

file <- paste0( path, 'BASE,nentx,Female,SGO.csv')
nentx_female <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'nentx', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 nentx )


file <- paste0( path, 'BASE,nentx,Male,SGO.csv')
nentx_male <- read.table( file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'nentx', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 nentx )


nentx <- rbind( nentx_female, nentx_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )


#Jubilados de vejez---------------------------------------------------------------------------------
## Lectura de jubilados de vejez por edad, sexo y año------------------------------------------------

file <- paste0( path, 'BASE,oldage,Female.csv')
oldage_female <- read.table(file,
                            sep = ",",
                            dec = ".",
                            na = "",
                            header = FALSE,
                            skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'oldage', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 91 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 oldage )


file <- paste0( path, 'BASE,oldage,Male.csv')
oldage_male <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'oldage', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 91 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 oldage )


oldage <- rbind( oldage_female, oldage_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )


## Lectura de beneficio promedio de jubilados de vejez por edad, sexo y año--------------------------

file <- paste0( path, 'BASE,oldage_ben,Female.csv')
oldage_ben_female <- read.table(file,
                                sep = ",",
                                dec = ".",
                                na = "",
                                header = FALSE,
                                skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'oldage_ben', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 91 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 oldage_ben )


file <- paste0( path, 'BASE,oldage_ben,Male.csv')
oldage_ben_male <- read.table(file,
                              sep = ",",
                              dec = ".",
                              na = "",
                              header = FALSE,
                              skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'oldage_ben', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 91 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 oldage_ben )

oldage_ben <- rbind( oldage_ben_female, oldage_ben_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

# Nuevos jubilados----------------------------------------------------------------------------------
##Lectura de nuevos jubilados por edad, sexo y año -------------------------------------------------

file <- paste0( path, 'BASE,nretx,Female.csv')
nret_female <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'nret', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 16 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 nret )


file <- paste0( path, 'BASE,nretx,Male.csv')
nret_male <- read.table( file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'nret', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 16 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 nret )


nret <- rbind( nret_female, nret_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )



##Lectura de nuevos jubilados de vejez, que estaban activos por edad, sexo y año---------------------

nretfactgx_male <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,nretfactgxc,Male,SGO,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( nretfactgx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = male,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   nretfactgx )
  
  if (dim(nretfactgx_male)[1] == 1) {
    nretfactgx_male <- aux
  } else if (dim(aux)[1] > 0) {
    nretfactgx_male <- rbind( nretfactgx_male, aux )
  }
}

nretfactgx_female <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,nretfactgxc,Female,SGO,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( nretfactgx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = female,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   nretfactgx )
  
  if (dim( nretfactgx_female )[1] == 1) {
    nretfactgx_female <- aux
  } else if ( dim( aux )[1] > 0) {
    nretfactgx_female <- rbind( nretfactgx_female, aux )
  }
}

nretfactgx <- rbind( nretfactgx_male, nretfactgx_female ) %>%
  as_tibble( )


##Lectura de nuevos jubilados que estaban inactivos de vejez IVM por edad, sexo y año----------------

nretfinactx_male <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,nretfinactxc,Male,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( nretfinactx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = male,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   nretfinactx )
  
  if (dim(nretfinactx_male)[1] == 1) {
    nretfinactx_male <- aux
  } else if (dim(aux)[1] > 0) {
    nretfinactx_male <- rbind( nretfinactx_male, aux )
  }
}

nretfinactx_female <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,nretfinactxc,Female,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( nretfinactx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = female,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   nretfinactx )
  
  if (dim( nretfinactx_female )[1] == 1) {
    nretfinactx_female <- aux
  } else if ( dim( aux )[1] > 0) {
    nretfinactx_female <- rbind( nretfinactx_female, aux )
  }
}

nretfinactx <- rbind( nretfinactx_male, nretfinactx_female ) %>%
  as_tibble( )



#Lectura de inactivos viejos al SGO por edad, sexo y año--------------------------------------------

inact_oldage_male <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,inact_oldage_ben,Male,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( inact_oldage =  rowSums( .[ 3:ncol(.) ] ),
            sexo = male,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   inact_oldage )
  
  if (dim(inact_oldage_male)[1] == 1) {
    inact_oldage_male <- aux
  } else if (dim(aux)[1] > 0) {
    inact_oldage_male <- rbind( inact_oldage_male, aux )
  }
}

inact_oldage_female <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,inact_oldage_ben,Female,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( inact_oldage =  rowSums( .[ 3:ncol(.) ] ),
            sexo = female,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   inact_oldage )
  
  if (dim( inact_oldage_female )[1] == 1) {
    inact_oldage_female <- aux
  } else if ( dim( aux )[1] > 0) {
    inact_oldage_female <- rbind( inact_oldage_female, aux )
  }
}

inact_oldage <- rbind( inact_oldage_male, inact_oldage_female ) %>%
  as_tibble( )



#Jubilados por Viudez-------------------------------------------------------------------------------
## Lectura de jubilados de viudez por edad, sexo y año-----------------------------------------------

file <- paste0( path, 'BASE,wid,Female.csv')
wid_female <- read.table(file,
                            sep = ",",
                            dec = ".",
                            na = "",
                            header = FALSE,
                            skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'wid', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 106 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 wid )


file <- paste0( path, 'BASE,wid,Male.csv')
wid_male <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'wid', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 106 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 wid )


wid <- rbind( wid_female, wid_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

# Jubilados por orfandad----------------------------------------------------------------------------
## Lectura de jubilados de orfandad por edad, sexo y año--------------------------------------------

file <- paste0( path, 'BASE,orph,Female.csv')
orph_female <- read.table(file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'orph', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 106 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 orph )


file <- paste0( path, 'BASE,orph,Male.csv')
orph_male <- read.table( file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'orph', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 106 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 orph )


orph <- rbind( orph_female, orph_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )


#Jubilados por invalidez----------------------------------------------------------------------------
## Lectura de jubilados de invalidez por edad, sexo y año-------------------------------------------

file <- paste0( path, 'BASE,dis,Female.csv')
dis_female <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'dis', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 90 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 dis )


file <- paste0( path, 'BASE,dis,Male.csv')
dis_male <- read.table( file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'dis', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 90 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 dis )


dis <- rbind( dis_female, dis_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

##Lectura de nuevos jubilados de invalidez por edad, sexo y año--------------------------------------

ndisgx_male <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,ndisgxc,Male,SGO,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( ndisgx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = male,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   ndisgx )
  
  if (dim(ndisgx_male)[1] == 1) {
    ndisgx_male <- aux
  } else if (dim(aux)[1] > 0) {
    ndisgx_male <- rbind( ndisgx_male, aux )
  }
}

ndisgx_female <- as.data.frame(1)

for (anio in 2021:2060) {
  file <- paste0( path, 'BASE,ndisgxc,Female,SGO,', anio, '.csv')
  aux <- read.table(file,
                    sep = ",",
                    dec = ".",
                    na = "",
                    header = FALSE,
                    skip = 2 ) %>% clean_names() %>%
    mutate( ndisgx =  rowSums( .[ 3:ncol(.) ] ),
            sexo = female,
            t = anio ) %>%
    dplyr::select( t,
                   sexo,
                   x := v2,
                   ndisgx )
  
  if (dim( ndisgx_female )[1] == 1) {
    ndisgx_female <- aux
  } else if ( dim( aux )[1] > 0) {
    ndisgx_female <- rbind( ndisgx_female, aux )
  }
}

ndisgx <- rbind( ndisgx_male, ndisgx_female ) %>%
  as_tibble( )

#Masa Salarial--------------------------------------------------------------------------------------

## Lectura Salario promedio-------------------------------------------------------------------------
file <- paste0( path, 'BASE,sal,Female,SGO.csv')
sal_female <- read.table(file,
                           sep = ",",
                           dec = ".",
                           na = "",
                           header = FALSE,
                           skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'sal', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 sal )


file <- paste0( path, 'BASE,sal,Male,SGO.csv')
sal_male <- read.table( file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'sal', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 sal )


sal <- rbind( sal_female, sal_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )


## Lectura masa salarial por sexo y año-------------------------------------------------------------

file <- paste0( path, 'BASE,SALM,Female.csv')
sal_female <- read.table(file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  mutate( sexo = female ) %>%
  dplyr::select( t := v2,
                 sexo,
                 salm := v3 )


file <- paste0( path, 'BASE,SALM,Male.csv')
sal_male <- read.table(file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  mutate( sexo = male ) %>%
  dplyr::select( t := v2,
                 sexo,
                 salm := v3 )

salg <- rbind( sal_female, sal_male ) %>%
  arrange( t, sexo ) %>%
  as_tibble( )

##Lectura de asg-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,asg,SGO.csv')
asg <- read.table(file,
                  sep = ",",
                  dec = ".",
                  na = "",
                  header = FALSE,
                  skip = 2 ) %>% clean_names() %>%
  dplyr::select( t := v2,
                 asg := v3 )

##Lectura de Isal-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,Isal,Female,SGO.csv')
Isal_female <- read.table(file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  mutate( sexo = female ) %>%
  dplyr::select( sexo,
                 x := v2,
                 Isal:= v3 )

file <- paste0( path, 'BASE,Isal,Male,SGO.csv')
Isal_male <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  mutate( sexo = male ) %>%
  dplyr::select( sexo,
                 x := v2,
                 Isal:= v3 )


Isal <- rbind( Isal_female, Isal_male ) %>%
  arrange( sexo, x ) %>%
  as_tibble( )

##Lectura de csact-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,csact,Female,SGO.csv')
csact_female <- read.table(file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'csact', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 55 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 csact )


file <- paste0( path, 'BASE,csact,Male,SGO.csv')
csact_male <- read.table( file,
                        sep = ",",
                        dec = ".",
                        na = "",
                        header = FALSE,
                        skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'csact', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 55 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 csact )


csact <- rbind( csact_female, csact_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )


##Lectura de csinact-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,csinact,Female.csv')
csinact_female <- read.table(file,
                           sep = ",",
                           dec = ".",
                           na = "",
                           header = FALSE,
                           skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'csinact', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 55 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 csinact )


file <- paste0( path, 'BASE,csinact,Male.csv')
csinact_male <- read.table( file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'csinact', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 55 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 csinact )


csinact <- rbind( csinact_female, csinact_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

##Lectura de ITsal-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,ITsal,Female,SGO.csv')
ITsal_female <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  mutate( sexo = female ) %>%
  dplyr::select( sexo,
                 x := v2,
                 ITsal:= v3 )

file <- paste0( path, 'BASE,ITsal,Male,SGO.csv')
ITsal_male <- read.table(file,
                        sep = ",",
                        dec = ".",
                        na = "",
                        header = FALSE,
                        skip = 2 ) %>% clean_names() %>%
  mutate( sexo = male ) %>%
  dplyr::select( sexo,
                 x := v2,
                 ITsal:= v3 )


ITsal <- rbind( ITsal_female, ITsal_male ) %>%
  arrange( sexo, x ) %>%
  as_tibble( )




##Lectura de cent-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,cent,Female,SGO.csv')
cent_female <- read.table(file,
                             sep = ",",
                             dec = ".",
                             na = "",
                             header = FALSE,
                             skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'cent', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 cent )


file <- paste0( path, 'BASE,cent,Male,SGO.csv')
cent_male <- read.table( file,
                            sep = ",",
                            dec = ".",
                            na = "",
                            header = FALSE,
                            skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'cent', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 cent )


cent <- rbind( cent_female, cent_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )



##Lectura de Tsal-----------------------------------------------------------------------------------

file <- paste0( path, 'BASE,Tsal,Female,SGO.csv')
Tsal_female <- read.table(file,
                          sep = ",",
                          dec = ".",
                          na = "",
                          header = FALSE,
                          skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'Tsal', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = female ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 Tsal )


file <- paste0( path, 'BASE,Tsal,Male,SGO.csv')
Tsal_male <- read.table( file,
                         sep = ",",
                         dec = ".",
                         na = "",
                         header = FALSE,
                         skip = 2 ) %>% clean_names() %>%
  gather( ., key = 'anio', value = 'Tsal', v3:v42, factor_key = FALSE ) %>%
  arrange( v2 ) %>%
  mutate( t = rep( c(2021:2060), 56 ) ) %>%
  arrange( t ) %>%
  mutate( sexo = male ) %>%
  dplyr::select( t,
                 sexo,
                 x := v2,
                 Tsal )


Tsal <- rbind( Tsal_female, Tsal_male ) %>%
  arrange( t, sexo, x ) %>%
  as_tibble( )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando en un solo data.frame' )

save( q,
      ir,
      er,
      qi,
      LF,
      actgx,
      inactx,
      nentx,
      oldage,
      oldage_ben,
      nret,
      nretfactgx,
      nretfinactx,
      inact_oldage,
      wid,
      orph,
      dis,
      ndisgx,
      sal,
      salg,
      Isal,
      csact,
      csinact,
      asg,
      ITsal,
      cent,
      Tsal,
      female,
      male,
      file = paste0( parametros$RData, 'ILO_pension_inputs.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
