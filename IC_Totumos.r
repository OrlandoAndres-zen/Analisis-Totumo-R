datos <- read.csv("~/ProyectoTotumo/totumo_base_datos.csv")

head(datos)
str(datos)
dim(datos)
names(datos)

#################################################
##########     FUNTION  IC PARA σ²     ##########
#################################################

IC_sig2 <- function(x){

  x <- na.omit(x)

  n <- length(x)
  s2 <- var(x)

  cat("Varianza muestral:", s2, "\n")
  cat("Desviación estándar muestral:", sqrt(s2), "\n")

  numerador <- (n - 1) * s2

  LI <- numerador / qchisq(0.975, n - 1)
  LS <- numerador / qchisq(0.025, n - 1)

  IC_var <- c(LI, LS)
  IC_sd <- sqrt(IC_var)

  cat("IC(σ²):", IC_var, "\n")
  cat("IC(σ):", IC_sd, "\n")

 
}

########################|*|########################
##############     PESOS TOTALES     ##############
###################################################

pesos_totales <- datos$PESO_TOTAL

pesos_totales

length(pesos_totales)

##############  PESO TOTAL ARBOL 1 ##############
arbol1 <- datos[datos$ARBOL == 1, ]
peso_arbol1 <- datos$PESO_TOTAL[datos$ARBOL == 1]

peso_arbol1

length(peso_arbol1)

############## PESO TOTAL ARBOL 2 ##############

peso_arbol2 <- datos$PESO_TOTAL[datos$ARBOL == 2]

peso_arbol2

length(peso_arbol2)

############## INTERVALOS DE CONFIANZA PARA μ CON σ² DESCONOCIDA ##############

       # Todos los totumos
       t.test(pesos_totales)

       # Árbol 1
       t.test(peso_arbol1)

       # Árbol 2
       t.test(peso_arbol2)

############## COMPARACIÓN DE MEDIAS ##############

       ## Asumiendo varianzas iguales ##

              # Árbol 1 - Árbol 2
              t.test(peso_arbol1, peso_arbol2,
                     var.equal = TRUE,
                     conf.level = 0.95)

              # Árbol 2 - Árbol 1

              t.test(peso_arbol2, peso_arbol1,
                     var.equal = TRUE,
                     conf.level = 0.95)

       ## Sin asumir varianzas iguales (Welch) ##

              # Árbol 1 - Árbol 2
              t.test(peso_arbol1, peso_arbol2,
                     var.equal = FALSE,
                     conf.level = 0.95)

              # Árbol 2 - Árbol 1

              t.test(peso_arbol2, peso_arbol1,
                     var.equal = FALSE,
                     conf.level = 0.95)


############## INTERVALOS DE CONFIANZA PARA σ² CON μ DESCONOCIDA ##############

       # Varianza y desviación estándar de todos los totumos
       IC_sig2(pesos_totales)

       # Varianza y desviación estándar del árbol 1
       IC_sig2(peso_arbol1)

       # Varianza y desviación estándar del árbol 2
       IC_sig2(peso_arbol2)

########################|*|########################


########################|*|########################
##############     PESOS INTERNO     ##############
###################################################

pesos_int <- datos$PESO_INTERNO

pesos_int

length(pesos_int)

##############  PESO INTERNO ARBOL 1 ##############

pesoint_arbol1 <- datos$PESO_INTERNO[datos$ARBOL == 1]

pesoint_arbol1

length(pesoint_arbol1)

############## PESO INTERNO ARBOL 2 ##############

pesoint_arbol2 <- datos$PESO_INTERNO[datos$ARBOL == 2]

pesoint_arbol2

length(pesoint_arbol2)

############## INTERVALOS DE CONFIANZA PARA μ CON σ² DESCONOCIDA ##############

       # Todos los totumos
       t.test(pesos_int)

       # Árbol 1
       t.test(pesoint_arbol1)

       # Árbol 2
       t.test(pesoint_arbol2)

############## COMPARACIÓN DE MEDIAS ##############

       ## Asumiendo varianzas iguales ##

              # Árbol 1 - Árbol 2
              t.test(pesoint_arbol1, pesoint_arbol2,
                     var.equal = TRUE,
                     conf.level = 0.95)

              # Árbol 2 - Árbol 1

              t.test(pesoint_arbol2, pesoint_arbol1,
                     var.equal = TRUE,
                     conf.level = 0.95)

       ## Sin asumir varianzas iguales (Welch) ##

              # Árbol 1 - Árbol 2
              t.test(pesoint_arbol1, pesoint_arbol2,
                     var.equal = FALSE,
                     conf.level = 0.95)

              # Árbol 2 - Árbol 1

              t.test(pesoint_arbol2, pesoint_arbol1,
                     var.equal = FALSE,
                     conf.level = 0.95)


############## INTERVALOS DE CONFIANZA PARA σ² CON μ DESCONOCIDA ##############

       # Varianza y desviación estándar de todos los totumos
       IC_sig2(pesos_int)

       # Varianza y desviación estándar del árbol 1
       IC_sig2(pesoint_arbol1)

       # Varianza y desviación estándar del árbol 2
       IC_sig2(pesoint_arbol2)

########################|*|########################


########################|*|########################
############     PESOS SUPERFICIE     #############
###################################################

pesos_sup <- datos$PESO_SUPERFICIE

pesos_sup

length(pesos_sup)

##############  PESO SUPERFICIE ARBOL 1 ##############

pesosup_arbol1 <- datos$PESO_SUPERFICIE[datos$ARBOL == 1]

pesosup_arbol1

length(pesosup_arbol1)

############## PESO SUPERFICIE ARBOL 2 ##############

pesosup_arbol2 <- datos$PESO_SUPERFICIE[datos$ARBOL == 2]

pesosup_arbol2

length(pesoint_arbol2)


############ INTERVALOS DE CONFIANZA PARA μ CON σ² DESCONOCIDA ############

       # Todos los totumos
       t.test(pesos_sup)

       # Árbol 1
       t.test(pesosup_arbol1)

       # Árbol 2
       t.test(pesosup_arbol2)

##################### COMPARACIÓN DE MEDIAS #####################

       ## Asumiendo varianzas iguales ##

              # Árbol 1 - Árbol 2
              t.test(pesosup_arbol1, pesosup_arbol2,
                     var.equal = TRUE,
                     conf.level = 0.95)

              # Árbol 2 - Árbol 1

              t.test(pesosup_arbol2, pesosup_arbol1,
                     var.equal = TRUE,
                     conf.level = 0.95)

       ## Sin asumir varianzas iguales (Welch) 

              # Árbol 1 - Árbol 2
              t.test(pesosup_arbol1, pesosup_arbol2,
                     var.equal = FALSE,
                     conf.level = 0.95)

              # Árbol 2 - Árbol 1

              t.test(pesosup_arbol2, pesosup_arbol1,
                     var.equal = FALSE,
                     conf.level = 0.95)


############## INTERVALOS DE CONFIANZA PARA σ² CON μ DESCONOCIDA ##############

       # Varianza y desviación estándar de todos los totumos
       IC_sig2(pesos_sup)

       # Varianza y desviación estándar del árbol 1
       IC_sig2(pesosup_arbol1)

       # Varianza y desviación estándar del árbol 2
       IC_sig2(pesosup_arbol2)

########################|*|########################

