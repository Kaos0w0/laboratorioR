#--------------------------------------------------#
#- 1. Pre-procesamiento y limpieza de los datos ---#
#--------------------------------------------------#

# Primero, se instala el paquete pacman, que permite instalar
# paquetes de forma eficiente, comprobando primero si ya están
# instalados o no, para posteriormente, cargarlos (p_load).

install.packages("pacman")


# Para trabajar con archivos de excel, se puede usar el paquete readxl
# Uso: read_xlsx(archivo, IndiceDeHoja, col_names=TRUE)
# (Añadido a la función p_load en la posición 0)

# Se hace uso de la función p_load, que recibe como parámetros los
# paquetes a instalar y/o cargar:

pacman::p_load("readxl", "stringi", "igraph", "editrules", "ggplot2",
               "naniar", "gridExtra", "mice")  

# Se añade un archivo R donde irán las funciones provenientes de
# librerías, pero que por alguna razón necesiten ser modificadas

source("funciones.R")

#--------- a) Lectura de la hoja y adecuación de variables ---------#


# Una vez importado el paquete readxl, se guardan los datos del archivo.
# El destino es la variable llamada paises.

paises <- read_xls("paises.xls", 1, col_names=TRUE)

paises            # Visualización de los datos en consola
summary(paises)   # Información de cada atributo en cuestión

# Se procede a usar la librería stringi, útil para el procesamiento
# de textos y cadenas. En este caso, se usará para homogeneizar las
# cadenas correspondientes a los grupos. Ej: ("FunCión" --> "funcion")
# (Añadida en la posición 1 de p_load [linea 18])

# stri_trans_general, en este caso, recibe un vector de cadenas, 
# y una tabla de codificación, retorna un vector homogeneizado.
# toupper, lleva las cadenas a mayúsculas.

grupos <- toupper(stri_trans_general(paises$GRUPOS,"Latin-ASCII"))

# El atributo "GRUPOS", pasa a ser un factor:

paises <- transform(paises, GRUPOS = factor(grupos))
str(paises)   # Se comprueba la conversión

#--------- b) Creación de reglas de consistencia ---------#

# Para aplicar las reglas de verificación de consistencia, se hace uso
# del paquete editrules
# Uso: editfile(txt.txt) lee las reglas de un archivo de texto.
# (Añadido en la posición 3 de p_load [linea 18])
# En la posición 2 se ha añadido igraph, necesario para editrules.

reglas <- editrules::editfile("consistencia.txt")
reglas # Vistazo a las reglas

#--------- c) Aplicación de las reglas de consistencia ---------#

# En "violaciones_Consistencia" se guardan booleanos que indican violaciones 
# en las reglas de consistencia por cada registro. Con summary se presenta 
# información sobre ellos.

violaciones_Consistencia <- editrules::violatedEdits(reglas, paises)
summary(violaciones_Consistencia)

# Si summary(violacionesConsistencia) retorna "No violations detected" 
# significa que todos los valores cumplen con las reglas, a excepción de
# los evaluados a NA. Representan los valores que hacen falta en los datos.

# Con dev.new() creamos una ventana donde se alojarán plots

dev.new()

# Función basada en plot.violatedRules de la librería editrules.
# Se hacen cambios en los ejes, etiquetas y formas de mostrar
# la información. Puede verla en funciones.R. Crea un plot donde
# se muestra la información referente a la violación de reglas
# por parte de los registros

vrPlot(violaciones_Consistencia)

#--------- d) Identificación de datos faltantes ---------#

# Para identificar visualmente cuantos datos faltantes tiene una columna,
# se usará la función gg_miss_var() proveniente de la librería ggplot2
# (Añadida en la posición 4 de p_load)

plot1 <- gg_miss_var(paises) + labs(y = "Número de datos faltantes") +
ggtitle("Datos faltantes por variable") +
theme(plot.title = element_text(hjust = 0.5)) +
scale_y_continuous(breaks = seq(0, 20, by = 2))

# Para identificar visualmente datos faltantes en un grupo de datos,
# se usará la función vis_miss() proveniente de la librería naniar
# (Añadida en la posición 5 de p_load)

plot2 <- vis_miss(paises)

# Se crea una ventana que presenta la vista general de datos faltantes,
# junto con un plot que muestra específicamente cuantos de estos hay por
# columna. Para dividir la ventana en dos, se usa la función grid.arrange,
# proveniente de la librería gridExtra.
# (Añadida en la posición 6 de p_load)

x11(width = 12, height = 10)
grid.arrange(plot2, plot1, ncol=2)

# Se muestra en consola las columnas con datos faltantes, indicando su
# posición exacta (fila en la que se encuentran)

faltantes <- sapply(paises, function(x) which(is.na(x)))
faltantes <- faltantes[!(faltantes %in% list(c(integer(0))))]
faltantes

# Se guardan los nombres de las columnas con datos faltantes para poder
# acceder a ellas a la hora de imputar.

nombres <- names(faltantes)

#--------- e) Imputación de datos faltantes ---------#

# La imputación se hará con regresión estocástica, con ayuda de la
# función mice, proveniente de la librería con el mismo nombre.
# (Añadida en la posición 7 de p_load)

imputacion <- mice(paises, method = "norm.nob", m = 1) # Impute data
paises <- complete(imputacion)

# Se guardan los resultados en un archivo

save(paises, file="paises_Limpios.RData")

##SEGUNDA PARTE - PARTE GRÁFICA

##-------------PRIMER PUNTO-----------
#Se cargan los datos limpios
load("paises_Limpios.RData")

#Definición de variable grupos
distribucion <- table(paises$GRUPOS)
grupos <- c("AFRICA",
            "ASIA",
            "EO-NA_JAPON_AUSTR_NZ",
            "EUROPA ORIENTAL",
            "IBEROAMERICA",
            "ORIENTE MEDIO")

x11()
barplot(distribucion,
        main = "Distribución paises por grupo",
        xlab = "Grupos",
        ylab = "Cantidad de paises",
        ylim = c(0,35),
        font.axis = 4,
        names.arg = c(" "," "," "," "," "," "),
        col = c("blue","green","purple","red","pink","yellow"))
legend(x = "topright", legend = c(grupos),
       fill = c("blue","green","purple","red","pink","yellow"),
       bty = "n")

##-------------SEGUNDO PUNTO-----------
x11()
par(mfrow=c(1,3)) #definir el espacio de trabajo para 3 boxplots

#creacion del primer boxplot
boxplot(paises$Tasa.mortalidad ~ paises$Grupo,
        names = c("","","","","",""),
        main = "Tasa Mortalidad Por grupos",
        ylab = "Tasa Mortalidad",
        ylimit = c(0,32),
        xlab = "Grupos",
        col = c("blue","green","purple","red","pink","yellow"))

#inserción de una leyenda para facilitar lectura
legend(x = "topleft", legend = c(grupos),
       fill = c("blue","green","purple","red","pink","yellow"),
       bty = "n")

#segundo boxplot
boxplot(paises$Tasa.natalidad ~ paises$Grupo,
        names = c("","","","","",""),
        main = "Tasa Natalidad por grupos",
        ylab = "Tasa Natalidad",
        ylimit = c(0,57),
        xlab = "Grupos",
        col = c("blue","green","purple","red","pink","yellow"))

##leyenda de datos
legend(x = "topleft", legend = c(grupos),
       fill = c("blue","green","purple","red","pink","yellow"),
       bty = "n")

#tercer boxplot
boxplot(paises$Mortalidad.infantil ~ paises$Grupo,
        names = c("","","","","",""),
        main = "Tasa Mortalidad Infantil por grupos",
        ylab = "Tasa Mortalidad Infantil",
        xlab = "Grupos",
        col = c("blue","green","purple","red","pink","yellow"))

#leyenda de datos
legend(x = "topleft", legend = c(grupos),
       fill = c("blue","green","purple","red","pink","yellow"),
       bty = "n")

##-------------TERCER PUNTO-----------

#Se modifica el Dataframe para añadir una nueva columna llamada "PNB.per.capita"
paises <- dplyr::mutate(paises,
                              PNB.per.capita = PNB/Población..miles.)

coloresPaises <- c("blue","green","purple","red","pink","yellow")
nombresVacios <- c("","","","","","")

#Realiza un respecivo boxplot de PNB.per.capita a partir de los grupos (DATOS GRANDES)
x11()
par(mfrow=c(1,2))
boxplot(PNB.per.capita ~ GRUPOS, 
        data = paises,
        col = coloresPaises,
        xlab = "Grupos de países",
        ylab = "PNB per cápita",
        main = "PNB per cápita por grupos de países",
        names = nombresVacios)

legend("topleft",
       legend = c("AFRICA","ASIA","EO-NA_JAPON_AUSTR_NZ","EUROPA ORIENTAL","IBEROAMERICA","ORIENTE MEDIO"),
       fill = coloresPaises,
       inset = c(0.03, 0.03),
       bg = "white",
       bty = "n")

#Realiza un respecivo boxplot de PNB.per.capita a partir de los grupos (DATOS PEQUEÑOS)
datosLimpiosSinPaisesGrandes <- dplyr::filter(paises,
                                              !(GRUPOS %in% c("EO-NA_JAPON_AUSTR_NZ", "ORIENTE MEDIO")))
boxplot(PNB.per.capita ~ GRUPOS, 
        data = datosLimpiosSinPaisesGrandes,
        col = coloresPaises,
        xlab = "Grupos de países",
        ylab = "PNB per cápita",
        ylim = c(0,0.3),
        main = "PNB per cápita por grupos de países",
        names = nombresVacios)

##-------------CUARTO PUNTO-------------
#Obtenemos los respectivos percentiles
percentiles <- quantile(paises$PNB.per.capita)

#Añadimos el respectivo nivel de pobreza al respectivo dataframe
paises <- dplyr::mutate(paises, nivel.pobreza = dplyr::case_when(
  PNB.per.capita < percentiles[2] ~ "Bajo",
  PNB.per.capita < percentiles[3] ~ "Medio Bajo",
  PNB.per.capita < percentiles[4] ~ "Medio Alto",
  PNB.per.capita <= percentiles[5] ~ "Alto"
))

#Creamos tablas de frecuencias a partir de cada país
tfAf <- dplyr::filter(paises, GRUPOS == "AFRICA")$nivel.pobreza
tfAf <- table(factor(tfAf, levels=c("Alto","Medio Alto","Medio Bajo","Bajo")))
tfAf <- tfAf/sum(tfAf)

tfAs <- dplyr::filter(paises, GRUPOS == "ASIA")$nivel.pobreza
tfAs <- table(factor(tfAs, levels=c("Alto","Medio Alto","Medio Bajo","Bajo")))
tfAs <- tfAs/sum(tfAs)

tfEo <- dplyr::filter(paises, GRUPOS == "EO-NA_JAPON_AUSTR_NZ")$nivel.pobreza
tfEo <- table(factor(tfEo, levels=c("Alto","Medio Alto","Medio Bajo","Bajo")))
tfEo <- tfEo/sum(tfEo)

tfEor <- dplyr::filter(paises, GRUPOS == "EUROPA ORIENTAL")$nivel.pobreza
tfEor <- table(factor(tfEor, levels=c("Alto","Medio Alto","Medio Bajo","Bajo")))
tfEor <- tfEor/sum(tfEor)

tfIb <- dplyr::filter(paises, GRUPOS == "IBEROAMERICA")$nivel.pobreza
tfIb <- table(factor(tfIb, levels=c("Alto","Medio Alto","Medio Bajo","Bajo")))
tfIb <- tfIb/sum(tfIb)

tfOm <- dplyr::filter(paises, GRUPOS == "ORIENTE MEDIO")$nivel.pobreza
tfOm <- table(factor(tfOm, levels=c("Alto","Medio Alto","Medio Bajo","Bajo")))
tfOm <- tfOm/sum(tfOm)


#Creamos un Data Frame que nos servirá para realizar el gráfico
data <- data.frame(tfAf, tfAs, tfEo, tfEor, tfIb, tfOm)
#Elimanos a que nivel de pobreza pertenece
data<- data[,-c(1,3,5,7,9,11)]
#Le asignamos el grupo de país al que pertenece cada columna
colnames(data) <- c("AFRICA","ASIA","EO-NA_JAPON_AUSTR_NZ","EUROPA ORIENTAL","IBEROAMERICA","ORIENTE MEDIO")

#Realizamos el barplot de múltiples columnas.
x11()
barplot(as.matrix(data),
        main="Niveles de pobreza por grupos de países",
        
        # setting y label only
        # because x-label will be our
        # barplots name
        ylab="Cantidad",
        xlab="Grupos de países",
        
        # to plot the bars vertically
        beside=TRUE,
        
        col=c("#D8E9A8","#4E9F3D","#1E5128","#191A19"),
        ylim = c(0,1)
)

#Añadimos legenda con información de los colores
legend("topright",
       legend = c("Alto","Medio alto","Medio bajo","Bajo"),
       fill = c("#D8E9A8","#4E9F3D","#1E5128","#191A19"),
       inset = c(0.03, 0.03),
       bg = "white",
       bty = "n")
