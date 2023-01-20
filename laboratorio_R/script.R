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

pacman::p_load("readxl", "stringi", "igraph", "editrules", "ggplot2")  

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

# Para identificar visualmente datos faltantes, se usará la función
# gg_miss_var() proveniente de la librería naniar.
# (Añadida en la posición 4 de p_load)

dev.new()
gg_miss_var(paises) + labs(y = "Número de datos faltantes") +
ggtitle("Datos faltantes por variable") +
theme(plot.title = element_text(hjust = 0.5)) +
scale_y_continuous(breaks = seq(0, 20, by = 2))




