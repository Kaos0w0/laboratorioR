# Función proveniente de la librería editrules.

vrPlot <- function(x, topn=min(10,ncol(x))){
  x[is.na(x)] <- TRUE
  cnt <- table(rowSums(x))
  ner <- as.integer(names(cnt))
  cnt <- as.integer(cnt)
  
  noerr <- ner==0
  nnoer <- sum(cnt[noerr],0)
  ner <- ner[!noerr]
  cnt <- cnt[!noerr]
  lgcrit <- 50
  lg <- ''
  if ( max(ner) > lgcrit ) lg <- paste(lg,'x',sep='')
  if ( max(cnt) > lgcrit ) lg <- paste(lg,'y',sep='')
  plot( ner,cnt,
        , main=  "Violaciones por registro"
        , xlab = "Número de violaciones"
        , ylab = "Cantidad de registros"
        , log=lg
        , xaxp  = c(1, max(ner), max(ner)-1)
  )
  mtext(paste(nnoer,'registros sin violaciones'),side=3,line=0)
}