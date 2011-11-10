netProp = function(network, from = "FROMJCT", to = "TOJCT", pred = "pred") {

  if (require(igraph)) {

    rndf = data.frame(FROMJCT = network$FROMJCT, TOJCT = network$TOJCT, 
               OBJECTIT = network$OBJECTID, pred = network$pred)
    igr = graph.data.frame(rndf)
    igrs = topological.sort(igr, mode = "out")
    rndf$to = match(as.character(rndf$TOJCT), V(igr)$name[igrs+1])

    while (TRUE) {
      lcon = which(rndf$to == min(rndf$to[is.na(rndf$pred)]))
      if (length(lcon)==0) break()
      print(lcon)
      while(is.na(rndf$pred[lcon[1]])) lcon = c(neighbors(igr,lcon[1]-1)+1, lcon)
      rndf$pred[lcon] = rndf$pred[lcon[1]]
    }
    network$pred = rndf$pred
  } else {
    warning("This function will perform faster with igraph installed")
    ichange = 1
    while (ichange > 0) {
      ichange = 0
      for (i in 1:dim(network)[1]) {
        if (!is.na(network@data[i,pred])) {
          tt = which(network@data[,to] == network@data[i,from])
          if (length(tt) > 0 && is.na(network@data[tt,pred])) {
            network@data[tt, pred] = network@data[i, pred]
            ichange = ichange + 1
          }
        }
      }
      print(paste("ichange",ichange))
    }
  }
  network
}
