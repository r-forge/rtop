
aOverlap = function(a1,a2) {
# This function is rather clumsy, creating polygons of the raster data, then overlaying the smallest on the larger to find
# overlapping area
  ap1 = as.SpatialPolygons.SpatialPixels(a1)
  ap2 = as.SpatialPolygons.SpatialPixels(a2)
  na1 = length(ap1@polygons)
  na2 = length(ap2@polygons)
  da1 = ap1[1]@polygons[[1]]@area 
  da2 = ap2[1]@polygons[[1]]@area 

  if (da1* na1 > da2*na2) {
    iOver = overlay(a2,ap1)
    return(sum(!is.na(iOver))*da2)
  } else {
    iOver = overlay(a1,ap2)
    return(sum(!is.na(iOver))*da1)
  } 
}





vred = function(a1,a2=NULL,vredTyp = "hyp",
         variogramModel,pdf1=NULL,pdf2=NULL,aover=NULL,dist=NULL,inner = 0,resol=5) {
#  print(asdf)
#  return(0)
  model = variogramModel$model
  param = variogramModel$params
  ci = 0
  if (vredTyp == "ind"){
    if (is(a1,"list")) {
      a2 = a1[[2]]
      a1 = a1[[1]]
    } else if (is.null(a2)) a2 = a1
    a1 = coordinates(a1)
    a2 = coordinates(a2)
    ip1 = dim(a1)[1]
    ip2 = dim(a2)[1]
    vreda = .Fortran("vredind",ci,ip1,ip2,a1,a2,length(param),param,model)
#  } else if (vredTyp == "pdf") {
#    vreda = .Fortran("vredpdf",ci,c1,c2,ip1,ip2,ipb,pdf1,pdf2,pdfb,length(param),param,model)
  } else if (vredTyp == "hyp") {
    vreda = .Fortran("vredhyp",ci,a1,a2,dist,length(param),param,resol,model)
  }
###### Nugget needs to be implemented
  if (!is.null(aover)) {
    nug = 0
  } else {
    nug = 0
  }

  vreda[[1]]
}


summary.rtop.old = function(object, ...) {
  ainfo = object[["ainfo"]]
  print("ainfo:")
  print(summary(ainfo),...)
  if ("areas" %in% names(object)) {
    cat(paste("\n areas:"))
    areas = object$areas
    cat(paste("\n",length(areas), " objects of class", class(areas[[1]])[[1]]),"\n")
  }
  if ("dareas" %in% names(object)) {
    dareas = object$dareas
    cat(paste("\n dareas:"))
    cat(paste("\n",length(areas), " objects of class", class(areas[[1]])),"\n")
  }
 if ("variogram" %in% names(object)) {
    cat(paste("\n variogram \n"))
    variogram = object$variogram
   print(variogram)
  }
  cat(paste("\n"))
  cat(paste("variables in object:"))  
  print(names(object))
}


diagonalBbox = function(bb){
  if (!is.matrix(bb) && !(all(dim(bb) == c(2,2)))) stop("Input is not a boundary box")
  xd = bb[1,2]-bb[1,1]
  yd = bb[2,2]-bb[2,1]
  xdd = xd*xd
  ydd = yd*yd
  sqrt(xdd+ydd)  
}

diagonalBbox = function(bb) {
return(sqrt(bbArea(bb)))
}




rtopMakeGrid = function(a1,a2,distance,resol = 5){
  plen = resol*resol
  p1 = data.frame(id = c(1:plen),x=c(1:plen),y=c(1:plen))
  p2 = data.frame(id = c(1:plen),x=c(1:plen),y=c(1:plen))
  ar1 = sqrt(a1)
  ar2 = sqrt(a2)
  dx1 = ar1/resol
  dx2 = ar2/resol
  dy1 = ar1/resol
  dy2 = ar2/resol
  x01 = 0.5*dx1
  x02 = 0.5*dx2 + distance
  y01 = 0.5*dy1
  y02 = 0.5*dy2
  xkor1 = mapply(xkor,p1$id,MoreArgs = list(x0 = x01,dx = dx1, resol = resol))
  ykor1 = mapply(ykor,p1$id,MoreArgs = list(y0 = y01,dy = dy1, resol = resol))
  p3 = data.frame(x = xkor1,y = ykor1,id = p1$id,ida = 1)

  xkor2 = mapply(xkor,p2$id,MoreArgs = list(x0 = x02,dx = dx2, resol = resol))
  ykor2 = mapply(ykor,p2$id,MoreArgs = list(y0 = y02,dy = dy2, resol = resol))
  p4 = data.frame(x = xkor2,y = ykor2,id = p2$id,ida = 2)

#  coordinates(p3) = ~x+y
#  coordinates(p4) = ~x+y
  p = rbind(p3,p4)
  return(p)
}



xkor = function(x0,dx,id,resol) {
  xid = (id-1) %/% resol
  x = xid*dx+x0
  return(x)
}

ykor = function(y0,dy,id,resol) {
  yid = (id-1) %% resol
  y = yid*dy+y0
  return(y)
}  



bbZoom = function(bb,areas){
  newpoints = spsample(bb,16,"regular",offset = c(0.5,0.5))
  gridded(newpoints) = TRUE
  cs = newpoints@grid@cellsize[1]/2
  pts = spsample(areas,25,"regular",offset=c(0.5,0.5))
  Srl = list()
  j = 0
  for (i in 1:dim(coordinates(newpoints))[1]) {
    pt1 = coordinates(newpoints)[i,]
    x1 = pt1[1]-cs
    x2 = pt1[1]+cs
    y1 = pt1[2]-cs
    y2 = pt1[2]+cs
    boun = data.frame(x=c(x1,x2,x2,x1,x1),y=c(y1,y1,y2,y2,y1))
    coordinates(boun) = ~x+y
    boun = Polygon(boun)
    bounp = Polygons(list(boun),ID = as.character(i))
    SP = SpatialPolygons(list(bounp))
    lin = overlay(pts,SP)
    t(bbox(SP))
     t(bbox(areas))
     if (!all(is.na(lin))) {
      j = j + 1
      Srl[[j]] = bounp
    }
  }
  Srl = SpatialPolygons(Srl)
  bb = as.data.frame(t(bbox(Srl)))
  coordinates(bb) = ~r1+r2
  print(length(Srl@polygons))
  if (bbArea(bbox(bb))/bbArea(bbox(areas)) > 100) bb = bbZoom(bb,areas)
  print(bb)
  bb
}


rtopSample = function(bb,areas,resol) {
  
  if (bbArea(bbox(bb))/bbArea(bbox(areas)) > 100) bb = bbZoom(bb,areas)
  lres = 1
  for (pow in 1:100) {
    lres = lres*4
    pp = spsample(bb,lres,"regular",offset=c(0.5,0.5))
    pInSp = overlay(pp,areas)
    # To many points, use old result
    if (sum(!is.na(pInSp)) > resol) break
    opInSp = pInSp
    opp = pp
    # Getting close to resol, waste of time to try once more
    if (sum(!is.na(pInSp)) > (resol/4+10)) break
  }
  print(paste(pow,sum(!is.na(pInSp)),resol,length(pInSp)))
  newpts = opp[!is.na(opInSp)]
  newpts
}




rtopFitVariogram = function(object, vario, ainfo,areas,dAreas,dAreasHyp, gDist, gDistHyp,
      model = "Ex1", geoDist = FALSE, pdfdist = FALSE,par0,paru, parl,stype="regular",resol = 5,
      fit.method = 3, debug.level = 1,params=list(),...) {
  dots = list(...)
  if (missing(object) & missing(ainfo) &
     (missing(areas) || !inherits(areas,"SpatialPolygonsDataFrame"))) stop("Missing data")
  if (inherits(object,"rtopVariogram") | inherits(object,"rtopVariogramCloud")) vario = object
  if (inherits(object,"rtop")) {
    if (missing(vario)) {
      if ("variogram" %in% names(object)) {
        vario = object$variogram
      } else if ("variogramCloud" %in% names(object)) {
        vario = object$variogramCloud
      } else {
        warning("Missing variogram, calling rtopVariogram")
#      vario = rtopVariogram(object,unlist(dots[names(dots) %in% names(formals(rtopVariogram))]))$variogram
        if (inherits(object,"rtop")) {
          object = rtopVariogram(object,...)
          if ("variogram" %in% names(object)) vario = object$variogram else vario = object$variogramCloud
        } else {
        warning("Missing variogram, calling rtopVariogram")
          vario = rtopVariogram(ainfo,...)
        }
      }
    }
    areas = object$areas
    ainfo = areas@data
    if (missing(dAreas) & "dAreas" %in% names(object)) dAreas = object$dAreas
    if (missing(gDist) & "gDist" %in% names(object)) gDist= object$gDist
    if (missing(dAreasHyp) & "dAreasHyp" %in% names(object)) dAreasHyp = object$dAreasHyp
    if (missing(gDistHyp) & "gDistHyp" %in% names(object)) gDistHyp= object$gDistHyp
  }
  if (inherits(object,"SpatialPolygonsDataFrame" & missing(vario))) {
    vario = rtopVariogram(object,...)
  }
  if (inherits(vario,"rtopVariogramCloud")) {
    if (missing(dAreas)) {
      dAreas = rtopDiscAreas(areas,...)
      if (!missing(object) & inherits(object,"rtop")) object$dAreas = dAreas
    }
    if (geoDist & missing(gDist)) {
        gDist = findGeoDist(dAreas,...)
        if (!missing(object) & inherits(object,"rtop")) object$gDist = gDist
    }
  } else {
    if (geoDist & missing(gDistHyp)) {
      if (missing(dAreasHyp)) {
        dAreasHyp = mapply(rtopDiscHypArea,as.list(vario$a1),
            as.list(vario$a2),as.list(vario$dist),MoreArgs = list(resol,stype),SIMPLIFY = FALSE)
        if (!missing(object) & inherits(object,"rtop")) object$dAreasHyp = dAreasHyp
      }
      gDistHyp = geoDistFind(dAreasHyp)
      if (!missing(object) & inherits(object,"rtop")) object$gDistHyp = gDistHyp
    }
  }

  if (geoDist) {
    if (!inherits(vario,"rtopVariogramCloud")) {
      bestPar = sceua(objfunc,par0,parl ,paru,varioIn = vario,
         gDistIn = gDistHyp, geoDist = TRUE,model = model,...)
      varFit = objfunc(bestPar,varioIn = vario,gDistIn = gDistHyp,
                     geoDist = TRUE,last = TRUE,model = model,...)
    } else {
      bestPar = sceua(objfunc,par0,parl ,paru,varioIn = vario,
         gDistIn = gDist, geoDist = TRUE,model = model,...)
      varFit = objfunc(bestPar,varioIn = vario,gDistIn = gDist,
                     geoDist = TRUE,last = TRUE,model = model,...)
    }
  } else {
    if (inherits(vario,"rtopVariogramCloud")) {
      bestPar = sceua(objfunc,par0,parl,paru,varioIn = vario,
         dAreas = dAreas,geoDist = FALSE,model = model,...)
      varFit = objfunc(bestPar,varioIn = vario,geoDist = FALSE,dAreas = dAreas,last = TRUE,model = model,...)
    } else {
      bestPar = sceua(objfunc,par0,parl,paru,varioIn = vario,geoDist = FALSE,model = model,...)
      varFit = objfunc(bestPar,varioIn = vario,geoDist = FALSE,last = TRUE,model = model,...)
    }
  }
  varModel = list(model = model,params = bestPar)
  if (inherits(object,"rtop")){
    object$varModel = varModel
    object$varFit = varFit
    return(object)
  } else {
    return(varModel)
  }
}

