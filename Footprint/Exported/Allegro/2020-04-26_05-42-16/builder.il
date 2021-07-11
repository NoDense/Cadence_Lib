(defstruct lb_footprint
  name units extents padstackMap pins vias layers readStatus step 
                     ; derived entities used for analysis 
)

(defstruct lb_units
 type precision
)

(defstruct lb_extents
 minX minY maxX maxY 
)

(defstruct lb_padstack
  name type players hole drillSymbol shapeMap readStatus
)

(defstruct lb_ps_layer
  name pad thermal antipad 
)

(defstruct lb_ps_shape
  name type width height 
)

(defstruct lb_hole
  plated drill
)

(defstruct lb_drill
  figure diameter width height 
)

(defstruct lb_drillSymbol
  figure width height drillChars 
)

(defstruct lb_pin
  number padName originX originY rotation isMechanical textBlk justify
)

(defstruct lb_via
  name connects 
)

(defstruct lb_via_connect
 x y 
)

(defstruct lb_shape
  type name paths circles arcs polygons layer width height isComposite
)

(defstruct lb_layer
  name packageHeight paths circles polygons labels
)

(defstruct lb_circle
 type lineWidth originX originY radius
)

(defstruct lb_arc
 lineWidth centerX centerY x1 y1 x2 y2 x3 y3 x4 y4 angle 
)

(defstruct lb_polygon
 points
)

(defstruct lb_path
 points lineWidth type
)

(defstruct lb_point
  x y
)

(defstruct lb_label
 textBlock angle x y text layer justify
)

(defstruct lb_step
 fileName rotationX
)

(defun LB_boxCreate (lx ly ux uy) (list (list lx ly) (list ux uy)))
(defun LB_boxCreateFromSizes (lx ly width height) (LB_boxCreate lx ly (lx + width) (ly + height)))

(defun LB_createFootprints ( inDir outDir )
(let ( xmlFiles )
 (rexMagic t)
 xmlFiles = (rexMatchList "\\.xml$" (getDirFiles inDir)) 
 (rexMagic nil)
 (changeWorkingDir outDir)
 (foreach x xmlFiles
  (LB_createFootprint (strcat inDir "/" x))
 )
))

(defun LB_createFootprint ( filePath )
(let ( lbFootprint footprintFileName )
  (axlMsgPut "Reading Footprint XML")
  lbFootprint = (LB_readFootprintFile filePath)
  (when (isValidFootprint lbFootprint)
    (axlMsgPut "Creating padstacks")
    (axlDBCloak '(LB_createPadstacks lbFootprint))
    footprintFileName = (strcat lbFootprint->name ".dra")
    (axlMsgPut "Opening Design %L" footprintFileName)
    (AFn_OpenDesign ?design footprintFileName, ?mode "wf", ?noMru t, ?noConfirm t, ?forceNew t)
    (axlSetSymbolType "package")
    (axlDBChangeDesignUnits lbFootprint->units->type lbFootprint->units->precision)
    (axlDBChangeDesignExtents (list (list lbFootprint->extents->minX lbFootprint->extents->minY) (list lbFootprint->extents->maxX lbFootprint->extents->maxY)))
    (axlMsgPut "Creating Shapes on Package layers")
    (axlDBCloak '(LB_createPackageLayers lbFootprint) )
    (axlMsgPut "Creating Pins")
    (axlDBCloak '(LB_createPins lbFootprint))
    (axlMsgPut "Creating Vias")
    (LB_createVias lbFootprint)
    (axlCompileSymbol)
    (axlMsgPut "Symbol Created")
    (axlWindowFit)
    (axlSaveDesign ?noMru t)
    (axlOpenDesign  ?design footprintFileName)
    (axlVisibleLayer "PACKAGE GEOMETRY/PIN_NUMBER" nil)
    (axlMsgPut "Symbol Saved")
  )
))

(defun isValidFootprint ( lbFootprint )
(let (  fpName fpNameLength  (allegroLongNameSize (axlGetVariable "allegro_long_name_size")) (valid nil))
  (when lbFootprint 
    fpName = lbFootprint->name
    fpNameLength = (strlen fpName)
    (if fpNameLength > 31 && (!allegroLongNameSize || (atoi allegroLongNameSize) < fpNameLength) then
      (let (errMsg )
        (sprintf errMsg "Footprint file name is too long. You need to set the preference 'allegro_long_name_size' value to %d to create this footprint. Would you like to set the value to %d to create footprint?" fpNameLength fpNameLength) 
        (if (axlUIYesNo errMsg) then
          (axlSetVariableFile "allegro_long_name_size" fpNameLength)
          (axlMsgPut "'allegro_long_name_size' preference is set to %d" fpNameLength)
          valid = t
	 else
          (axlUIConfirm "Failed to create footprint. File name is too long. Please fix 'allegro_long_name_size' in 'Setup - User Preferences - Drawing' before exporting footprint."))) 
     else 
      valid = t)
  )
  valid))

(defun AFn_OpenDesign ( @key design (mode "w") noMru noConfirm forceNew)
(let (noconfirmStatus, result)
  when(noConfirm, noconfirmStatus = axlGetVariable("NOCONFIRM"), axlSetVariable("NOCONFIRM", t))
  when(forceNew && isFile(design), deleteFile(design))
  result = if(noMru && axlVersion('version) >= 16.0
   then axlOpenDesign(?design design, ?mode mode, ?noMru t)
   else axlOpenDesign(?design design, ?mode mode)
  )
  when(noConfirm, if(noconfirmStatus then axlSetVariable("NOCONFIRM", t) else axlUnsetVariable("NOCONFIRM")))
  result
))
(defun LB_createPackageLayers ( lbFootprint )
(let ( path )
  (foreach l lbFootprint->layers
     (LB_createPathShape l->paths l->name)
     (LB_createPolygon l->polygons l->name) 
     (LB_createCircle l->circles l->name)
     (LB_createLabel l->labels)
  )
))

(defun LB_createLabel ( lbLabels )
  (foreach lbLabel lbLabels
    (axlDBCreateText lbLabel->text lbLabel->x:lbLabel->y
                       (make_axlTextOrientation ?textBlock lbLabel->textBlock
                                               ?rotation lbLabel->angle
                                               ?mirrored nil
                                               ?justify lbLabel->justify)
                               lbLabel->layer nil)
  )
)

(defun LB_createPolygon ( lbPolygons layerName )
(let ( lp (shapeId nil) shapes )
  (foreach lp lbPolygons
    shapeId = (LB_createShape lp->points layerName)
    (when shapeId
      shapes = (cons shapeId shapes))
  )
  shapes
))

(defun LB_createShape ( lbPoints layerName )
(let (p shapeId )
  (unless ((car lbPoints ) == (car (last lbPoints)))
    lbPoints = (append lbPoints (list (car lbPoints))) )
    p = (axlPathStart lbPoints)
    shapeId = (axlDBCreateOpenShape p t layerName)
    shapeId = (axlDBCreateCloseShape shapeId t)
  (car shapeId)
))

(defun LB_createCircle ( lbCircles layerName )
(let ( lc p id shapes)
  (foreach lc lbCircles
      (if lc->type == "shape" then
	p = (axlPathStartCircle (list lc->originX:lc->originY lc->radius) 0.0 )
        id = (axlDBCreateShape p t layerName)
       else 
        id = (axlDBCreateCircle  (list lc->originX:lc->originY lc->radius) lc->lineWidth layerName)
      )
     (when id 
       shapes = (cons (car id) shapes))
  )
  shapes 
))

(defun LB_createArc ( lbArcs layerName )
(let ( p id shapes larc x1 y1 x2 y2 x3 y3 x4 y4)
  (foreach larc lbArcs
    x1 = larc->x1
    y1 = larc->y1
    x2 = larc->x2
    y2 = larc->y2
    x3 = larc->x3
    y3 = larc->y3
    x4 = larc->x4
    y4 = larc->y4
    p = (axlPathStart (list x1:y1))
    p = (axlPathArcAngle p 0.0 x2:y2 nil larc->angle)
    p = (axlPathLine p 0.0 x3:y3)
    p = (axlPathArcAngle p 0.0 x4:y4 t larc->angle)
    p = (axlPathLine p 0.0 x1:y1)
    id = (axlDBCreateOpenShape p t layerName)
    id = (axlDBCreateCloseShape id)
    shapes = (cons (car id) shapes)
  )
  shapes ))


(defun LB_createPathShape ( lbPaths layerName )
(let ( path lbPoint path point  bBox boxList )
  (foreach lbPath lbPaths
     lbPoint = (car lbPath->points)
     (if lbPath->type == "shape" then
        (LB_createShape lbPath->points layerName)
      else
  	 (axlDBCreateLine lbPath->points lbPath->lineWidth layerName 'line)
     ) 
  )
))

(defun LB_createPins ( footprint )
  (foreach p footprint->pins
    (LB_createPin p->padName (list p->originX p->originY) p->number ?angle p->rotation ?txtBlk p->textBlk ?isMechanical p->isMechanical ?justify p->justify)
  )
)

(defun LB_createVias ( lbFootprint )
;; create vias
  (axlCNSCreate 'physical "DEFAULT" nil)
  grid = (axlDBGridGet "TOP")
  grid->name = 'etch
  grid->xGrids = 0.1
  grid->yGrids = 0.1
  (axlDBGridSet grid)
  (foreach lbVia lbFootprint->vias
    (LB_createVia lbVia)
  )
)

(defun LB_createVia (lbVia)
(let ( (viaPadname lbVia->name) lbConnect )
  (axlCnsAddVia "DEFAULT" lbVia->name)
  (foreach lbConnect lbVia->connects
     (axlDBCreateVia viaPadname lbConnect->x:lbConnect->y nil nil 0 nil)
  )
))

(defun LB_createPin (padStackId loc pinNumber @key (txtBlk "1") (angle 0) (isMechanical nil) (justify "center"))
(let (txtO txtOffset (txtid nil) result)
 (unless isMechanical
   txtO = make_axlTextOrientation(?textBlock txtBlk, ?justify justify)
   txtid = make_axlPinText(?number pinNumber ?offset 0.0:0.0 ?text txtO))
 result = axlDBCreatePin(padStackId, loc, txtid, angle)
;; (axlMsgPut "created pin %L %L loc %L txtid %L angle %L result %L isMechanical %L justify %L" pinNumber padStackId loc txtid angle result isMechanical justify)
))

(defun LB_createPadstacks (footprint)
(let ( padstack psId padList fileName drillData drillSymbolData figureType lbDrill lbDrillSymbol holeType plateType result a b c d)
  (foreach name footprint->padstackMap
    padstack =  footprint->padstackMap[name]
    (when padstack
        (LB_createShapes padstack footprint->units)
        fileName = (strcat padstack->name ".pad")
      	AFn_OpenDesign(?design padstack->name, ?mode "wf", ?noMru t, ?noConfirm t, ?forceNew t)
        (axlDBChangeDesignUnits footprint->units->type footprint->units->precision)
        drillData = nil
        (when padstack->hole &&  padstack->hole->drill
          (if padstack->hole->plated then
              plateType = 'PLATED
           else  plateType = 'NON_PLATED)
          lbDrill =  padstack->hole->drill
          
          figureType = (LB_getFigureType  lbDrill->figure)
          holeType =  "Circle Drill"
          (cond 
             (figureType == 'RECTANGLE   holeType = "Rectangle Slot")
             (figureType == 'OBLONG   holeType = "Oval Slot")
          )
          drillData = (make_axlPadStackDrill ?drillDiameter lbDrill->diameter
                             ?figure figureType ?figureSize lbDrill->width:lbDrill->height ?plating plateType ?holeType holeType) )
       
   	padList = (LB_createPads padstack)
    	psId = (axlDBCreatePadStack padstack->name drillData padList t)
        (when padstack->drillSymbol
          lbDrillSymbol =  padstack->drillSymbol
          figureType = (LB_getFigureType  lbDrillSymbol->figure)
          drillSymbolData = (list (list 'drillFigureName figureType) (list 'drillFigureHeight lbDrillSymbol->height) (list 'drillFigureWidth lbDrillSymbol->width) (list 'drillChar lbDrillSymbol->drillChars))
          result = (axlPadstackEdit psId drillSymbolData) 
        )
    	result = (axlPadstackToDisk padstack->name fileName)
    )
  )      
))

(defun LB_createShapes ( padstack units )
(let ( lbShape shapeName x y shapeFile pshapes shapes arcShapes pid symName shapeType)
  (foreach shapeName  padstack->shapeMap
    lbShape =  padstack->shapeMap[shapeName]
    x = lbShape->width / 2
    y = lbShape->height / 2
    shapeFile = (strcat shapeName ".dra") 
    AFn_OpenDesign(?design shapeFile, ?mode "wf", ?noMru t, ?noConfirm t, ?forceNew t)
    (if lbShape->type && lbShape->type == "Thermal" then
       shapeType = "flash"
     else  shapeType = "shape")
    (axlSetSymbolType shapeType)
    (axlDBChangeDesignUnits units->type units->precision)
    (axlDBChangeDesignExtents (list (-x:-y) (x:y)) )
    pshapes = (LB_createPolygon lbShape->polygons "ETCH/TOP") 
    arcShapes = (LB_createArc lbShape->arcs  "ETCH/TOP")
    shapes = (LB_createCircle lbShape->circles  "ETCH/TOP")
    (when lbShape->isComposite
      (when pshapes
        pid = (car pshapes) 
        pshapes = (cdr pshapes))
      (unless pid
        (when shapes
          pid = (car shapes)
          shapes = (cdr shapes)))
      (unless pid
        (when arcShapes
          pid = (car arcShapes)
          arcShapes = (cdr arcShapes)))
      (foreach s pshapes
        shapes = (cons s shapes))
      (foreach s arcShapes
        shapes = (cons s shapes))
      (when pid && shapes
      (axlShapeMerge pid shapes nil)) )

    symName = (axlCompileSymbol)
    (axlSaveDesign ?noMru t) 
  )
))

(defun LB_boxWidth (box) ((CS_ux box) - (CS_lx box)))
(defun LB_boxHeight (box) ((CS_uy box) - (CS_ly box)))
(defun CS_lx (box) (car (lowerLeft box)))
(defun CS_ly (box) (cadr (lowerLeft box)))
(defun CS_ux (box) (car (upperRight box)))
(defun CS_uy (box) (cadr (upperRight box)))


(defun LB_createPads ( padstack )
(let ( padList pad figureType width height lbPadShape (shapeName nil) layer )
  (foreach layer padstack->players
    lbPadShape = nil 
    shapeName = nil
    (when layer->pad 
        (if layer->pad->name then
           shapeName = layer->pad->name
           lbPadShape = padstack->shapeMap[shapeName]
         else lbPadShape = layer->pad)
        pad = (LB_createPad lbPadShape shapeName layer->name "REGULAR")
        (when pad
          padList = (cons pad padList)))    
    (when layer->thermal
        (if layer->thermal->name then
           shapeName = layer->thermal->name
           lbPadShape = padstack->shapeMap[shapeName]
         else lbPadShape = layer->thermal)
        pad = (LB_createPad lbPadShape shapeName layer->name "THERMAL")
        (when pad
       	  padList = (cons pad padList)) )    

    (when layer->antipad
        pad = (LB_createPad layer->antipad nil layer->name "ANTIPAD")
        (when pad
       	  padList = (cons pad padList))     
    )
  )
  (when padList padList = (reverse padList))
  padList
))

(defun LB_createPad (  lbPadShape shapeName layerName padType)
(let ( figureType pad width height )
 
 (if shapeName then 
    figureType = shapeName
  else
    figureType = (LB_getFigureType lbPadShape->type) 
 )
 (when figureType
   (if padType == "THERMAL" then
    pad = (make_axlPadStackPad ?layer layerName ?type padType 
		?flash shapeName ?figure 'FLASH ?figureSize lbPadShape->width:lbPadShape->height)
    else
    pad = (make_axlPadStackPad ?layer layerName ?type padType 
			?figure figureType ?figureSize lbPadShape->width:lbPadShape->height))
 )
 pad
))

(defun LB_getFigureType ( shapeType )
(let ( figureType )
  shapeType = (lowerCase shapeType)
  (cond
    ((shapeType == "rectangle")  figureType = 'RECTANGLE)
    ((shapeType == "square")  figureType = 'SQUARE)
    ((shapeType == "circle")  figureType = 'CIRCLE)
    ((shapeType == "oblong")  figureType = 'OBLONG)
    ((shapeType == "Oval")  figureType = 'OBLONG)
    (t   (axlMsgPut "Unknown pad shape type %L" shapeType))
  )
figureType
))

(defun LB_readFootprintFile (filePath)
  (let (iFile firstToken nextLine lineTokens footprint (keepGoing t) (readStatus t))
    (cond
      (filePath == nil (axlMsgPut "Footprint file path is not given."))
      (iFile = infile( filePath)
        (while keepGoing && (gets nextLine iFile)
          (cond
            (nextLine
                      lineTokens = (parseString nextLine)
                      firstToken = (nth 0 lineTokens)
                      (case firstToken
                         (("<Footprint")
                             footprint = make_lb_footprint() 
 			     (foreach token (cdr lineTokens)
                               (cond
                                 ((nindex token "name=")
                                       footprint->name = (LB_readStr "name=\"%s" token))
                               ))
                             (LB_readFootprint iFile footprint ) 
                             keepGoing = nil
                          )
                         (("!<" "<!--" "<!--Copyright")                          t)
                         ("<?xml"                                                t)
                         (t (axlMsgPut "Unknown xml content in %L" filePath))
                       ))
             (t       (axlMsgPut "Found no data in footprint file %L" filePath) )
           )                 ; end cond
         )
         close( iFile)
         (unless footprint 
           (axlMsgPut "Failed to read footprint data in %L" filePath))
      )
      (iFile == nil
             (axlMsgPut "Footprint file %L does not exist" filePath))
     )                                 ; end cond
    footprint
  )                                   ; end let
)                                     ; end defun
        

(defun LB_readFootprint (iFile footprint)
  (let ( nextLine lineTokens firsToken (keepGoing t) padstack)
    footprint->readStatus = t
    footprint->padstackMap = (makeTable "PadstackMap" nil)
    (while footprint->readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (nil t)
             (("<Padstack")    padstack = make_lb_padstack() 
 			       (foreach token (cdr lineTokens)
                                 (cond
                                 ((nindex token "name=")
                                       padstack->name = (LB_readStr "name=\"%s" token))
				 ((nindex token "type=")
                                       padstack->type = (LB_readStr "type=\"%s" token))
                                 ))
                                (LB_readPadstack iFile padstack ) 
                                footprint->padstackMap[padstack->name] = padstack )
             (("<Units")        footprint->units = (LB_readUnits (cdr lineTokens)))  
             (("<Extents")      footprint->extents = (LB_readExtents  (cdr lineTokens))) 
             (("<Pins>")        footprint->pins = (LB_readPins iFile)) 
             (("<Vias>")        footprint->vias = (LB_readVias iFile)) 
             (("<Layers>")      
				footprint->layers = (LB_readLayers iFile)
                                ) 
             (("<Step")         footprint->step = make_lb_step() 
				(foreach token (cdr lineTokens)
                                 (cond
                                 ((nindex token "file=")
                                       footprint->step->fileName = (LB_readStr "file=\"%s" token))
				 ((nindex token "rotation_x=")
                                       footprint->step->rotationX = (LB_readStr "rotation_x=\"%s" token))
                                ))) 
	     (("!<" "<!--")     t)
             (("</Footprint>")   footprint->readStatus = nil)
             (t                 (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    footprint
))

(defun LB_readUnits ( lineTokens )
(let (token units)
   units =  make_lb_units() 
    (foreach token lineTokens
      (cond
        ((nindex token "type=") units->type = (LB_readStr "type=\"%s" token))
	((nindex token "precision=") units->precision = (LB_readStr "precision=\"%d" token))
    ))
   units
))

(defun LB_readExtents ( lineTokens )
(let (token extents width height)
   extents =  make_lb_extents() 
   (foreach token lineTokens
      (cond
        ((nindex token "minX=") extents->minX = (LB_readStr "minX=\"%f" token))
	((nindex token "minY=") extents->minY = (LB_readStr "minY=\"%f" token))
	((nindex token "width=") width = (LB_readStr "width=\"%f" token))
	((nindex token "height=") height = (LB_readStr "height=\"%f" token))
   ))
   extents->maxX =  extents->minX + width
   extents->maxY =  extents->minY + height
   extents
))

(defun LB_readPadstack (iFile padstack)
(let (view nextLine lineTokens firsToken (keepGoing t) layer pin lbHole str lbDrillSymbol)
    padstack->readStatus = t
    (while padstack->readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (nil t)
             (("<Hole")        lbHole = make_lb_hole() 
 			       (foreach token (cdr lineTokens)
                                 (cond
                                   ((nindex token "plating=")
                                                              lbHole->plated = ((LB_readStr "plating=\"%s" token) == "plated") )
                                 )
                               )
                               lbHole->drill = (LB_readDrill iFile) 
                               padstack->hole = lbHole)
              (("<Symbol")     lbDrillSymbol = make_lb_drillSymbol() 
 			       (foreach token (cdr lineTokens)
                                 (cond
                                   ((nindex token "drillChars=") lbDrillSymbol->drillChars = (LB_readStr "drillChars=\"%s" token) )
                                   ((nindex token "figure=") lbDrillSymbol->figure = (LB_readStr "figure=\"%s" token) )
                                   ((nindex token "width=") lbDrillSymbol->width = (LB_readStr "width=\"%f" token) )
                                   ((nindex token "height=") lbDrillSymbol->height = (LB_readStr "height=\"%f" token) )
                                 )
                               )
                               padstack->drillSymbol = lbDrillSymbol)
             (("<Shapes>")     padstack->shapeMap = (LB_readShapes iFile) )
             (("<Layers>")     padstack->players = (LB_readPadstackLayers iFile) )
             (("</Padstack>")  padstack->readStatus = nil)
	     (("!<" "<!--")             t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    padstack
))

(defun LB_readDrill ( iFile )
(let (nextLine lineTokens firsToken (keepGoing t) readStatus lbDrill)
  readStatus = t
  (while readStatus && (gets nextLine iFile) 
     lineTokens = (parseString nextLine)
     firstToken = (nth 0 lineTokens)
     (case firstToken
       (("<Drill")    lbDrill = make_lb_drill( ) 
 		      (foreach token (cdr lineTokens)
                        (cond
                          ((nindex token "figure=") lbDrill->figure = (LB_readStr "figure=\"%s" token) )
                          ((nindex token "diameter=") lbDrill->diameter = (LB_readStr "diameter=\"%f" token) )
                          ((nindex token "width=") lbDrill->width = (LB_readStr "width=\"%f" token) )
                          ((nindex token "height=") lbDrill->height = (LB_readStr "height=\"%f" token) )
                        )
                      )) 
       (("</Hole>")    readStatus = nil )
       (("!<" "<!--")                                                      t)
       (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
     )
   )
   lbDrill 
))

(defun LB_readShapes (iFile)
(let (nextLine lineTokens firsToken (keepGoing t) lbShape (lbShapeMap  (makeTable "ShapeMap" nil)) readStatus)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Shape")       lbShape = make_lb_shape() 
 			       (foreach token (cdr lineTokens)
                                 (cond
                                   ((nindex token "type=")
                                       lbShape->type = (LB_readStr "type=\"%s" token)) 
                                   ((nindex token "name=")
                                       lbShape->name = (LB_readStr "name=\"%s" token))
                                 ((nindex token "layer=")
                                       lbShape->layer = (LB_readStr "layer=\"%s" token))
                                 ((nindex token "width=")
                                       lbShape->width = (LB_readStr "width=\"%f" token))
                                 ((nindex token "height=")
                                       lbShape->height = (LB_readStr "height=\"%f" token))
                                 ))
                               (LB_readShape iFile lbShape)     
			       lbShapeMap[lbShape->name] = lbShape ) 
	     (("</Shapes>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    lbShapeMap 
)) 

(defun LB_readPadstackLayers (iFile)
(let (nextLine lineTokens firsToken (keepGoing t) psLayer layers readStatus )
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Layer")	psLayer = make_lb_ps_layer() 
 			       (foreach token (cdr lineTokens)
                                 (cond
                                 ((nindex token "name=")
                                       psLayer->name = (LB_readStr "name=\"%s" token))
                                 ((nindex token "shapeName=")
                                       psLayer->shapeName = (LB_readStr "shapeName=\"%s" token))
                                 ))
                                (LB_readPadstackLayer iFile psLayer )     
			        layers = (cons psLayer layers) )
             (("</Layers>")    readStatus = nil)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when layers  layers = (reverse layers))
    layers
))

(defun LB_readPadstackLayer (iFile pLayer)
(let (nextLine lineTokens firsToken (keepGoing t) shape readStatus )
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Pad")	     pLayer->pad = (LB_readPadstackLayerShape lineTokens) )
	     (("<Thermal")   pLayer->thermal = (LB_readPadstackLayerShape lineTokens) )
	     (("<AntiPad")   pLayer->antipad = (LB_readPadstackLayerShape lineTokens) )
             (("</Layer>")   readStatus = nil)
             (t              (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
))

(defun LB_readPadstackLayerShape (lineTokens)
(let ( shape )
  shape = make_lb_ps_shape() 
  (foreach token (cdr lineTokens)
    (cond
      ((nindex token "shapeName=")   shape->name = (LB_readStr "shapeName=\"%s" token))
      ((nindex token "shape=")   shape->type = (LB_readStr "shape=\"%s" token))
      ((nindex token "width=")  shape->width = (LB_readStr "width=\"%f" token))
      ((nindex token "height=") shape->height = (LB_readStr "height=\"%f" token))
   ))
  shape
))

(defun LB_readPins (iFile)
(let (nextLine lineTokens firsToken (keepGoing t) layer pin pins readStatus)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (nil t)
             (("<Pin")         pin = make_lb_pin( ?justify "center")
 			       (foreach token (cdr lineTokens)
    				  (cond
                                    ((nindex token "justify=")  pin->justify = (LB_readStr "justify=\"%s" token))
      				    ((nindex token "number=")   pin->number = (LB_readStr "number=\"%s" token))
      				    ((nindex token "padName=")  pin->padName = (LB_readStr "padName=\"%s" token))
      				    ((nindex token "originX=")  pin->originX = (LB_readStr "originX=\"%f" token))
      				    ((nindex token "originY=")  pin->originY = (LB_readStr "originY=\"%f" token))
      				    ((nindex token "rotation=")  pin->rotation = (LB_readStr "rotation=\"%f" token))
      				    ((nindex token "isMechanical=")  pin->isMechanical = ((LB_readStr "isMechanical=\"%s" token) == "yes"))
                                    ((nindex token "textBlk=")  pin->textBlk = (LB_readStr "textBlk=\"%s" token))
   			       ))
                               pins = (cons pin pins) )
             (("</Pins>")      readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when pins  pins = (reverse pins))
    pins
))

(defun LB_readLayers (iFile)
(let (nextLine lineTokens firsToken (keepGoing t) layer layers readStatus)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Layer")       layer = make_lb_layer()
 			       (foreach token (cdr lineTokens)
    				  (cond
      				    ((nindex token "name=")   layer->name = (LB_readStrWithSpaces nextLine "name="))
      				    ((nindex token "packageHeight=")  layer->packageHeight = (LB_readStr "packageHeight=\"%f" token))
   			       ))
                               (LB_readLayer iFile layer)
                               layers = (cons layer layers) 
			       )
	     (("</Layers>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when layers
      layers = (reverse layers))
    layers
))

(defun LB_readShape (iFile lbShape)
(let (nextLine lineTokens firsToken (keepGoing t) label lbPath lbCircle lbArc lbPolygon readStatus)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<SimpleShape>")     lbShape->isComposite = nil )
             (("<CompositeShape>")  lbShape->isComposite = t )
             (("<Path")       lbPath = make_lb_path()
                              (foreach token (cdr lineTokens)
    				  (cond
      				    ((nindex token "lineWidth=")   lbPath->lineWidth = (LB_readStr "lineWidth=\"%f" token))
                                    ((nindex token "type=")   lbPath->type = (LB_readStr "type=\"%s" token))
      				  )
			      )
                              (LB_readPath iFile lbPath)
                              lbShape->paths = (cons lbPath lbShape->paths) 
			      )
               (("<Polygon>") 
                              lbPolygon = make_lb_polygon()
                              (LB_readPolygon iFile lbPolygon)
                              lbShape->polygons = (cons lbPolygon lbShape->polygons) 
			      )
              (("<Circle")    lbCircle = make_lb_circle()
                              (foreach token (cdr lineTokens)
    				  (cond
                                    ((nindex token "type=")   lbCircle->type = (LB_readStr "type=\"%s" token))
      				    ((nindex token "lineWidth=")   lbCircle->lineWidth = (LB_readStr "lineWidth=\"%f" token))
      				    ((nindex token "originX=")   lbCircle->originX = (LB_readStr "originX=\"%f" token))
      				    ((nindex token "originY=")   lbCircle->originY = (LB_readStr "originY=\"%f" token))
      				    ((nindex token "radius=")   lbCircle->radius = (LB_readStr "radius=\"%f" token))
      				  )
			      )
                              lbShape->circles = (cons lbCircle lbShape->circles) 
			      )
               (("<Arc")    lbArc = make_lb_arc()
                              (foreach token (cdr lineTokens)
    				  (cond
      				    ((nindex token "lineWidth=")   lbArc->lineWidth = (LB_readStr "lineWidth=\"%f" token))
      				    ((nindex token "centerX=")   lbArc->centerX = (LB_readStr "centerX=\"%f" token))
      				    ((nindex token "centerY=")   lbArc->centerY = (LB_readStr "centerY=\"%f" token))
      				    ((nindex token "x1=")   lbArc->x1 = (LB_readStr "x1=\"%f" token))
      				    ((nindex token "y1=")   lbArc->y1 = (LB_readStr "y1=\"%f" token))
      				    ((nindex token "x2=")   lbArc->x2 = (LB_readStr "x2=\"%f" token))
      				    ((nindex token "y2=")   lbArc->y2 = (LB_readStr "y2=\"%f" token))
                                    ((nindex token "x3=")   lbArc->x3 = (LB_readStr "x3=\"%f" token))
      				    ((nindex token "y3=")   lbArc->y3 = (LB_readStr "y3=\"%f" token))
                                    ((nindex token "x4=")   lbArc->x4 = (LB_readStr "x4=\"%f" token))
      				    ((nindex token "y4=")   lbArc->y4 = (LB_readStr "y4=\"%f" token))
      				    ((nindex token "angle=")   lbArc->angle = (LB_readStr "angle=\"%f" token))
      				  )
			      )
                              lbShape->arcs = (cons lbArc lbShape->arcs) 
			      )
	     
	     (("</SimpleShape>")   t ) 
	     (("</CompositeShape>")   t ) 
	     (("</Shape>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when  lbShape->paths 
        lbShape->paths = (reverse  lbShape->paths ))
    
))

(defun LB_readLayer (iFile layer)
(let (nextLine lineTokens firsToken (keepGoing t) label lbPath lbCircle lbPolygon readStatus)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Path")       lbPath = make_lb_path()
                              (foreach token (cdr lineTokens)
    				  (cond
      				    ((nindex token "lineWidth=")   lbPath->lineWidth = (LB_readStr "lineWidth=\"%f" token))
                                    ((nindex token "type=")   lbPath->type = (LB_readStr "type=\"%s" token))
      				  )
			      )
                              (LB_readPath iFile lbPath)
                              layer->paths = (cons lbPath layer->paths) 
			      )
               (("<Polygon>") 
                              lbPolygon = make_lb_polygon()
                              (LB_readPolygon iFile lbPolygon)
                              layer->polygons = (cons lbPolygon layer->polygons) 
			      )
              (("<Circle")    lbCircle = make_lb_circle()
                              (foreach token (cdr lineTokens)
    				  (cond
                                    ((nindex token "type=")   lbCircle->type = (LB_readStr "type=\"%s" token))
      				    ((nindex token "lineWidth=")   lbCircle->lineWidth = (LB_readStr "lineWidth=\"%f" token))
      				    ((nindex token "originX=")   lbCircle->originX = (LB_readStr "originX=\"%f" token))
      				    ((nindex token "originY=")   lbCircle->originY = (LB_readStr "originY=\"%f" token))
      				    ((nindex token "radius=")   lbCircle->radius = (LB_readStr "radius=\"%f" token))
      				  )
			      )
                              layer->circles = (cons lbCircle layer->circles) 
			      )
	      (("<Label")     label = make_lb_label( ?justify "center")
 			      (foreach token (cdr lineTokens)
                                  (cond
                                    ((nindex token "justify=") label->justify = (LB_readStr "justify=\"%s" token))
      				    ((nindex token "layer=")  label->layer = (LB_readStrWithSpaces nextLine "layer="))
      				    ((nindex token "textBlock=")  label->textBlock = (LB_readStr "textBlock=\"%d" token))
      				    ((nindex token "angle=")  label->angle = (LB_readStr "angle=\"%d" token))
      				    ((nindex token "x=")  label->x = (LB_readStr "x=\"%f" token))
      				    ((nindex token "y=")  label->y = (LB_readStr "y=\"%f" token))
      				    ((nindex token "text=")  label->text = (LB_readStr "text=\"%s" token))
   			       ))
                               layer->labels = (cons label layer->labels) 
			       )
	     (("</Layer>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when  layer->paths 
        layer->paths = (reverse  layer->paths ))
    (when  layer->labels 
        layer->labels = (reverse  layer->labels ))
))

(defun LB_readPath (iFile lbPath )
(let (nextLine lineTokens firsToken (keepGoing t) readStatus path)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Point")      point = (LB_readPoint (cdr lineTokens))
                              (when point  
			      	lbPath->points = (cons point lbPath->points)) )
	     (("</Path>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when lbPath->points  
      lbPath->points = (reverse lbPath->points))
))

(defun LB_readPoint ( lineTokens )
(let ( x y token )
  (foreach token lineTokens
    (cond
       ((nindex token "x=")  x = (LB_readStr "x=\"%f" token))
       ((nindex token "y=")  y = (LB_readStr "y=\"%f" token))
    )
  )
  ( x:y )
))

(defun LB_readPolygon (iFile lbPolygon )
(let (nextLine lineTokens firsToken (keepGoing t) readStatus path point)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Point")
                              point = (LB_readPoint (cdr lineTokens))
                              (when point 
			        lbPolygon->points = (cons point lbPolygon->points)) )
	     (("</Polygon>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when lbPolygon->points  
      lbPolygon->points = (reverse lbPolygon->points))
))

(defun LB_readVias (iFile)
(let (nextLine lineTokens firsToken (keepGoing t) lbVia vias readStatus)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Via")       lbVia = make_lb_via()
                             (foreach token (cdr lineTokens)
    				  (cond
      				    ((nindex token "name=")   lbVia->name = (LB_readStr "name=\"%s" token))
      				  )
			     )
                             (LB_readViaConnects iFile lbVia)
			     vias = (cons lbVia vias))
	     (("</Vias>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    vias 
)) 

(defun LB_readViaConnects (iFile lbVia )
(let (nextLine lineTokens firsToken (keepGoing t) readStatus lbConnect )
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Connects>")
                              lbVia->connects = (LB_readConnects iFile))
	     (("</Via>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
))

(defun LB_readConnects (iFile )
(let (nextLine lineTokens firsToken (keepGoing t) readStatus lbConnect connects)
    readStatus = t
    (while readStatus && (gets nextLine iFile) 
           lineTokens = (parseString nextLine)
           firstToken = (nth 0 lineTokens)
           (case firstToken
             (("<Connect")
                             lbConnect = make_lb_via_connect()
                             (foreach token (cdr lineTokens)
    			  	(cond
      				    ((nindex token "x=")   lbConnect->x = (LB_readStr "x=\"%f" token))
      				    ((nindex token "y=")   lbConnect->y = (LB_readStr "y=\"%f" token))
      				)
			     )
                             connects = (cons lbConnect connects)
                             )
             (("</Connects>")    readStatus = nil )
	     (("!<" "<!--")                                                      t)
             (t                (axlMsgPut "Unknown token %L in %L" firstToken nextLine))
           )
    )
    (when connects
      (reverse connects)) 
    connects
))

(defun LB_getBBox (boxList)
  (cond (!boxList nil)
        (!(cdr boxList) (car boxList))
        (t (let ((lx (CS_min (foreach mapcar x boxList (CS_lx x)))) (ly (CS_min (foreach mapcar x boxList (CS_ly x))))
                 (ux (CS_max (foreach mapcar x boxList (CS_ux x)))) (uy (CS_max (foreach mapcar x boxList (CS_uy x)))))
             (list (list lx ly) (list ux uy))))))

LB_GLOBAL_rexMagicVals = nil
(defun LB_rexMagicSet (flag) (LB_rexMagicSave) (rexMagic flag))
(defun LB_rexMagicSave () LB_GLOBAL_rexMagicVals = (cons (rexMagic) LB_GLOBAL_rexMagicVals))
(defun LB_rexMagicRestore ()
  (when LB_GLOBAL_rexMagicVals
    (rexMagic (car LB_GLOBAL_rexMagicVals))
    LB_GLOBAL_rexMagicVals = (cdr LB_GLOBAL_rexMagicVals)))


(defmacro LB_sscanf (@rest args)
  `(letseq ((port (instring ,(car args))) (result (fscanf port ,@(cdr args))))
    (close port)
    result))

(defun LB_readStr (formatStr sourceStr)
  (let (buffer)
    (LB_sscanf sourceStr formatStr buffer)
    (LB_cleanString buffer)))

(defun LB_cleanString (theString)
  (when (LB_isNonEmptyString theString)
    (LB_rexMagicSet t)
    (rexCompile "/>$")
    theString = (rexReplace theString "" 0)
    (rexCompile ">$")
    theString = (rexReplace theString "" 0)
    (rexMagic nil) ;; already saved old val
    (rexCompile "\"")
    theString = (rexReplace theString "" 0)
    (LB_rexMagicRestore)
    theString = (LB_decodeXML theString))
  theString)
LB_XML_escapeMap = '(("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("'" "&apos;") ("\"" "&quot;"))
LB_XML_decodeMap = (reverse LB_XML_escapeMap) ;; replace ampersand last to avoid any double decoding
(defun LB_fix4XML (word)
  (when (LB_isNonEmptyString word)
    (LB_rexMagicSet nil)
    (foreach x LB_XML_escapeMap
      (rexCompile (car x))
      word = (rexReplace word (cadr x) 0))
    (LB_rexMagicRestore))
  word)

(defun LB_decodeXML (word)
  (when (LB_isNonEmptyString word)
    (LB_rexMagicSet nil)
    (foreach x LB_XML_decodeMap
      (rexCompile (cadr x))
      word = (rexReplace word (car x) 0))
    (LB_rexMagicRestore))
  word)

(defun LB_fixSpaces (word)
  (when (word && word != "")
    (LB_rexMagicSet nil)
    (rexCompile " ")
    word = (rexReplace word "_sp_" 0) ; should not be encoded. Need to fix its bad effects
    (LB_rexMagicRestore))
  word)

(defun LB_encodeSpaces (string)
  (cond ((stringp string)
         (if (string == "") string
             (let ()
               (LB_rexMagicSet nil) (rexCompile " ")
               string = (rexReplace string "&space;" 0)
               (LB_rexMagicRestore)
               string)))
        ((string && (listp string)) (foreach mapcar x string (LB_encodeSpaces x)))
        (t string)))

(defun LB_decodeSpaces (string)
  (cond ((stringp string)
         (if (string == "") string
             (let ()
               (LB_rexMagicSet nil) (rexCompile "&space;")
               string = (rexReplace string " " 0)
               (LB_rexMagicRestore)
               string)))
        ((string && (listp string)) (foreach mapcar x string (LB_decodeSpaces x)))
        (t string)))

(defun LB_intString (intNum) (sprintf nil "%d" intNum))

(defun LB_readStrWithSpaces (lineStr @optional (prefix "") (delim "\""))
  prefix = (strcat prefix delim)
  (let ((str (substring lineStr ((nindex lineStr prefix) + (strlen prefix)))))
    (LB_decodeXML (substring str 1 ((nindex str delim) - (strlen delim))))))

(defun LB_isNonEmptyString (string @key validName)
  (string && (stringp string) && (string != "") && (!validName || (string != "FALSE"))))

(defun LB_attachStepFile ( lbStep )
(let ( p pvalue )
  p = (axlDBGetDesign)
  pvalue = (strcat lbStep->fileName ", 36135, 1447109240, 0, 0.000000, 0.000000, 0.000000, 86484")
  (axlDBAddProp p (list "PKGDEF_STEP_FILE" pvalue))
  pvalue = (strcat "MAP=STEP_PRIMARY|TRANSPARENT=None|VIEW=Front Left|PKG=" (car (parseString lbStep->fileName "."))  "|INCREMENT=1.000000|")
  (axlDBAddProp p (list "STEP_MAPPING_UI_OPTIONS" pvalue))
  pvalue = (strcat "MM,0.000000,0.000000,0.000000," lbStep->rotationX ",0.000,0.000")
  (axlDBAddProp p (list "PKGDEF_STEP_TRANSFORMATION" pvalue))
))

(defun LB_compressAndEncryptFile (srcFile targFile)
  (letseq ((dir (LB_getParentDir targFile)) (cmpFile (makeTempFileName (strcat dir "/cmpskill"))))
    (axlMsgPut "Compressing %L -> %L" srcFile cmpFile)
    (compress srcFile cmpFile)
    (axlMsgPut "Encrypting %L -> %L" cmpFile targFile)
    (encrypt cmpFile targFile)
    (deleteFile cmpFile)))

(defun LB_getParentDir (fileOrDir) (car (axlDMFileParts fileOrDir)))



axlCmdRegister( "ema_fix_text" 'ema_fixText )

(defun ema_fixText (  )
(let ( pins )
 (axlSetFindFilter ?enabled (list "noall" "pins") ?onButtons (list "noall" "pins"))
 (axlAddSelectAll)
 pins = (axlGetSelSet)
 (axlClearSelSet)
 (foreach p pins  
   (when p->rotation == 180 || p->rotation == 270
     (foreach x  (axlDBGetAttachedText p)
       (axlTransformObject x ?angle -180 ?origin x->xy) ) ) )
))

axlCmdRegister( "cdpxTrigger" 'ema_cdpxTrigger )

(defun ema_cdpxTrigger ( edaBuilderPath )
(let ( (exitAllegro t) )
 (when (axlUIYesNo "Would you like to continue creating parts in Allegro?")
   (if (isFile edaBuilderPath) then
     (when (EMA_Builder_run (strcat edaBuilderPath " -fb"))
       (axlMsgPut "Launched %s successfully" edaBuilderPath)
       exitAllegro = nil) 
    else (axlUIConfirmEx  "EDA Builder executable file does not exist %s. Exiting Allegro..." edaBuilderPath) ) )
 (when exitAllegro  (axlShell "exit"))
))

(defun EMA_Builder_buildFootprint ( filePath )
  (axlMsgPut "Building the footprint %L" filePath)
  (axlShell (strcat "replay " filePath ))
)

(defun EMA_Builder_run ( cmd )
 (ipcBeginProcess cmd "" 'EMA_Builder_outHandler 'EMA_Builder_errHandler 'EMA_Builder_postFunc) 
)

(defun EMA_Builder_outHandler (childId data)
  (EMA_debugMsg "Received from EDA Builder %L" data)
  (when (nindex data "EMA_FPG_")
     (EMA_Builder_buildFootprint (substring data 9)) )
)

(defun EMA_Builder_errHandler (childId data)
  (axlMsgPut "Received an error from EDA Builder %L" data)
)

(defun EMA_Builder_postFunc (childId exitStatus)
  (axlMsgPut "EDA Builder is exited.")
)
