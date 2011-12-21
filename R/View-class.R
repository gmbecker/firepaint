fView = setRefClass("fireView",
            fields = list(
              .layer = "fireLayer",
              layer = function(value) {
                if( !missing( value ) )
                  {
                    .layer <<- value
                    #this will destroy all existing contexts every time a new root layer is assigned to the View (which I think should not be happening very often)
                    if(length(.self$viewLayers))
                      .self$CleanupViewLayers(.self$viewLayers[[1]])
                    .self$SetupViewLayers()
                  }
                else
                  .layer
              },

              .viewLayers = "list",
              viewLayers = function(value)
              {
                if(!missing(value))
                  .viewLayers <<- value
                else
                  .viewLayers
              },
              
              .transform = "matrix",
              transform = function(value) {
                if ( !missing(value))
                  {
                    .transform <<- value
                    .self$SetContextTransforms()
                  } else {
                    .transform
                  }
              },
              .containingDiv = "jsvalRef",
              containingDiv = function(value)
              {
                if(!missing(value))
                  .containingDiv <<- value
                else
                  .containingDiv
              }
                ),
            methods = list(
              SetupViewLayers = function()
              {
                layer = .self$.layer
                if(is.null(layer))
                  stop("Unable to setup ViewLayers with no root Layer present.")
                else
                  {
                    if(length(.self$viewLayers))
                      .self$CleanupViewLayers(.self$viewLayers[[1]])
                    .self$CreateViewLayers(layer, NULL, zindex=1)
                    .self$SetContextTransforms()

                  }
              },
              #this should clean up the passed in ViewLayer, including all of it's children
              CleanupViewLayers = function(viewLayer)
                #destroy all contexts and all divs except the parent. Set parent div's innerHTML to empty string
              {
                #this may leak memory by not destroying the canvas elements properly, but I think it should.
                #for firepaint ViewLayer$nativeResource is list(context, div)
                set_JS_Property(ScriptCon, viewLayer$nativeResource[[2]], "innerHTML", "")
                .self$viewLayers = do.call("c", sapply(.self$viewLayers,
                  function(x)
                  {
                    if(x != viewLayer)
                      x
                    else
                      NULL
                  }))
                
                return(TRUE)
              },

                #I think this is going to have to be recursive?
              CreateViewLayers = function(layer, parent = NULL, zindex)
                  {
                    if(length(layer$geometry))
                                        #geometry x1, y1
                                        #         x2, y2
                      {
                        tmp = cbind(layer$geometry, 1)
                        #print(tmp)
                        #print(.self$transform)
                        devspace = {tmp %*% .self$transform}[1:2,]
                        dims = abs( c( devspace[1,1] - devspace[1, 2] , devspace[2, 1] - devspace[2, 2]) )
                        position = c (min( devspace[ 1 , ] ) , max( devspace[ 2 , ] ) ) 
                   
                      } else if (!is.null(layer$layout)) {
                        stop("Layouts not supported yet")
                      } else {
                                        # assume simple stacking
                        if(length(parent))
                          {
                            position = parent$position
                            dims = parent$dim
                          } else {
                            return(zindex)
                          }
                        
                      }
                    newVL = .self$ViewLayerFactory(layer = layer, parent = parent, dim = dims, position = position, zindex)
                    .self$viewLayers = c(.self$viewLayers, newVL)
                    if(length(.self$viewLayers) == 1 && class(.self$viewLayers) != "list")
                       .viewLayers <<- list(.self$viewLayers)
                    zindex = zindex + 1
                    for(i in seq(along = layer$children))
                      {
                        zindex = .self$CreateViewLayers(layer$children[[i]], parent = newVL, zindex)
                      }
                    return(zindex)
                  }
              
                  ,
              ViewLayerFactory  = function(layer, parent, dim, position, zindex)
              {
                fViewLayer$new(layer = layer, parent = parent, dim= dim, position = position, zindex = zindex, view = .self)
              },
              SetContextTransforms = function()
              {
                tform = .self$transform
                if(!length(tform))
                  args = c(1, 0, 0, 1, 0, 0)
                else
                  args = c(tform[1:2, 1], tform[1:2, 2], tform[1:2, 3])
                vls = .self$viewLayers
                
                for(i in seq(along = vls))
                    {
                      
                      print(vls[[1]]$nativeResource)     
                      call_JS_Method(ScriptCon, vls[[i]]$nativeResource[[1]], "setTransform", args, addRoot = FALSE)
                    }
                return(TRUE)
              },
              #vectorized
              ApplyTransform = function(x, y)
              {
                pts = cbind(x, y, 1)
                res = pts %*% .self$transform
                t(res[1:2,])
              },
              paint = function()
              {
                vls = .self$viewLayers
                print(vls) 
                for (i in seq(along = vls))
                  {
                    #print(vls[[i]]$layer$handlers)
                    print(vls[[i]]$nativeResource)
                    
                    p = vls[[i]]$createPainter()
                    vls[[i]]$layer$paint(p)

                  }
                

              }
            ),
  contains = "View")

  



if(FALSE)
  {
    
                    
                      script = "document;"
                      doc = jsVal(addRoot = TRUE)
                      JS_EvaluateScript(ScriptCon, script= script, nchar(script), "doc", 1, doc)
                    if (is.null(parent))
                      {
                        pardiv = doc
                      } else {
                        pardiv = parent$nativeResource[[2]]
                      }
                      
                      
                      
                    if(FALSE)
                      {
                    script = "document;"
                    doc = jsVal(addRoot = TRUE)
                    JS_EvaluateScript(ScriptCon, script= script, nchar(script), "doc", 1, doc)
                    newcanv = call_JS_Method(ScriptCon, doc, "createElement", "canvas")
                    newdiv = call_JS_Method(ScriptCon, doc, "createElement", "div")
                    #Give the canvas and div the right dimensions
                    
                    oldStyle = get_JS_Attribute(ScriptCon, parentdiv, "style", addRoot = TRUE)
                    oldWidth = get_JS_Attribute(Scriptcon, oldStyle, "width", addRoot = TRUE)
                    oldHeight = get_JS_Attribute(Scriptcon, oldStyle, "height", addRoot = TRUE)
                    if(length(layer$geometry))
                      {
                        #x1, y1
                        #x2, y2
                      geom = layer$geometry
                    } else if (!is.null(layer$layout))
                      {
                        layout = layer$layout
                      } else {
                        #if no layout or geometry information is specified, we assume that the layer is simply stacked on top of it's parent, taking up the same area
                        
                        style = paste( "width:",oldWidth,";height:", oldHeight, ";left:0px;top:0px;position:absolute;z-index:" , zindex , ";" , sep = "" )
                        set_JS_Property(ScriptCon, newdiv, "style", style)
                      }

                    #grab div that is currently on top
                    
                    call_JS_Method(ScriptCon, olddiv, "appendChild", newcanv, addRoot = FALSE)
                    call_JS_Method(ScriptCon, olddiv, "appendChild", newdiv, addRoot = FALSE)
                    newctx = call_JS_Method(ScriptCon, newcanv, "getContext", "2d")
                    .self$canvases = list(.self$canvases, newcanv)
                    .self$divs = list(.self$divs, newdiv) 
                  }
                  

                      



                    }
