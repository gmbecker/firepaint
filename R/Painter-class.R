fPainter = setRefClass("firePainter",
            ##These fields will be set by the calling Layer
            fields = list(
              ctx = "jsvalRef",
              canvas = "jsvalRef"),
            methods = list(
              drawLine = function(x, y, stroke = NULL) {
                
                call_JS_Method(ScriptCon, .self$ctx, "beginPath", list(), addRoot = FALSE)
                
                res = mapply(function(x,y)
                  {
                    call_JS_Method(ScriptCon, .self$ctx, "lineTo", list(x,y), addRoot = FALSE)
                    call_JS_Method(ScriptCon, .self$ctx, "moveTo", list(x,y), addRoot = FALSE)
                  },
                    
                       x,
                       y)
                call_JS_Method(ScriptCon, .self$ctx, "moveTo", list(0, 0), addRoot = FALSE)
                call_JS_Method(ScriptCon, .self$ctx, "stroke", list(), addRoot = FALSE)
                print(res)
                TRUE
              },
              drawSegment = function(x0, y0, x1, y1, stroke = NULL) {
                call_JS_Method(ScriptCon, .self$ctx, "beginPath", list(), addRoot = FALSE)
                mapply(function(x0, y0, x1, y1) {
                  call_JS_Method(ScriptCon, .self$ctx, "moveTo", list(x0, y0), addRoot = FALSE)
                  call_JS_Method(ScriptCon, .self$ctx, "lineTo", list(x1, y1), addRoot = FALSE)
                }, x0, y0, x1, y1)
                call_JS_Method(ScriptCon, .self$ctx, "stroke", list(), addRoot = FALSE)
                
                TRUE
              },
              ##Not sure what to do about the point method. Very small filled circle or rectangle?
              ##currently they are non-filled circles of radius 5 pixels
              drawPoint = function(x,y, stroke = NULL) {
                print("in drawPoint function")
                if(!is.null(stroke))
                  stroke = col2rgb(stroke)
                if (!length(stroke))
                  {
                    change = rep(FALSE, times= length(x))
                    call_JS_Method(ScriptCon, .self$ctx, "beginPath", list(), addRoot = FALSE)
                  } else {
                    
                    if (length(stroke) == 1)
                      change = c(TRUE,rep(FALSE, times = length(x) - 1))
                    else
                      {
                        stroke2 = stroke[-1]
                        change = c(TRUE, stroke == stroke2)
                      }
                  }
                mapply(function(x, y, strokeChange, stroke) {
                  if (strokeChange)
                    {
                      ##This will likely cause strange results if drawPoint is called when there is a path already started.
                      call_JS_Method(ScriptCon, .self$ctx, "fill", list(), addRoot = FALSE)
                      set_JS_Property(ScriptCon, .self$ctx, "strokeStyle", list(stroke))
                      call_JS_Method(ScriptCon, .self$ctx, "beginPath", list(), addRoot = FALSE)
                    }
                  call_JS_Method(ScriptCon, .self$ctx, "rect", list(x, y, 1, 1), addRoot = FALSE)
                }, x, y, change, stroke)
                call_JS_Method(ScriptCon, .self$ctx, "fill", list(), addRoot = FALSE)
                TRUE

              },
              drawRect = function(xleft, ybottom, xright, ytop, stroke = NULL, fill = NULL)
              {
                call_JS_Method(ScriptCon, .self$ctx, "beginPath", list(), addRoot = FALSE)
                ##the documentation isn't clear whether this is a global function or a method on the context
                glob = JS_GetGlobalObject(Scriptcon, returnInputs = FALSE)
                ##rect gives us the ability to be vectorized and only draw once, but I don't think we can get stroke/fill capabilities per rectangle
                if (is.null(fill) & length(stroke) < 2)
                  {
                    mapply(function(x0, y0, x1, y1) {
                      call_JS_Method(SciptCon, glob, "rect",
                                     list(x0, y0, x1 - x0, y1 - y0),
                                     addRoot = FALSE)
                    }, xleft, ybottom, xright, ytop)
                    JS_Call_Method(ScriptCon, .self$ctx, "stroke", list(), addRoot = FALSE)
                  }
                TRUE
              }),
            contains = "Painter")
              
#experiment for speed. This is much faster than naively looping in R 
fPainter2 = setRefClass("firePainter2",
  fields = list(canvas = "character", ctx = "character"), 
  methods = list(
    drawLine = function(x, y, stroke = NULL) {
      
      c1 = paste(.self$ctx, ".beginPath();", sep="")
      #call_JS_Method(ScriptCon, .self$ctx, "beginPath", list(), addRoot = FALSE)
       
      cmiddle = mapply(function(x,y) paste(.self$ctx, ".lineTo(", x, " , ", y, ");", sep = ""),
        #call_JS_Method(ScriptCon, .self$ctx, "lineTo", list(x,y), addRoot = FALSE),
                       x,
        y)
      cend = paste(.self$ctx, ".stroke();", sep="")
      #call_JS_Method(ScriptCon, .self$ctx, "stroke", list(), addRoot = FALSE)

      script = paste(c(c1, cmiddle, cend), collapse = " ")
      print(script)
      ret = jsVal()
      JS_EvaluateScript(ScriptCon,
                        JS_GetGlobalObject(ScriptCon),
                        script,
                        nchar(script),
                        "drawPoints",
                        1L, ret)
      
      TRUE
    },

    drawPoint = function(x,y, stroke)
    {
      script = paste(
        paste(.self$ctx, ".beginPath();", sep=""),
        paste(#.self$ctx, ".moveTo(" , x , " , " , y , ");\n",
              .self$ctx, ".rect(", x , " , " , y , " , ", 1 , " , ", 1 , ");", sep=""),
        paste(.self$ctx, ".fill();", sep = ""),
        collapse="\n")

      ret = jsVal()
      JS_EvaluateScript(ScriptCon,
                        JS_GetGlobalObject(ScriptCon),
                        script,
                        nchar(script),
                        "drawPoints",
                        1L, ret)
      
      TRUE                  
    }),
  contains = "Painter")
                       
