#http://unixpapa.com/js/testkey.html
eventMap = list(
  #the paint "event" is not an event like the rest of these. no javasript equivalent because it originates in R (from a layer$paint() call).
  #paint = NULL,
  #it is possible that keyPress is intended to be keydown, not keypress. keydown only happens once, keypress happens repeatedly until the key is released
  keyPress = "keypress",
  keyRelease = "keyup",
  #having click and doubleclick handlers is very dangerous. We should confirm that doubleclick is what we want 
  mouseDoubleClick = "dblclick",

  mouseMove = "mousemove",
  mousePress = "mousedown",
  mouseRelease = "mouseup",
  #this mapping may not be perfect. Need to be careful with nested/stacked objects. mouseenter and mouseleave are Microsoft proprietary though, so this is the best we're going to get. May need to fix with state.
  mouseEnter = "mouseover",
  mouseLeave = "mouseout",
  #apparently we can't prevent the defaut eventhandler for mousewheel, but we can add our own. This may or may not be an issue (ie accidentally scrolling when you mean to zoom...
  wheel = "mousewheel",
  #we are going to need some state to distinguish between moving and dragging. The javascript canvas does not appear to be able to differentiate them.
  dragEnter = "mouseover",
  dragLeave = "mouseout",
  dragMove = "mousemove",
  drop = "mouseup",
  #for focus to work, each canvas needs to have a tabIndex attribute. Even then, this may or may not do what Michael intended wrt focus.
  focusIn = "focus",
  focusOut = "blur"
  #I'm not sure what sizeHint is supposed to be so I'm leaving it out for now.
  )
  

  
  
  

fLayer = setRefClass("fireLayer",
  fields = list(
    handlers = function(value)
    {
      if(!missing(value))
        {
          .handlers = value
          sapply(slotNames(.handlers),
                 function(nm, map)
                 {
                   if (nm %in% names(map) && !is.null(slot(value, nm)))
                     {
                       print(paste("Attaching", nm, "event handler."))
                     }
                 }, map = eventMap)
        }
    },
    .children = "list",
    children = function(value)
    {
      if(!missing(value))
        .children <<- value
      else
        .children
      },
    .parent = "ANY",
    parent = function(value)
    {
      if(!missing(value))
        {
          if(class(value) == "Layer" || is.null(value))
            .parent <<- value
          else
            stop("Parent of a Layer must be a Layer or NULL")
        } else {
          .parent
        }
      },
    geometryBounds = function(value)
    {
      if(!missing(value))
        .geometryBounds <<- value
      else
        .geometryBounds
    },
    .layout = "Layout",
    layout = function(value)
    {
      if(!missing(value))
        .layout <<- value
      else
        .layout
    },
    .isCached = "logical",
    isCached = function(value)
    {
      if(!missing(value))
        .isCached <<- value
      else
        .isCached
    },
    .isClipped = "logical",
    isClipped = function(value)
    {
      if(!missing(value))
        .isClipped <<- value
      else
        .isClipped
    },
    .hasFocus = "logical",
    hasFocus = function(value)
    {
      if(!missing(value))
        .hasFocus <<- value
      else
        .hasFocus
    },
    .limits = "matrix",
    limits = function(value)
    {
      if(!missing(value))
        .limits <<- value
      else
        .limits
    },
    .handlers = "LayerHandlers",
    handlers = function(value)
    {
      if(!missing(value))
        .handlers <<- value
      else
        .handlers
      }
   ),
  methods = list(
    paint = function(p)
    {
      if(!is.null(.handlers@paint))
        .handlers@paint(p)
      else
        {
          warning("No paint event handler specified for this method")
          NULL
        }
    }),
  contains = "Layer")
