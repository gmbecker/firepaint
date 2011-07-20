
fLayer = setRefClass("fireLayer",
  fields = list(zaxis = "numeric",
    canvas = "jsvalRef",
    handlers = "LayerHandlers",
    paint = "activeBindingFunction"),
  contains = "Layer")
