#include <R.h>
#include <Rdefines.h>
#include "jsapi.h"

static const double smallpi = 3.1415926535924; //slightly bigger than pi so we the full circle when using arc

SEXP
R_DrawPoints(SEXP Rx, SEXP Ry, SEXP Rstroke, SEXP Rjscon, SEXP Rchange, SEXP Rctx)
{
  int len = LENGTH(Rx);
  double *x = REAL(Rx);
  double *y = REAL(Ry);
  int *change = LOGICAL(Rchange);
  JSContext *jscon =  (JSContext *) R_ExternalPtrAddr(GET_SLOT( Rjscon , Rf_install( "ref" ) ) );
  jsval *val =  (jsval *) R_ExternalPtrAddr(GET_SLOT( Rctx , Rf_install( "ref" ) ) );
  JSObject *ctx;
  JS_ValueToObject(jscon, *val, &ctx);
  JS_AddObjectRoot(jscon, &ctx);
  jsval *ret = (jsval *) JS_malloc(jscon, sizeof(jsval));
  JS_AddValueRoot(jscon, ret);
  JS_NewNumberValue(jscon, 1.0, ret);
  
  jsval  args4[4], tmp;
  JSString *tmpstr;
  int strrooted = 0;
  for (int j = 0; j < 4; j++)
    {

      JS_NewNumberValue(jscon, 1.0, &args4[j]);
      JS_AddValueRoot(jscon, &args4[j]);
    }
    
  for(int i =0; i < len; i++)
    {
      if(change[i])
	{
	  JS_CallFunctionName(jscon, ctx, "fill", 0,  NULL, ret);
	  tmpstr = JS_NewStringCopyZ( jscon , CHAR( STRING_ELT(Rstroke , i) ) );
	  if (!strrooted)
	    {
	      JS_AddStringRoot(jscon, &tmpstr);
	      strrooted = 1;
	    }
	  tmp = STRING_TO_JSVAL(tmpstr);
	  JS_SetProperty(jscon, ctx, "strokeStyle", &tmp);
	  JS_CallFunctionName(jscon, ctx, "beginPath",0, NULL, ret);
	}
      JS_NewNumberValue(jscon, x[i], &args4[0]);
      JS_NewNumberValue(jscon, y[i], &args4[1]);
      JS_CallFunctionName(jscon, ctx, "rect", 4, &args4, ret);
      
    }
  JS_CallFunctionName(jscon, ctx, "fill", 0, NULL, ret);
  JS_RemoveValueRoot(jscon, ret);
  JS_free(jscon, ret);
  if(strrooted)
    JS_RemoveStringRoot(jscon, &tmpstr);
  JS_RemoveObjectRoot(jscon, &ctx);
  for (int i =0; i<4; i++)
    JS_RemoveValueRoot(jscon, &args4[i]);
  return ScalarLogical(1);
}

SEXP 
R_DrawCircles(SEXP Rx, SEXP Ry, SEXP Rr, SEXP Rstroke, SEXP Rjscon, SEXP Rchange, SEXP Rctx)
{
  int len = LENGTH(Rx);
  double *x = REAL(Rx);
  double *y = REAL(Ry);
  double *r = REAL(Rr);
  int *change = LOGICAL(Rchange);
  JSContext *jscon =  (JSContext *) R_ExternalPtrAddr(GET_SLOT( Rjscon , Rf_install( "ref" ) ) );
  jsval *val =  (jsval *) R_ExternalPtrAddr(GET_SLOT( Rctx , Rf_install( "ref" ) ) );
  JSObject *ctx;
  JS_ValueToObject(jscon, *val, &ctx);
  JS_AddObjectRoot(jscon, &ctx);
  jsval *ret = (jsval *) JS_malloc(jscon, sizeof(jsval));
  JS_AddValueRoot(jscon, ret);
  JS_NewNumberValue(jscon, 1.0, ret);
  
  jsval  args6[6], tmp;
  JSString *tmpstr;
  int strrooted = 0;
  for (int j = 0; j < 3; j++)
    {
      JS_NewNumberValue(jscon, 5.0, &args6[j]);
      JS_AddValueRoot(jscon, &args6[j]);
    }
  JS_NewNumberValue(jscon, 0.0, &args6[3]);
  //JS_NewNumberValue(jscon, 5.0, &args6[3]);
  JS_AddValueRoot(jscon, &args6[3]);
  JS_NewNumberValue(jscon, 2.0 * smallpi , &args6[4]);
  JS_AddValueRoot(jscon, &args6[4]);
  //JS_NewNumberValue(jscon, BOOLEAN_TO_JSVAL(1) , &args6[4]);
  args6[5] = BOOLEAN_TO_JSVAL((JSBool) 1);
  JS_AddValueRoot(jscon, &args6[5]);
  JS_CallFunctionName(jscon, ctx, "beginPath",0, NULL, ret);  
  for(int i =0; i < len; i++)
    {
      if(change[i])
	{
	  JS_CallFunctionName(jscon, ctx, "fill", 0,  NULL, ret);
	  tmpstr = JS_NewStringCopyZ( jscon , CHAR( STRING_ELT(Rstroke , i) ) );
	  if (!strrooted)
	    {
	      JS_AddStringRoot(jscon, &tmpstr);
	      strrooted = 1;
	    }
	  tmp = STRING_TO_JSVAL(tmpstr);
	  JS_SetProperty(jscon, ctx, "strokeStyle", &tmp);
	  JS_CallFunctionName(jscon, ctx, "beginPath",0, NULL, ret);
	}
      JS_NewNumberValue(jscon, x[i], &args6[0]);
      JS_NewNumberValue(jscon, y[i], &args6[1]);
      JS_NewNumberValue(jscon, r[i], &args6[2]);
      //we don't want to fill the paths between the circles
      JS_CallFunctionName(jscon, ctx, "moveTo", 2, &args6, ret);
      JS_CallFunctionName(jscon, ctx, "arc", 6, &args6, ret);
      //JS_CallFunctionName(jscon, ctx, "rect", 4, &args6, ret);
     
      
    }
  JS_CallFunctionName(jscon, ctx, "fill", 0, NULL, ret);
  JS_RemoveValueRoot(jscon, ret);
  JS_free(jscon, ret);
  if(strrooted)
    JS_RemoveStringRoot(jscon, &tmpstr);
  JS_RemoveObjectRoot(jscon, &ctx);
  for (int i =0; i<6; i++)
    JS_RemoveValueRoot(jscon, &args6[i]);
  return ScalarLogical(1);

}
