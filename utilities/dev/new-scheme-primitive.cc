// %[C function name? %]
// %[Scheme procedure name? %]
// %[Number of required args? %]
// %[Number of optional args? %]
// %[Varargs? (0 or 1) %]

SCWM_PROC (%1, "%2", %3, %4, %5,
           (%@))
     /**
      */
#define FUNC_NAME s_%1
{
  int iarg = 1;
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

