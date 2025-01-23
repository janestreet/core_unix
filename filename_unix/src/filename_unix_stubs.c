#define _XOPEN_SOURCE 500
#include "ocaml_utils.h"

#if OCAML_VERSION >= 41300

CAMLprim value caml_unix_realpath(value);

CAMLprim value core_unix_realpath(value v_path) {
  return caml_unix_realpath(v_path);
}

#elif !defined(_WIN32)

CAMLprim value core_unix_realpath(value v_path)
{
  caml_unix_check_path(v_path, "realpath");
  char *res = realpath(String_val(v_path), NULL);
  if (res == NULL)
    caml_uerror("realpath", v_path);
  else {
    value v_res = caml_copy_string(res);
    free(res);
    return v_res;
  }
}

#else

CAMLprim value core_unix_realpath(value __unused v_path) {
{
  caml_invalid_argument("realpath not implemented");
}

#endif
