#define _GNU_SOURCE

#include <errno.h>
#include <limits.h>
#include <stdlib.h>

#include "ocaml_utils.h"

/* Pathname resolution */

/* Seems like a sane approach to getting a reasonable bound for the
   maximum path length */
#ifdef PATH_MAX
#define JANE_PATH_MAX ((PATH_MAX <= 0 || PATH_MAX > 65536) ? 65536 : PATH_MAX)
#else
#define JANE_PATH_MAX (65536)
#endif

CAMLprim value core_unix_realpath(value v_path) {
  CAMLparam1(v_path);
  char *path = caml_stat_strdup(String_val(v_path));

  caml_enter_blocking_section();

#ifdef __GLIBC__
  char *res = realpath(path, NULL);
  char *to_free = res;
#else
  /* [realpath] is inherently broken without GNU-extension, and this
     seems like a reasonable thing to do if we do not build against
     GLIBC. */
  char resolved_path[JANE_PATH_MAX];
  char *res = realpath(path, resolved_path);
  char *to_free = NULL;
#endif

  int err = errno;

  caml_leave_blocking_section();

  caml_stat_free(path);

  if (res == NULL) {
    free(to_free);
    errno = err;
    uerror("realpath", v_path);
  }

  value v_res = caml_copy_string(res);

  free(to_free);

  CAMLreturn(v_res);
}
