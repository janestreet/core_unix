#define _GNU_SOURCE

#include <assert.h>
#include <errno.h>
#include <langinfo.h>
#include <locale.h>
#include <string.h>
#ifdef __APPLE__
#include <xlocale.h>
#endif
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include "ocaml_utils.h"

static_assert(sizeof(locale_t) == sizeof(intnat), "");

#define DEFINE_INT32_CONSTANT(name, z)                                                   \
  CAMLprim value name(value __unused v_unit) { return (z); }                             \
  CAMLprim value name##_bytecode(value __unused v_unit) { return caml_copy_int32(z); }

#define UNIX_INT32_CONST(CONST) DEFINE_INT32_CONSTANT(unix_##CONST, CONST)

#define DEFINE_NATIVEINT_CONSTANT(name, z)                                               \
  CAMLprim value name(value __unused v_unit) { return (z); }                             \
  CAMLprim value name##_bytecode(value __unused v_unit) { return caml_copy_nativeint(z); }

#define UNIX_NATIVEINT_CONST(CONST) DEFINE_NATIVEINT_CONSTANT(unix_##CONST, (intnat)CONST)

UNIX_INT32_CONST(LC_ALL)
UNIX_INT32_CONST(LC_CTYPE)
UNIX_INT32_CONST(LC_COLLATE)
UNIX_INT32_CONST(LC_MESSAGES)
UNIX_INT32_CONST(LC_MONETARY)
UNIX_INT32_CONST(LC_NUMERIC)
UNIX_INT32_CONST(LC_TIME)

UNIX_INT32_CONST(LC_ALL_MASK)
UNIX_INT32_CONST(LC_CTYPE_MASK)
UNIX_INT32_CONST(LC_COLLATE_MASK)
UNIX_INT32_CONST(LC_MESSAGES_MASK)
UNIX_INT32_CONST(LC_MONETARY_MASK)
UNIX_INT32_CONST(LC_NUMERIC_MASK)
UNIX_INT32_CONST(LC_TIME_MASK)

static const locale_t zero_locale = (locale_t)0;

UNIX_NATIVEINT_CONST(LC_GLOBAL_LOCALE)
UNIX_NATIVEINT_CONST(zero_locale)

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim value unix_freelocale(intnat locale) {
  errno = 0;
  freelocale((locale_t)locale);
  if (errno)
    uerror("freelocale", Nothing);
  return Val_unit;
}

CAMLprim value unix_freelocale_bytecode(value v_locale) {
  return unix_freelocale(Nativeint_val(v_locale));
}

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim intnat unix_duplocale(intnat locale) {
  errno = 0;
  locale_t result = duplocale((locale_t)locale);
  if (result == zero_locale)
    uerror("duplocale", Nothing);
  return (intnat)result;
}

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim value unix_duplocale_bytecode(value v_locale) {
  return caml_copy_nativeint(unix_duplocale(Nativeint_val(v_locale)));
}

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim intnat unix_newlocale(int32_t category_mask, value v_locale, intnat base) {
  errno = 0;
  locale_t result = newlocale(category_mask, String_val(v_locale), (locale_t)base);
  if (result == zero_locale)
    uerror("newlocale", Nothing);
  return (intnat)result;
}

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim value unix_newlocale_bytecode(value v_category_mask, value v_locale,
                                       value v_base) {
  return caml_copy_nativeint(
      unix_newlocale(Int32_val(v_category_mask), v_locale, Nativeint_val(v_base)));
}

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim intnat unix_uselocale(intnat locale) {
  errno = 0;
  locale_t result = uselocale((locale_t)locale);
  if (result == zero_locale)
    uerror("uselocale", Nothing);
  return (intnat)result;
}

CAMLprim value unix_uselocale_bytecode(value v_locale) {
  return caml_copy_nativeint(unix_uselocale(Nativeint_val(v_locale)));
}

CAMLprim value unix_getlocalename(int32_t category, intnat i_locale) {
  locale_t locale = (locale_t)i_locale;
#if _POSIX_VERSION >= 202405L
  errno = 0;
  const char *result = getlocalename_l(category, locale);
  if (!result)
    uerror("getlocalename_l", Nothing);
  return caml_copy_string(result);
#else
  if (locale == LC_GLOBAL_LOCALE) {
    errno = 0;
    const char *result = setlocale(category, NULL);
    if (!result)
      uerror("setlocale", Nothing);
    return caml_copy_string(result);
  } else {
    if (category == LC_ALL) {
      // NL_LOCALE_NAME/querylocale do not work with LC_ALL/LC_ALL_MASK, and the only way
      // to work around it would involve temporarily modifying the global locale with
      // [setlocale], which wouldn't be thread-safe, so instead fail.
      caml_failwith("This platform doesn't support getlocalename with category == LC_ALL "
                    "and locale != LC_GLOBAL_LOCALE");
    } else {
#if defined __linux__ || defined __CYGWIN__
      errno = 0;
      const char *result = nl_langinfo_l(NL_LOCALE_NAME(category), locale);
      if (!result || !*result)
        uerror("nl_langinfo_l", Nothing);
      return caml_copy_string(result);
#elif defined __APPLE__
      int category_mask;
      switch (category) {
      case LC_CTYPE:
        category_mask = LC_CTYPE_MASK;
        break;
      case LC_COLLATE:
        category_mask = LC_COLLATE_MASK;
        break;
      case LC_MESSAGES:
        category_mask = LC_MESSAGES_MASK;
        break;
      case LC_MONETARY:
        category_mask = LC_MONETARY_MASK;
        break;
      case LC_NUMERIC:
        category_mask = LC_NUMERIC_MASK;
        break;
      case LC_TIME:
        category_mask = LC_TIME_MASK;
        break;
      default:
        caml_failwith("querylocale: unsupported locale category");
      }
      const char *result = querylocale(category_mask, locale);
      if (!result || !*result)
        uerror("querylocale", Nothing);
      return caml_copy_string(result);
#else
#error "No getlocalename_l or equivalent implementation found"
#endif
    }
  }
#endif
}

CAMLprim value unix_getlocalename_bytecode(value v_category, value v_locale) {
  return unix_getlocalename(Int32_val(v_category), Nativeint_val(v_locale));
}

CAMLprim value unix_setlocale(int32_t category, value v_locale) {
  errno = 0;
  const char *result = setlocale(category, String_val(v_locale));
  if (!result)
    uerror("setlocale", v_locale);
  return caml_copy_string(result);
}

CAMLprim value unix_setlocale_bytecode(value v_category, value v_locale) {
  return unix_setlocale(Int32_val(v_category), v_locale);
}
