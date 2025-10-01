
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <locale.h>
#ifdef __APPLE__
#include <xlocale.h>
#endif
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include "ocaml_utils.h"
#include "timespec.h"
/* Improved localtime implementation

   Addresses bug:

   http://caml.inria.fr/mantis/view.php?id=5193
 */

#include <errno.h>
#include <stdio.h>

#include "config.h"

#ifdef JSC_POSIX_TIMERS

static inline clockid_t clockid_t_of_val(value clock_type) {
  if (Is_block(clock_type)) {
    return Long_val(Field(clock_type, 0));
  }

  switch (Long_val(clock_type)) {
  case 0:
    return CLOCK_REALTIME;
  case 1:
    return CLOCK_MONOTONIC;
  case 2:
    return CLOCK_PROCESS_CPUTIME_ID;
  case 3:
    return CLOCK_THREAD_CPUTIME_ID;
  };

  caml_failwith("invalid Clock.t");
}

static inline value val_underlying_of_clockid_t(clockid_t clock) {
  return Val_long(clock);
}

#ifdef JSC_CLOCK_GETCPUCLOCKID

CAMLprim value caml_clock_getcpuclockid(value v_pid) {
  pid_t pid = Long_val(v_pid);

  clockid_t clock;

  int ret = clock_getcpuclockid(pid, &clock);

  /*  HEADS UP: error returns are *not* negated here, quite surprisingly. Check
     the man page. Error codes are positive here. */

  if (ret != 0) {
    unix_error(ret, "clock_getcpuclockid", Nothing);
  }

  return val_underlying_of_clockid_t(clock);
}

#endif

value caml_clock_getres(value clock_type) {
  struct timespec tp;
  clock_getres(clockid_t_of_val(clock_type), &tp);
  return (
      caml_alloc_int63(((int64_t)tp.tv_sec * 1000 * 1000 * 1000) + (int64_t)tp.tv_nsec));
}

value caml_clock_gettime(value clock_type) {
  struct timespec tp;
  clock_gettime(clockid_t_of_val(clock_type), &tp);
  return (
      caml_alloc_int63(((int64_t)tp.tv_sec * 1000 * 1000 * 1000) + (int64_t)tp.tv_nsec));
}

#endif /* JSC_POSIX_TIMERS */

static value alloc_tm(const struct tm *tm) {
  value res;
  res = caml_alloc_small(9, 0);
  Field(res, 0) = Val_int(tm->tm_sec);
  Field(res, 1) = Val_int(tm->tm_min);
  Field(res, 2) = Val_int(tm->tm_hour);
  Field(res, 3) = Val_int(tm->tm_mday);
  Field(res, 4) = Val_int(tm->tm_mon);
  Field(res, 5) = Val_int(tm->tm_year);
  Field(res, 6) = Val_int(tm->tm_wday);
  Field(res, 7) = Val_int(tm->tm_yday);
  Field(res, 8) = tm->tm_isdst ? Val_true : Val_false;
  return res;
}

static void assign_tm(struct tm *tm, value v_tm) {
  tm->tm_sec = Int_val(Field(v_tm, 0));
  tm->tm_min = Int_val(Field(v_tm, 1));
  tm->tm_hour = Int_val(Field(v_tm, 2));
  tm->tm_mday = Int_val(Field(v_tm, 3));
  tm->tm_mon = Int_val(Field(v_tm, 4));
  tm->tm_year = Int_val(Field(v_tm, 5));
  tm->tm_wday = Int_val(Field(v_tm, 6));
  tm->tm_yday = Int_val(Field(v_tm, 7));
  tm->tm_isdst = Bool_val(Field(v_tm, 8));
}

/* Thread-safe if its function argument is. */
CAMLprim value core_time_ns_strftime_gen(locale_t locale, value v_tm, value v_fmt,
                                         size_t f_strftime(char *, size_t, const char *,
                                                           const struct tm *, locale_t)) {
  struct tm tm = {0};
  assign_tm(&tm, v_tm);
  size_t buf_len = 10 * caml_string_length(v_fmt) + 1;
  char *buf;
  while (1) {
    buf = malloc(buf_len);
    if (!buf) {
      caml_failwith("core_time_ns_format_tm: malloc failed");
    }
    /* POSIX only recently required strftime and strftime_l to set errno to ERANGE when
       the buffer is too small, but most implementations don't yet do this, so we have to
       use a hack to distinguish zero-length output from failure. */
    buf[0] = '\x01';
    errno = 0;
    size_t len = f_strftime(buf, buf_len, String_val(v_fmt), &tm, locale);
    if (len || !buf[0])
      break;
    if (errno && errno != ERANGE) {
      free(buf);
      caml_failwith("core_time_ns_format_tm: strftime failed");
    }
    free(buf);
    buf_len *= 2;
  }
  value v_str = caml_copy_string(buf);
  free(buf);
  return v_str;
}

size_t strftime_callback(char *buf, size_t buf_len, const char *fmt, const struct tm *tm,
                         locale_t locale) {
  /* Ignore locale, it should always be 0. */
  (void)locale;
  return strftime(buf, buf_len, fmt, tm);
}

CAMLprim value core_time_ns_strftime(value v_tm, value v_fmt) {
  return core_time_ns_strftime_gen((locale_t)0, v_tm, v_fmt, strftime_callback);
}

/* Its OCaml binding promises that this function is thread-safe. */
CAMLprim value core_time_ns_strftime_l(value v_locale, value v_tm, value v_fmt) {
  locale_t locale = (locale_t)Nativeint_val(v_locale);
  return core_time_ns_strftime_gen(locale, v_tm, v_fmt, strftime_l);
}

/*
 * converts a tm structure to a float with the assumption that that the
 * structure defines a gmtime
 */
CAMLprim value core_timegm(value v_tm) {
  struct tm tm = {0};
  time_t res;
  assign_tm(&tm, v_tm);
  /*  tm_isdst is not used by timegm (which sets it to 0) */

  res = timegm(&tm);

  if (res == (time_t)-1)
    caml_failwith("timegm");

  return caml_copy_double((double)res);
}

CAMLprim value core_time_ns_nanosleep(value v_seconds) {
  struct timespec req = timespec_of_double(Double_val(v_seconds));
  struct timespec rem;
  int retval;

  caml_enter_blocking_section();
  retval = nanosleep(&req, &rem);
  caml_leave_blocking_section();

  if (retval == 0)
    return caml_copy_double(0.0);
  else if (retval == -1) {
    if (errno == EINTR)
      return caml_copy_double(timespec_to_double(rem));
    else
      uerror("nanosleep", Nothing);
  } else
    caml_failwith("core_time_ns_nanosleep: impossible return value from nanosleep(2)");
}

/*
 * These are the same functions as the ones in ocaml except that they call
 * {localtime,gmtime}_r instead of {localtime,gmtime} to avoid setting the
 * global tzname (instead setting the tm_store value that we discard).
 */
#define WRAP_TIME_FUN(NAME, ERROR)                                                       \
  CAMLprim value core_##NAME(value t) {                                                  \
    time_t clock;                                                                        \
    struct tm *tm;                                                                       \
    struct tm tm_store;                                                                  \
    clock = (time_t)Double_val(t);                                                       \
    tm = NAME##_r(&clock, &tm_store);                                                    \
    if (tm == NULL)                                                                      \
      caml_failwith(ERROR);                                                              \
    return alloc_tm(tm);                                                                 \
  }

WRAP_TIME_FUN(localtime, "localtime")
WRAP_TIME_FUN(gmtime, "gmtime")
