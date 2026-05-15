#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <time.h>
#include <math.h>
#include <sys/time.h>
#include <stdint.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

#include "ocaml_utils.h"

/* CAMLweakdef allows linking with the Ocaml_intrinsics library,
   which also implements these stubs. The stubs cannot be renamed
   because our compiler's [@@builtin] recognition relies on them.

   The `rdtsc` implementation is duplicated to assure it always
   matches the strong symbol in OCaml_intrinsics.
*/

#if (defined(__i386__) || defined(__x86_64__))
static uint64_t rdtsc()
{
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ((uint64_t)lo) | (((uint64_t)hi)<<32);
}
#elif defined(__aarch64__)
static uint64_t rdtsc()
{
  uint64_t tsc;
  asm volatile("mrs %0, cntvct_el0" : "=r" (tsc));
  return tsc;
}
#endif

CAMLprim CAMLweakdef uint64_t caml_rdtsc_unboxed(value unit) {
  (void)unit;
  return rdtsc();
}

CAMLprim CAMLweakdef value caml_rdtsc(value unit) {
  (void)unit;
  return caml_copy_int64(rdtsc());
}

static struct timespec timespec_of_double(double seconds)
{
  struct timespec ts;
  ts.tv_sec = (time_t) floor(seconds);
  ts.tv_nsec = (long) (1e9 * (seconds - ts.tv_sec));
  return ts;
}

static double timespec_to_double(struct timespec ts)
{
  return (double) ts.tv_sec + ((double) ts.tv_nsec / 1e9);
}

/* This function is copied from Core_unix, as we cannot depend on Core_unix if this
   library is to be compilable under Js_of_ocaml. */
CAMLprim value tsc_nanosleep(value v_seconds)
{
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
      caml_failwith("tcs_nanosleep failed");
  }
  else
    caml_failwith("tcs_nanosleep: impossible return value from nanosleep(2)");
}
