#include <math.h>
#include <stdint.h>
#include <time.h>

struct timespec timespec_of_double(double seconds) {
  struct timespec ts;

  ts.tv_sec = (time_t)floor(seconds);
  ts.tv_nsec = (long)(1e9 * (seconds - ts.tv_sec));

  return ts;
}

double timespec_to_double(struct timespec ts) {
  return (double)ts.tv_sec + ((double)ts.tv_nsec / 1e9);
}

static const int64_t NS_IN_SEC = 1000000000;

struct timespec timespec_of_int_ns(int64_t nanoseconds) {
  struct timespec ts;

  ts.tv_sec = (time_t)(nanoseconds / NS_IN_SEC);
  ts.tv_nsec = (long)(nanoseconds % NS_IN_SEC);

  return ts;
}

int64_t timespec_to_int_ns(struct timespec ts) {
  return ((int64_t)ts.tv_sec) * NS_IN_SEC + (int64_t)ts.tv_nsec;
}
