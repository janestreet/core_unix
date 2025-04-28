#include <stdint.h>
#include <time.h>

struct timespec timespec_of_double(double seconds);
double timespec_to_double(struct timespec ts);

struct timespec timespec_of_int_ns(int64_t nanoseconds);
int64_t timespec_to_int_ns(struct timespec ts);
