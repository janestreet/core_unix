
//Provides: tsc_get
//Requires: caml_int64_of_float
function tsc_get() {
    return caml_int64_of_float(performance.now() * 1000.0)
}

//Provides: caml_rdtsc
//Requires: caml_int64_of_float
function caml_rdtsc() {
    return caml_int64_of_float(performance.now() * 1000.0)
}

//Provides: tsc_nanosleep
//Requires: caml_failwith
function tsc_nanosleep(ns) {
    caml_failwith("nanosleep is not supported in javascript")
}
