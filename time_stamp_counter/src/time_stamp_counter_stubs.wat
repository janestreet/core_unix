(module
   (import "env" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "env" "caml_js_global"
      (func $caml_js_global (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_to_float"
      (func $caml_js_to_float (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_meth_call"
      (func $caml_js_meth_call
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "env" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

   (data $performance "performance")
   (data $now "now")

   (func (export "caml_rdtsc") (param (ref eq)) (result (ref eq))
      (call $caml_copy_int64
         (i64.trunc_sat_f64_s
            (f64.mul (f64.const 1000.)
               (struct.get $float 0
                  (ref.cast (ref $float)
                     (call $caml_js_to_float
                        (call $caml_js_meth_call
                           (call $caml_js_get
                              (call $caml_js_global (ref.i31 (i32.const 0)))
                              (array.new_data $string $performance
                                 (i32.const 0) (i32.const 11)))
                           (array.new_data $string $now
                              (i32.const 0) (i32.const 3))
                           (array.new_fixed $block 1
                              (ref.i31 (i32.const 0)))))))))))

   (data $nanosleep_not_implemented "nanosleep is not supported in javascript")

   (func (export "tsc_nanosleep") (param (ref eq)) (result (ref eq))
      (call $caml_failwith
         (array.new_data $string $nanosleep_not_implemented
            (i32.const 0) (i32.const 40)))
      (ref.i31 (i32.const 0)))
)
