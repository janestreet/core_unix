# Time_float_unix

`Time_float_unix` is a standalone library that extends `Core.Time_float` and
depends on `Core_unix`.  Previously known as `Time_unix` and `Core.Time`.

Idiomatic usage is to put a module alias in `import.ml` or near the
top of a file:

    module Time = Time_float_unix

For stable types, idiomatic usage is to add an alias to the stable
submodule:

    module Time = Time_float_unix.Stable
