open! Core
open! Import

module%test [@name "Unix.Cidr.Stable.V1"] _ = Stable_unit_test.Make (struct
    include Unix.Cidr.Stable.V1

    let equal a b = compare a b = 0

    let tests =
      [ Unix.Cidr.of_string "0.0.0.0/8", "0.0.0.0/8", "\000\b"
      ; Unix.Cidr.of_string "123.213.1.51/13", "123.208.0.0/13", "\253\000\000\208{\r"
      ]
    ;;
  end)
