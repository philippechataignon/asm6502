powers = (1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000)

fpow        .function _x
            .endf _x & $ff

            .byte fpow(powers)

strlow       .function _x
            _a := _x[:-1] + $80
            _b := _x[-1]
            .endf [_a] .. [_b]

            .text   strlow(["a", "b", "c"])
