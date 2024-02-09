# elm-wide-float

This module provides functionalities for expressing and calcurating floating point number, named `WideFloat`, which has 32 exponent bits (21 bits more than ordinary `Float`).

The number of significand bit is 52 (the same as ordinary `Float`).

This module is not intended to cover all the specs included in IEEE754, especially about dealing with non normalized numbers like `NaN` and `Infinity`.

The behavior at overflowing, underflowing, division by 0, and creation from `NaN` is undefined, and may differ depending on the published version.

Please notify the author if you have some usecase that needs additional functionalities.
