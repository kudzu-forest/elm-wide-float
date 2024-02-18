# elm-wide-float

This module provides functionalities for expressing and calcurating floating point numbers which have 42 exponent bits (31 bits more than ordinary `Float`) and 52 or 53 significand bits (the same count as `Float`). 


## What this module is for

If you have trouble with overflow or underflow of `Float` number, then this module may help you.

A positive `Float` covers from _2^(-1074)_(about 5e-324) to _2^1024_(about 1.8e308). In most cases this is enough, but exponentially changing value can easily exceeds this range within a few minutes.

A positive `WideFloat` in this module, on the other hand, can range from _2^(-2^42)_ to _2^(2^42)_. Even if a `WideFloat` grows 10 times larger every seconds, it takes several thousands years to overflow.


## What this module is *not* for

If you are seeking to get higher _precision_, this package could nothing for you. In that case, you should take a look at `BigInt` or `BigFloat` package that can have arbitrary size and precision.

The number of significand bits in `WideFloat` is 52, which is the same count as ordinary `Float`s, meaning that the precision of `Float` and `WideFloat` has no difference.


Another caution is that this module is *not* intended to satisfy all the specs included in IEEE754, especially about dealing with non normalized numbers like `NaN` and `Infinity`.

The behavior of `WideFloat` at overflowing, underflowing, division by 0, and creation from `NaN` is not defined, and may differ depending on the published version.


## Coorporation

	- Please notify the author if you find bugs.
	- If you want this package to expose additional functionalities, let me know through GitHub.
	- Advise for better naming, better coding or better documentation are welcome.
