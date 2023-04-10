# calculator
This calculator parses a string and returns the resul of its computation as `Either[String, Double]`. 
The string can contain the following symbols:
- Digits `0-9`
- Point: `.` - A single point is interpreted as `0`, `.2` is interpreted as `0.2`, `3.2` is interpreted as expected.
- Simple operators: `+`, `-`, `/`, `*` - they work mostly as expected, the order of computations is maintained (`1 + 2 * 3 = 7`). 
  If you want to construct negative value for anything except numbers and constants, you should additionally wrap the negation
  in parentheses (`-lg 100` is incorrect and will not work when passed as an operand, `(-lg 100)` is the correct way to construct
  a negative value)
- Special operators: `ln ...`, `lg ...`, `log...(...)`, `... ^ ...`, `sin ...`, `cos ...`, `... !` - in all of these cases `(...)` is a 
  placeholder for an expression wrapped in parentheses, and `...` without parentheses around is a placeholder for either
  non-empty sequence of digits or an expression wrapped in parentheses (for example, `log(10)(...)` and `log 10 (...)` are 
  both valid, but `log 10 10` and `log e 10` are not).
- Parentheses: `(`, `)` - if the sequence is incorrect, the calculator will return an error. If `(` follows a digit or an expression
  wrapped in parentheses - the calculator will implicitly consider that as multiplication (`2(2 + 3)` = `2 * (2 + 3)`)
- Whitespaces - they are not required and don't affect the computation in any way (`2 2 2 * 3` = `222*3`)
- Constants: `E`, `Pi` or `P` - case-insensitive. If a constant is preceded and/or followed by a number, the calculator 
  will implicitly consider that as multiplication (`222EP11` = `222 * E * P * 11`)
