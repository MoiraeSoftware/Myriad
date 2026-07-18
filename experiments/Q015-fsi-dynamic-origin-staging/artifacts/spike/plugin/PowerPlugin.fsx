// This file is the "general implementation" under test. It is never #load-ed, #r-ed, or referenced
// by name anywhere in Program.fs - Program.fs reads this file's TEXT at runtime via File.ReadAllText
// and hands that text to a live FsiEvaluationSession. The host program's own compiled code has no
// knowledge of the identifier `power`, `Target`, or this function's shape at ITS compile time.
module Target =
    [<ReflectedDefinition>]
    let rec power (n: int) (x: float) : float =
        if n = 0 then 1.0 else x * power (n - 1) x
