module Sample2

let result =
    let n = 7
    let bumped = RefLib.Ops.bump n
    if RefLib.Ops.isBig bumped then "big" else "small"
