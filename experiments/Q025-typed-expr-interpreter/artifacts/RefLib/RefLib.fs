namespace RefLib

type Widget = { Name: string; Count: int }

module Ops =
    let bump (x: int) = x + 10
    let isBig (n: int) = n > 15
