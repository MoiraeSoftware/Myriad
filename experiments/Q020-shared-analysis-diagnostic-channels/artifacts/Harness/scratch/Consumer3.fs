module Consumer3
type C = DiagTP.Provided.Fields<"C:/Users/Dave/Documents/GitHub/Myriad/experiments/Q020-shared-analysis-diagnostic-channels/artifacts/SampleLib/Company.fs", "SampleNs.Company">
let describe (o: obj) : obj =
    let c = C(o)
    c.meta
