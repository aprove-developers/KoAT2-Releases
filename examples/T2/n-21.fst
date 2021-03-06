model main {
  var A, B, C, D;
  states f2, f300, f1, f3;
  transition t0 := {
    from   := f2;
    to     := f2;
    guard  := 29 >= A;
    action := B' = -1 + B, D' = ?;
  };
  transition t1 := {
    from   := f2;
    to     := f300;
    guard  := A > 29;
    action := B' = -1 + B, D' = ?;
  };
  transition t2 := {
    from   := f300;
    to     := f2;
    guard  := 19 >= B;
    action := D' = ?;
  };
  transition t3 := {
    from   := f300;
    to     := f1;
    guard  := B > 19;
    action := C' = D, D' = ?;
  };
  transition t4 := {
    from   := f3;
    to     := f300;
    guard  := true;
    action := D' = ?;
  };
}
strategy dumb {
  Region init := { state = f3 };
}
