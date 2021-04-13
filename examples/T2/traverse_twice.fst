model main {
  var A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1, W1, X1, Y1, Z1, A2, B2, C2, D2, E2, F2, G2, H2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2;
  states f13, f20, f16, f17, f11, f7, f4, f5, f22, f21;
  transition t0 := {
    from   := f13;
    to     := f20;
    guard  := A >= B && A >= 0 && W1 > 1 && 0 > C && X1 >= W1 && D >= W1 && E = 0;
    action := F' = W1, B' = Y1, C' = Z1, A' = D, G' = A2, H' = B2, I' = C2, J' = D2, K' = E2, L' = C, M' = C, N' = C, O' = X1, E' = 0, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t1 := {
    from   := f13;
    to     := f20;
    guard  := A >= B && A >= 0 && W1 > 1 && C > 0 && X1 >= W1 && D >= W1 && E = 0;
    action := F' = W1, B' = Y1, C' = Z1, A' = D, G' = A2, H' = B2, I' = C2, J' = D2, K' = E2, L' = C, M' = C, N' = C, O' = X1, E' = 0, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t2 := {
    from   := f13;
    to     := f13;
    guard  := B > A && A >= 0;
    action := C' = K, A' = 1 + A, J' = K, K' = W1, P' = Y1, Q' = A, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t3 := {
    from   := f16;
    to     := f20;
    guard  := A >= 0 && W1 > 1 && 0 > N && B2 >= W1 && 0 > Y1 && E = 1;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1, R' = Z1, S' = 1 + D, T' = A2, U' = D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t4 := {
    from   := f16;
    to     := f20;
    guard  := A >= 0 && W1 > 1 && 0 > N && B2 >= W1 && Y1 > 0 && E = 1;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1, R' = Z1, S' = 1 + D, T' = A2, U' = D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t5 := {
    from   := f16;
    to     := f20;
    guard  := A >= 0 && W1 > 1 && N > 0 && B2 >= W1 && 0 > Y1 && E = 1;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1, R' = Z1, S' = 1 + D, T' = A2, U' = D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t6 := {
    from   := f16;
    to     := f20;
    guard  := A >= 0 && W1 > 1 && N > 0 && B2 >= W1 && Y1 > 0 && E = 1;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1, R' = Z1, S' = 1 + D, T' = A2, U' = D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t7 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && 0 > N && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t8 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && 0 > N && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t9 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && 0 > N && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t10 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && 0 > N && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t11 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && N > 0 && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t12 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && N > 0 && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t13 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && N > 0 && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t14 := {
    from   := f17;
    to     := f20;
    guard  := U >= 0 && W1 > 1 && N > 0 && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, V' = Z1, W' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t15 := {
    from   := f20;
    to     := f11;
    guard  := E >= 0 && D >= 0 && W1 > 1 && 0 > L && X = E && N = 0 && Y = 0;
    action := F' = W1, M' = L, N' = L, Y' = 0, X' = E, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t16 := {
    from   := f20;
    to     := f11;
    guard  := E >= 0 && D >= 0 && W1 > 1 && L > 0 && X = E && N = 0 && Y = 0;
    action := F' = W1, M' = L, N' = L, Y' = 0, X' = E, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t17 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && 0 > B2 && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t18 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && 0 > B2 && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t19 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && 0 > B2 && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t20 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && 0 > B2 && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t21 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && B2 > 0 && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t22 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && B2 > 0 && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t23 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && B2 > 0 && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t24 := {
    from   := f20;
    to     := f20;
    guard  := E >= 0 && D >= 0 && W1 > 1 && B2 > 0 && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E' = 1 + E, D' = -1 + D, R' = Z1, Z' = N, A1' = A2, B1' = 1 + E, C1' = -1 + D, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t25 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && 0 > N && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t26 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && 0 > N && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t27 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && 0 > N && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t28 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && 0 > N && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t29 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && N > 0 && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t30 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && N > 0 && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t31 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && N > 0 && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t32 := {
    from   := f7;
    to     := f11;
    guard  := D1 >= 0 && W1 > 1 && N > 0 && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, E1' = Z1, F1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t33 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && 0 > B2 && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t34 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && 0 > B2 && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t35 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && 0 > B2 && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t36 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && 0 > B2 && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t37 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && B2 > 0 && 0 > Y1 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t38 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && B2 > 0 && 0 > Y1 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t39 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && B2 > 0 && Y1 > 0 && 0 > A2;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t40 := {
    from   := f11;
    to     := f11;
    guard  := Y >= 0 && X >= 0 && W1 > 1 && B2 > 0 && Y1 > 0 && A2 > 0;
    action := F' = W1, L' = Y1, M' = Y1, R' = Z1, Y' = 1 + Y, X' = -1 + X, G1' = N, H1' = A2, I1' = 1 + Y, J1' = -1 + X, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t41 := {
    from   := f4;
    to     := f5;
    guard  := Z1 > K1 && L1 >= 0 && W1 > 1 && Y1 > Z1 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t42 := {
    from   := f4;
    to     := f5;
    guard  := Z1 > K1 && L1 >= 0 && W1 > 1 && Y1 > Z1 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t43 := {
    from   := f4;
    to     := f5;
    guard  := Z1 > K1 && L1 >= 0 && W1 > 1 && Z1 > Y1 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t44 := {
    from   := f4;
    to     := f5;
    guard  := Z1 > K1 && L1 >= 0 && W1 > 1 && Z1 > Y1 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t45 := {
    from   := f4;
    to     := f5;
    guard  := K1 > Z1 && L1 >= 0 && W1 > 1 && Y1 > Z1 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t46 := {
    from   := f4;
    to     := f5;
    guard  := K1 > Z1 && L1 >= 0 && W1 > 1 && Y1 > Z1 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t47 := {
    from   := f4;
    to     := f5;
    guard  := K1 > Z1 && L1 >= 0 && W1 > 1 && Z1 > Y1 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t48 := {
    from   := f4;
    to     := f5;
    guard  := K1 > Z1 && L1 >= 0 && W1 > 1 && Z1 > Y1 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t49 := {
    from   := f4;
    to     := f22;
    guard  := L1 >= 0 && 0 > Z1 && W1 > 1 && M1 = K1;
    action := F' = W1, H' = Y1, L' = Z1, N1' = A2, O1' = B2, P1' = C2, M1' = D2, K1' = E2, Q1' = X1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t50 := {
    from   := f4;
    to     := f22;
    guard  := L1 >= 0 && Z1 > 0 && W1 > 1 && M1 = K1;
    action := F' = W1, H' = Y1, L' = Z1, N1' = A2, O1' = B2, P1' = C2, M1' = D2, K1' = E2, Q1' = X1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t51 := {
    from   := f5;
    to     := f5;
    guard  := A2 > K1 && R1 >= 0 && W1 > 1 && Y1 > A2 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t52 := {
    from   := f5;
    to     := f5;
    guard  := A2 > K1 && R1 >= 0 && W1 > 1 && Y1 > A2 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t53 := {
    from   := f5;
    to     := f5;
    guard  := A2 > K1 && R1 >= 0 && W1 > 1 && A2 > Y1 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t54 := {
    from   := f5;
    to     := f5;
    guard  := A2 > K1 && R1 >= 0 && W1 > 1 && A2 > Y1 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t55 := {
    from   := f5;
    to     := f5;
    guard  := K1 > A2 && R1 >= 0 && W1 > 1 && Y1 > A2 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t56 := {
    from   := f5;
    to     := f5;
    guard  := K1 > A2 && R1 >= 0 && W1 > 1 && Y1 > A2 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t57 := {
    from   := f5;
    to     := f5;
    guard  := K1 > A2 && R1 >= 0 && W1 > 1 && A2 > Y1 && 0 > Y1 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t58 := {
    from   := f5;
    to     := f5;
    guard  := K1 > A2 && R1 >= 0 && W1 > 1 && A2 > Y1 && Y1 > 0 && M1 = 0;
    action := F' = W1, L' = Y1, R' = Z1, N1' = Y1, O1' = 0, P1' = Y1, M1' = 0, Q1' = K1, R1' = -1 + R1, S1' = -1 + R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t59 := {
    from   := f5;
    to     := f22;
    guard  := W1 > 1 && R1 >= 0 && M1 = K1;
    action := F' = W1, H' = Y1, N1' = Z1, O1' = A2, P1' = B2, M1' = C2, K1' = D2, Q1' = E2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t60 := {
    from   := f21;
    to     := f13;
    guard  := Y1 > 1;
    action := T1' = W1, F' = Y1, B' = Y1, C' = Z1, A' = 2, I' = Z1, J' = Z1, K' = A2, U1' = B2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t61 := {
    from   := f17;
    to     := f11;
    guard  := B2 > 1 && W1 > 1 && U >= 0 && 0 > L && 0 > Y1 && N = 0 && E = 1 && Y = 1;
    action := F' = W1, L' = Y1, M' = Y1, N' = L, E' = X + 1, R' = Z1, Y' = 1, D1' = X, V1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t62 := {
    from   := f17;
    to     := f11;
    guard  := B2 > 1 && W1 > 1 && U >= 0 && 0 > L && Y1 > 0 && N = 0 && E = 1 && Y = 1;
    action := F' = W1, L' = Y1, M' = Y1, N' = L, E' = X + 1, R' = Z1, Y' = 1, D1' = X, V1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t63 := {
    from   := f17;
    to     := f11;
    guard  := B2 > 1 && W1 > 1 && U >= 0 && L > 0 && 0 > Y1 && N = 0 && E = 1 && Y = 1;
    action := F' = W1, L' = Y1, M' = Y1, N' = L, E' = X + 1, R' = Z1, Y' = 1, D1' = X, V1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t64 := {
    from   := f17;
    to     := f11;
    guard  := B2 > 1 && W1 > 1 && U >= 0 && L > 0 && Y1 > 0 && N = 0 && E = 1 && Y = 1;
    action := F' = W1, L' = Y1, M' = Y1, N' = L, E' = X + 1, R' = Z1, Y' = 1, D1' = X, V1' = A2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t65 := {
    from   := f21;
    to     := f22;
    guard  := 0 >= F2 && 0 >= G2 && 0 >= H2 && 0 >= Y1 && 0 >= I2;
    action := T1' = W1, F' = Y1, B' = Z1, C' = A2, A' = B2, G' = C2, H' = D2, I' = E2, J' = X1, K' = J2, L' = 0, M' = K2, N' = L2, N1' = M2, O1' = N2, P1' = O2, M1' = P2, K1' = Q2, Q1' = R2, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t66 := {
    from   := f7;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && D1 >= 0 && L > 0 && 0 > L && N = 0 && Y = 1;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t67 := {
    from   := f7;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && D1 >= 0 && L > 0 && N = 0 && Y = 1;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t68 := {
    from   := f7;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && D1 >= 0 && 0 > L && N = 0 && Y = 1;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t69 := {
    from   := f7;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && D1 >= 0 && 0 > L && L > 0 && N = 0 && Y = 1;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t70 := {
    from   := f11;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && X >= 0 && Y >= 0 && L > 0 && 0 > L && N = 0;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t71 := {
    from   := f11;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && X >= 0 && Y >= 0 && L > 0 && N = 0;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t72 := {
    from   := f11;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && X >= 0 && Y >= 0 && 0 > L && N = 0;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
  transition t73 := {
    from   := f11;
    to     := f5;
    guard  := A2 > 1 && W1 > 1 && X >= 0 && Y >= 0 && 0 > L && L > 0 && N = 0;
    action := F' = W1, M' = Y1, N' = Z1, Y' = R1 + 1, N1' = L, O1' = 0, P1' = L, M1' = 0, K1' = L, Q1' = L, L1' = R1, W1' = ?, X1' = ?, Y1' = ?, Z1' = ?, A2' = ?, B2' = ?, C2' = ?, D2' = ?, E2' = ?, F2' = ?, G2' = ?, H2' = ?, I2' = ?, J2' = ?, K2' = ?, L2' = ?, M2' = ?, N2' = ?, O2' = ?, P2' = ?, Q2' = ?, R2' = ?;
  };
}
strategy dumb {
  Region init := { state = f21 };
}
