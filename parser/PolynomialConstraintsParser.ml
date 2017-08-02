
module Make

# 1 "PolynomialConstraintsParser.mly"
          (C : ConstraintTypes.ParseablePolynomialConstraints)
# 7 "PolynomialConstraintsParser.ml"

= struct
  
  module MenhirBasics = struct
    
    exception Error
    
    type token = 
      | UINT of (
# 4 "PolynomialConstraintsParser.mly"
       (int)
# 19 "PolynomialConstraintsParser.ml"
    )
      | TIMES
      | RPAR
      | POW
      | PLUS
      | NEQ
      | MINUS
      | LPAR
      | LESSTHAN
      | LESSEQUAL
      | ID of (
# 3 "PolynomialConstraintsParser.mly"
       (string)
# 33 "PolynomialConstraintsParser.ml"
    )
      | GREATERTHAN
      | GREATEREQUAL
      | EQUAL
      | EOF
      | COMMA
      | AND
    
  end
  
  include MenhirBasics
  
  let _eRR =
    MenhirBasics.Error
  
  type _menhir_env = {
    _menhir_lexer: Lexing.lexbuf -> token;
    _menhir_lexbuf: Lexing.lexbuf;
    _menhir_token: token;
    mutable _menhir_error: bool
  }
  
  and _menhir_state = 
    | MenhirState44
    | MenhirState37
    | MenhirState33
    | MenhirState31
    | MenhirState29
    | MenhirState27
    | MenhirState25
    | MenhirState23
    | MenhirState20
    | MenhirState14
    | MenhirState12
    | MenhirState9
    | MenhirState3
    | MenhirState2
    | MenhirState0
  
  let rec _menhir_fail : unit -> 'a =
    fun () ->
      Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
      assert false
  
  and _menhir_goto_separated_nonempty_list_COMMA_atom_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (C.atom list) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      match _menhir_s with
      | MenhirState37 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (x : (C.atom list)) = _v in
          let _v : (C.atom list) = 
# 130 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
    ( x )
# 88 "PolynomialConstraintsParser.ml"
           in
          _menhir_goto_loption_separated_nonempty_list_COMMA_atom__ _menhir_env _menhir_stack _menhir_s _v
      | MenhirState44 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (xs : (C.atom list)) = _v in
          let (_menhir_stack, _menhir_s, (x : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 98 "PolynomialConstraintsParser.ml"
          ))) = _menhir_stack in
          let _2 = () in
          let _v : (C.atom list) = 
# 217 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 104 "PolynomialConstraintsParser.ml"
           in
          _menhir_goto_separated_nonempty_list_COMMA_atom_ _menhir_env _menhir_stack _menhir_s _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 113 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState20 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (at : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 129 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 24 "PolynomialConstraintsParser.mly"
       (C.atom)
# 135 "PolynomialConstraintsParser.ml"
              ) = 
# 46 "PolynomialConstraintsParser.mly"
                 ( at )
# 139 "PolynomialConstraintsParser.ml"
               in
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_1 : (
# 24 "PolynomialConstraintsParser.mly"
       (C.atom)
# 146 "PolynomialConstraintsParser.ml"
              )) = _v in
              Obj.magic _1
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState44 | MenhirState37 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
          | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (x : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 182 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _v : (C.atom list) = 
# 215 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 187 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_separated_nonempty_list_COMMA_atom_ _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | _ ->
          _menhir_fail ()
  
  and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 202 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9
  
  and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 224 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12
  
  and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 246 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14
  
  and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 268 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      match _menhir_s with
      | MenhirState3 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s), _, (ex : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 289 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _3 = () in
              let _1 = () in
              let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 296 "PolynomialConstraintsParser.ml"
              ) = 
# 75 "PolynomialConstraintsParser.mly"
                  ( ex )
# 300 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState9 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let ((_menhir_stack, _menhir_s, (ex1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 317 "PolynomialConstraintsParser.ml"
          ))), _, (ex2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 321 "PolynomialConstraintsParser.ml"
          ))) = _menhir_stack in
          let _2 = () in
          let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 327 "PolynomialConstraintsParser.ml"
          ) = 
# 81 "PolynomialConstraintsParser.mly"
           ( C.PolynomialConstraintsAtoms_.Polynomial_.mul ex1 ex2 )
# 331 "PolynomialConstraintsParser.ml"
           in
          _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF | EQUAL | GREATEREQUAL | GREATERTHAN | LESSEQUAL | LESSTHAN | MINUS | NEQ | PLUS | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (ex1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 346 "PolynomialConstraintsParser.ml"
              ))), _, (ex2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 350 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 356 "PolynomialConstraintsParser.ml"
              ) = 
# 79 "PolynomialConstraintsParser.mly"
           ( C.PolynomialConstraintsAtoms_.Polynomial_.add ex1 ex2 )
# 360 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState14 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF | EQUAL | GREATEREQUAL | GREATERTHAN | LESSEQUAL | LESSTHAN | MINUS | NEQ | PLUS | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (ex1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 381 "PolynomialConstraintsParser.ml"
              ))), _, (ex2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 385 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 391 "PolynomialConstraintsParser.ml"
              ) = 
# 83 "PolynomialConstraintsParser.mly"
           ( C.PolynomialConstraintsAtoms_.Polynomial_.add ex1 (C.PolynomialConstraintsAtoms_.Polynomial_.neg ex2) )
# 395 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState2 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF | EQUAL | GREATEREQUAL | GREATERTHAN | LESSEQUAL | LESSTHAN | MINUS | NEQ | PLUS | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s), _, (ex : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 416 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _1 = () in
              let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 422 "PolynomialConstraintsParser.ml"
              ) = 
# 77 "PolynomialConstraintsParser.mly"
           ( C.PolynomialConstraintsAtoms_.Polynomial_.neg ex )
# 426 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState0 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (poly : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 446 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 28 "PolynomialConstraintsParser.mly"
       (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 452 "PolynomialConstraintsParser.ml"
              ) = 
# 49 "PolynomialConstraintsParser.mly"
                                      ( poly )
# 456 "PolynomialConstraintsParser.ml"
               in
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_1 : (
# 28 "PolynomialConstraintsParser.mly"
       (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 463 "PolynomialConstraintsParser.ml"
              )) = _v in
              Obj.magic _1
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState44 | MenhirState37 | MenhirState20 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | EQUAL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
          | GREATEREQUAL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
          | GREATERTHAN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
          | LESSEQUAL ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
          | LESSTHAN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | NEQ ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _tok = _menhir_env._menhir_token in
              (match _tok with
              | ID _v ->
                  _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
              | LPAR ->
                  _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
              | MINUS ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
              | UINT _v ->
                  _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
              | _ ->
                  assert (not _menhir_env._menhir_error);
                  _menhir_env._menhir_error <- true;
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState23 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (p1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 613 "PolynomialConstraintsParser.ml"
              ))), _, (p2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 617 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 623 "PolynomialConstraintsParser.ml"
              ) = 
# 55 "PolynomialConstraintsParser.mly"
            (C.PolynomialConstraintsAtoms_.mk_neq p1 p2)
# 627 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState25 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (p1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 652 "PolynomialConstraintsParser.ml"
              ))), _, (p2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 656 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 662 "PolynomialConstraintsParser.ml"
              ) = 
# 61 "PolynomialConstraintsParser.mly"
            (C.PolynomialConstraintsAtoms_.mk_lt p1 p2)
# 666 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState27 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (p1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 691 "PolynomialConstraintsParser.ml"
              ))), _, (p2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 695 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 701 "PolynomialConstraintsParser.ml"
              ) = 
# 63 "PolynomialConstraintsParser.mly"
            (C.PolynomialConstraintsAtoms_.mk_le p1 p2)
# 705 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState29 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (p1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 730 "PolynomialConstraintsParser.ml"
              ))), _, (p2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 734 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 740 "PolynomialConstraintsParser.ml"
              ) = 
# 57 "PolynomialConstraintsParser.mly"
            (C.PolynomialConstraintsAtoms_.mk_gt p1 p2)
# 744 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState31 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (p1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 769 "PolynomialConstraintsParser.ml"
              ))), _, (p2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 773 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 779 "PolynomialConstraintsParser.ml"
              ) = 
# 59 "PolynomialConstraintsParser.mly"
            (C.PolynomialConstraintsAtoms_.mk_ge p1 p2)
# 783 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | MenhirState33 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          match _tok with
          | MINUS ->
              _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
          | PLUS ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | COMMA | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (p1 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 808 "PolynomialConstraintsParser.ml"
              ))), _, (p2 : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 812 "PolynomialConstraintsParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 32 "PolynomialConstraintsParser.mly"
      (C.atom)
# 818 "PolynomialConstraintsParser.ml"
              ) = 
# 53 "PolynomialConstraintsParser.mly"
            (C.PolynomialConstraintsAtoms_.mk_eq p1 p2)
# 822 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  
  and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      match _menhir_s with
      | MenhirState44 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState37 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          raise _eRR
      | MenhirState33 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState31 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState29 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState27 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState25 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState23 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState20 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          raise _eRR
      | MenhirState14 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState9 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState3 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState2 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
      | MenhirState0 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          raise _eRR
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_atom__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (C.atom list) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_stack = Obj.magic _menhir_stack in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (xs0 : (C.atom list)) = _v in
      let _v : (
# 34 "PolynomialConstraintsParser.mly"
      (C.t)
# 901 "PolynomialConstraintsParser.ml"
      ) = let constr =
        let xs = xs0 in
        
# 206 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
    ( xs )
# 907 "PolynomialConstraintsParser.ml"
        
      in
      
# 43 "PolynomialConstraintsParser.mly"
                (C.mk constr)
# 913 "PolynomialConstraintsParser.ml"
       in
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      let _menhir_stack = Obj.magic _menhir_stack in
      assert (not _menhir_env._menhir_error);
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | EOF ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, (constr : (
# 34 "PolynomialConstraintsParser.mly"
      (C.t)
# 926 "PolynomialConstraintsParser.ml"
          ))) = _menhir_stack in
          let _2 = () in
          let _v : (
# 26 "PolynomialConstraintsParser.mly"
       (C.t)
# 932 "PolynomialConstraintsParser.ml"
          ) = 
# 39 "PolynomialConstraintsParser.mly"
                                         (constr)
# 936 "PolynomialConstraintsParser.ml"
           in
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_1 : (
# 26 "PolynomialConstraintsParser.mly"
       (C.t)
# 943 "PolynomialConstraintsParser.ml"
          )) = _v in
          Obj.magic _1
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  
  and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "PolynomialConstraintsParser.mly"
       (int)
# 956 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (c : (
# 4 "PolynomialConstraintsParser.mly"
       (int)
# 964 "PolynomialConstraintsParser.ml"
      )) = _v in
      let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 969 "PolynomialConstraintsParser.ml"
      ) = 
# 73 "PolynomialConstraintsParser.mly"
                  ( C.PolynomialConstraintsAtoms_.Polynomial_.from_constant_int c )
# 973 "PolynomialConstraintsParser.ml"
       in
      _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
  
  and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2
  
  and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      let _menhir_stack = (_menhir_stack, _menhir_s) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3
  
  and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "PolynomialConstraintsParser.mly"
       (string)
# 1018 "PolynomialConstraintsParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (v : (
# 3 "PolynomialConstraintsParser.mly"
       (string)
# 1026 "PolynomialConstraintsParser.ml"
      )) = _v in
      let _v : (C.PolynomialConstraintsAtoms_.Polynomial_.t) = 
# 67 "PolynomialConstraintsParser.mly"
                  ( C.PolynomialConstraintsAtoms_.Polynomial_.from_var_string v )
# 1031 "PolynomialConstraintsParser.ml"
       in
      let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
      let _menhir_stack = Obj.magic _menhir_stack in
      assert (not _menhir_env._menhir_error);
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | POW ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_env = _menhir_discard _menhir_env in
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | UINT _v ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_env = _menhir_discard _menhir_env in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (c : (
# 4 "PolynomialConstraintsParser.mly"
       (int)
# 1050 "PolynomialConstraintsParser.ml"
              )) = _v in
              let (_menhir_stack, _menhir_s, (v : (C.PolynomialConstraintsAtoms_.Polynomial_.t))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 1057 "PolynomialConstraintsParser.ml"
              ) = 
# 85 "PolynomialConstraintsParser.mly"
           ( C.PolynomialConstraintsAtoms_.Polynomial_.pow v c )
# 1061 "PolynomialConstraintsParser.ml"
               in
              _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | COMMA | EOF | EQUAL | GREATEREQUAL | GREATERTHAN | LESSEQUAL | LESSTHAN | MINUS | NEQ | PLUS | RPAR | TIMES ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, (v : (C.PolynomialConstraintsAtoms_.Polynomial_.t))) = _menhir_stack in
          let _v : (
# 30 "PolynomialConstraintsParser.mly"
      (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 1076 "PolynomialConstraintsParser.ml"
          ) = 
# 71 "PolynomialConstraintsParser.mly"
                  ( v )
# 1080 "PolynomialConstraintsParser.ml"
           in
          _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  
  and _menhir_discard : _menhir_env -> _menhir_env =
    fun _menhir_env ->
      let lexer = _menhir_env._menhir_lexer in
      let lexbuf = _menhir_env._menhir_lexbuf in
      let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }
  
  and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
    fun lexer lexbuf ->
      let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }
  
  and polynomial : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 28 "PolynomialConstraintsParser.mly"
       (C.PolynomialConstraintsAtoms_.Polynomial_.t)
# 1115 "PolynomialConstraintsParser.ml"
  ) =
    fun lexer lexbuf ->
      let _menhir_env = _menhir_init lexer lexbuf in
      Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  
  and polynomialConstraintAtom : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 24 "PolynomialConstraintsParser.mly"
       (C.atom)
# 1139 "PolynomialConstraintsParser.ml"
  ) =
    fun lexer lexbuf ->
      let _menhir_env = _menhir_init lexer lexbuf in
      Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
  
  and polynomialConstraints : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 26 "PolynomialConstraintsParser.mly"
       (C.t)
# 1163 "PolynomialConstraintsParser.ml"
  ) =
    fun lexer lexbuf ->
      let _menhir_env = _menhir_init lexer lexbuf in
      Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
      let _menhir_env = _menhir_discard _menhir_env in
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | ID _v ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
      | LPAR ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
      | MINUS ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37
      | UINT _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
      | EOF ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_s = MenhirState37 in
          let _v : (C.atom list) = 
# 128 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
    ( [] )
# 1185 "PolynomialConstraintsParser.ml"
           in
          _menhir_goto_loption_separated_nonempty_list_COMMA_atom__ _menhir_env _menhir_stack _menhir_s _v
      | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
  
# 219 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
  


# 1197 "PolynomialConstraintsParser.ml"
  
end
