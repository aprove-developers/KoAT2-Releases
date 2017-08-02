
module Make

# 1 "PolynomialParser.mly"
          (P : PolyTypes.ParseablePolynomial)
# 7 "PolynomialParser.ml"

= struct
  
  module MenhirBasics = struct
    
    exception Error
    
    type token = 
      | UINT of (
# 4 "PolynomialParser.mly"
       (int)
# 19 "PolynomialParser.ml"
    )
      | TIMES
      | RPAR
      | POW
      | PLUS
      | MINUS
      | LPAR
      | ID of (
# 3 "PolynomialParser.mly"
       (string)
# 30 "PolynomialParser.ml"
    )
      | EOF
    
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
    | MenhirState14
    | MenhirState12
    | MenhirState9
    | MenhirState3
    | MenhirState2
    | MenhirState0
  
  let rec _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 15 "PolynomialParser.mly"
      (P.t)
# 59 "PolynomialParser.ml"
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
# 15 "PolynomialParser.mly"
      (P.t)
# 81 "PolynomialParser.ml"
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
# 15 "PolynomialParser.mly"
      (P.t)
# 103 "PolynomialParser.ml"
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
# 15 "PolynomialParser.mly"
      (P.t)
# 125 "PolynomialParser.ml"
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
# 15 "PolynomialParser.mly"
      (P.t)
# 146 "PolynomialParser.ml"
              ))) = _menhir_stack in
              let _3 = () in
              let _1 = () in
              let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 153 "PolynomialParser.ml"
              ) = 
# 32 "PolynomialParser.mly"
                  ( ex )
# 157 "PolynomialParser.ml"
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
# 15 "PolynomialParser.mly"
      (P.t)
# 174 "PolynomialParser.ml"
          ))), _, (ex2 : (
# 15 "PolynomialParser.mly"
      (P.t)
# 178 "PolynomialParser.ml"
          ))) = _menhir_stack in
          let _2 = () in
          let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 184 "PolynomialParser.ml"
          ) = 
# 38 "PolynomialParser.mly"
           ( P.mul ex1 ex2 )
# 188 "PolynomialParser.ml"
           in
          _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
      | MenhirState12 ->
          let _menhir_stack = Obj.magic _menhir_stack in
          assert (not _menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | TIMES ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
          | EOF | MINUS | PLUS | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (ex1 : (
# 15 "PolynomialParser.mly"
      (P.t)
# 203 "PolynomialParser.ml"
              ))), _, (ex2 : (
# 15 "PolynomialParser.mly"
      (P.t)
# 207 "PolynomialParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 213 "PolynomialParser.ml"
              ) = 
# 36 "PolynomialParser.mly"
           ( P.add ex1 ex2 )
# 217 "PolynomialParser.ml"
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
          | EOF | MINUS | PLUS | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, (ex1 : (
# 15 "PolynomialParser.mly"
      (P.t)
# 238 "PolynomialParser.ml"
              ))), _, (ex2 : (
# 15 "PolynomialParser.mly"
      (P.t)
# 242 "PolynomialParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 248 "PolynomialParser.ml"
              ) = 
# 40 "PolynomialParser.mly"
           ( P.add ex1 (P.neg ex2) )
# 252 "PolynomialParser.ml"
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
          | EOF | MINUS | PLUS | RPAR ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s), _, (ex : (
# 15 "PolynomialParser.mly"
      (P.t)
# 273 "PolynomialParser.ml"
              ))) = _menhir_stack in
              let _1 = () in
              let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 279 "PolynomialParser.ml"
              ) = 
# 34 "PolynomialParser.mly"
           ( P.neg ex )
# 283 "PolynomialParser.ml"
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
          match _tok with
          | EOF ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, (ex : (
# 15 "PolynomialParser.mly"
      (P.t)
# 303 "PolynomialParser.ml"
              ))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 13 "PolynomialParser.mly"
       (P.t)
# 309 "PolynomialParser.ml"
              ) = 
# 20 "PolynomialParser.mly"
                       ( ex )
# 313 "PolynomialParser.ml"
               in
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_1 : (
# 13 "PolynomialParser.mly"
       (P.t)
# 320 "PolynomialParser.ml"
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
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  
  and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s ->
      match _menhir_s with
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
  
  and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "PolynomialParser.mly"
       (int)
# 366 "PolynomialParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (c : (
# 4 "PolynomialParser.mly"
       (int)
# 374 "PolynomialParser.ml"
      )) = _v in
      let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 379 "PolynomialParser.ml"
      ) = 
# 30 "PolynomialParser.mly"
                  ( P.from_constant_int c )
# 383 "PolynomialParser.ml"
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
# 3 "PolynomialParser.mly"
       (string)
# 428 "PolynomialParser.ml"
  ) -> 'ttv_return =
    fun _menhir_env _menhir_stack _menhir_s _v ->
      let _menhir_env = _menhir_discard _menhir_env in
      let _menhir_stack = Obj.magic _menhir_stack in
      let (v : (
# 3 "PolynomialParser.mly"
       (string)
# 436 "PolynomialParser.ml"
      )) = _v in
      let _v : (P.t) = 
# 24 "PolynomialParser.mly"
                  ( P.from_var_string v )
# 441 "PolynomialParser.ml"
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
# 4 "PolynomialParser.mly"
       (int)
# 460 "PolynomialParser.ml"
              )) = _v in
              let (_menhir_stack, _menhir_s, (v : (P.t))) = _menhir_stack in
              let _2 = () in
              let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 467 "PolynomialParser.ml"
              ) = 
# 42 "PolynomialParser.mly"
           ( P.pow v c )
# 471 "PolynomialParser.ml"
               in
              _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (not _menhir_env._menhir_error);
              _menhir_env._menhir_error <- true;
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, _) = _menhir_stack in
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
      | EOF | MINUS | PLUS | RPAR | TIMES ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, (v : (P.t))) = _menhir_stack in
          let _v : (
# 15 "PolynomialParser.mly"
      (P.t)
# 486 "PolynomialParser.ml"
          ) = 
# 28 "PolynomialParser.mly"
                  ( v )
# 490 "PolynomialParser.ml"
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
  
  and polynomial : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 13 "PolynomialParser.mly"
       (P.t)
# 515 "PolynomialParser.ml"
  ) =
    fun lexer lexbuf ->
      let _menhir_env = let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      } in
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
  
# 219 "/home/marcel/.opam/4.04.2/lib/menhir/standard.mly"
  


# 546 "PolynomialParser.ml"
  
end
