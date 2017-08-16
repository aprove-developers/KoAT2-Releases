open Batteries

(** Provides default implementations of an ID *)
   
(** An ID represented by a string *)
module StringID : PolyTypes.ID

(** An ID which should distinct prevariables and postvariables by type *)
module PrePostID : PolyTypes.ID
