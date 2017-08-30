
(** {1 Boolean Clauses} *)

open Solver_types

type t = clause

val empty : t (** The empty clause *)

val make : ?tag:int -> atom list -> premise -> t
(** [make atoms premise] creates a clause with the given attributes. *)

val make_arr : ?tag:int -> atom array -> premise -> t
(** [make_arr atoms premise] creates a clause with the
    given attributes.
    Consumes the array. *)

val visited : t -> bool
val mark_visited : t -> unit
val clear_visited : t -> unit

val attached : t -> bool
val set_attached : t -> unit

val atoms : t -> atom array
val activity : t -> float
val name : t -> int
val premise : t -> premise
val get_tag : clause -> int option (** Recover tag from a clause, if any *)

val pp : Term.t CCFormat.printer -> t CCFormat.printer
val debug : Term.t CCFormat.printer -> t CCFormat.printer
val pp_simple : t CCFormat.printer

val pp_dimacs : t CCFormat.printer