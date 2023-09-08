module Make (Id : Id.S) = struct
  type unify_kind =
    | Postcondition
    | Fold
    | FunctionCall
    | Invariant
    | LogicCommand
    | PredicateGuard
  [@@deriving yojson]

  type unify_result = Success | Failure [@@deriving yojson]

  type substitution = {
    assert_id : Id.t; [@key "assertId"]
    subst : string * string;
  }
  [@@deriving yojson]

  (** Represents one step of a unification *)
  type assertion_data = {
    id : Id.t;
        (** The report ID of the assertion in the log database *)
    fold : (Id.t * unify_result) option;
        (** The ID of the fold unification and its result, if this assertion requires a fold *)
    assertion : string;  (** The string representation of this assertion *)
    substitutions : substitution list;
        (** A list of the substitutions learned from this assertion specifically *)
  }
  [@@deriving yojson]

  (** A segment of unification *)
  type unify_seg =
    | Assertion of assertion_data * unify_seg  (** A single assertion *)
    | UnifyResult of Id.t * unify_result
        (** The end of this unification segment *)
  [@@deriving yojson]

  (** A unification map.
    Unification is either a single segment in the normal case, or potentially multiple segments when folding (i.e. when a predicate has multiple cases) *)
  type map = Direct of unify_seg | Fold of unify_seg list [@@deriving yojson]

  type t = unify_kind * map [@@deriving yojson]
end