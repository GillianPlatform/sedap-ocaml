module Make (Id : Id.S) = struct
  (** A map of execution of a function (symbolic or otherwise).

  This is represented in a tree structure, where each node represents a command.

  Each command node comes in one of these forms:
  - Normal ( {!Cmd} ) - a normal command with one command node as its child.
  - Branching ({!BranchCmd}) - a command with one or more children, with each
    child identified by a branch case type; this represents a command that has
    multiple potential outcomes, depending on the program state (e.g. an
    if/else). Note that this is only interesting in symbolic execution; in
    concrete execution, they will always have exactly one branch case, thus
    functioning similarly to normal command nodes.
  - Final ({!FinalCmd}) - a command with no children; this represents the end
    of an execution branch, either due to errors, or normal termination (i.e.
    when returning from the function).
  
  These nodes also contain some (configurable) set of information - this
  usually includes the relevant report ID from the log database, a
  human-readable representation of the command, and any errors and
  unifications that occurred during executing the command.
  
  The child of a {!Cmd} node may be {!Nothing}, as can any child of a
  {!BranchCmd} node; this represents that there is a command here in the code,
  but it hasn't yet been executed. This is to facilitate the debugger's
  step-by-step behaviour.
  
  A command node may also contain a submap; this embeds another exec map inside
  this command node, either described in full or referred to by name (see
  {!submap}). This is used to, for example, embed the body of a while-loop in
  the while-loop command itself. *)

  (** An exec map / node in an exec map; takes the following type parameters:
  - ['branch_case]: the type that identifies a branch case
  - ['cmd_data]: the type of the data attached to each non-[Nothing] node
  - ['branch_data]: additional data attached to each branch case
  *)
  type ('branch_case, 'cmd_data, 'branch_data) t =
    | Nothing  (** An empty space; represents a command yet to be executed*)
    | Cmd of {
        data : 'cmd_data;
        mutable next : ('branch_case, 'cmd_data, 'branch_data) t;
      }  (** A non-branching command with one next command *)
    | BranchCmd of {
        data : 'cmd_data;
        mutable nexts :
          ('branch_case
          * ('branch_data * ('branch_case, 'cmd_data, 'branch_data) t))
          list;
      }  (** A branching command, with one or more branch cases *)
    | FinalCmd of { data : 'cmd_data }
        (** A command with no subsequent ones, either due to normal termination or an error*)
  [@@deriving yojson]

  (** Data about a unification *)
  module Unification = struct
    type unify_kind =
      | Postcondition
      | Fold
      | FunctionCall
      | Invariant
      | LogicCommand
      | PredicateGuard
    [@@deriving yojson]

    type unify_result = Success | Failure [@@deriving yojson]

    type t = { id : Id.t; kind : unify_kind; result : unify_result }
    [@@deriving yojson]
  end

  type 't submap =
    | NoSubmap
    | Submap of 't  (** Embed an [Exec_map] as a submap *)
    | Proc of string  (** Embed the execution of another proc as a submap *)
  [@@deriving yojson]

  (** An Exec_map to be passed to the debugger frontend and displayed *)
  module Packaged = struct
    type branch_case = {
      kind : string;
      display : string * string;
          (** A friendly name for the branch kind and specific branch case to be displayed to the user *)
      json : Yojson.Safe.t;
          (** The JSON of the original branch case; this can be target language specific *)
    }
    [@@deriving yojson]

    (* Need this to avoid name conflict *)
    (**/**)

    type ('branch_case, 'cmd_data, 'branch_data) _map =
      ('branch_case, 'cmd_data, 'branch_data) t
    [@@deriving yojson]

    (**/**)

    type t = (branch_case, cmd_data, unit) _map

    and cmd_data = {
      ids : Id.t list;
      display : string;
      unifys : Unification.t list;
      errors : string list;
      submap : t submap;
    }
    [@@deriving yojson]

    (** Converts an Exec_map to a packaged Exec_map *)
    let package package_data package_case (map : ('c, 'd, 'bd) _map) : t =
      let rec aux map =
        match map with
        | Nothing -> Nothing
        | Cmd { data; next } ->
            Cmd { data = package_data aux data; next = aux next }
        | BranchCmd { data; nexts } ->
            let data = package_data aux data in
            let all_cases = nexts |> List.map (fun (c, (bd, _)) -> (c, bd)) in
            let nexts =
              nexts
              |> List.map (fun (case, (bdata, next)) ->
                     let case = package_case bdata case all_cases in
                     let next = aux next in
                     (case, ((), next)))
            in
            BranchCmd { data; nexts }
        | FinalCmd { data } ->
            let data = package_data aux data in
            FinalCmd { data }
      in
      aux map
  end
end
