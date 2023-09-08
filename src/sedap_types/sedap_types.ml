module Id = Id

module Make (Id : Id.S) = struct
  module Exec_map = Exec_map.Make (Id)
  module Unify_map = Unify_map.Make (Id)

  type debug_proc_state = {
    exec_map : Exec_map.Packaged.t; [@key "execMap"]
    lifted_exec_map : Exec_map.Packaged.t option; [@key "liftedExecMap"]
    current_cmd_id : Id.t; [@key "currentCmdId"]
    unifys: Exec_map.Unification.t list;
    proc_name: string; [@key "procName"]
  }
  [@@deriving yojson]

  let procs_to_yosjon procs : Yojson.Safe.t =
    let procs =
      procs
      |> List.map (fun (k, v) -> (k, debug_proc_state_to_yojson v))
    in
    `Assoc procs

  let procs_of_yojson json =
    let ( let++ ) f o = Result.map o f in
    let map_results f l =
      let rec aux acc = function
        | [] -> Ok (List.rev acc)
        | x :: xs -> (
            match f x with
            | Ok x -> aux (x :: acc) xs
            | Error e -> Error e)
      in
      aux [] l
    in
    let procs =
      json |> Yojson.Safe.Util.to_assoc
      |> map_results (fun (k, v) ->
             let++ v' = debug_proc_state_of_yojson v in
             (k, v'))
    in
    procs

  type debug_state = {
    main_proc : string; [@key "mainProc"]
    current_proc : string; [@key "currentProc"]
    procs : (string * debug_proc_state) list;
  }
  [@@deriving yojson]

  include Debug_protocol

(** Extension of the Launch command to include custom arguments *)
module Launch_command = struct
  let type_ = Launch_command.type_

  module Arguments = struct
    type t = {
      program : string;
      stop_on_entry : bool; [@default false] [@key "stopOnEntry"]
      procedure_name : string option; [@default None] [@key "procedureName"]
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end

module Debug_state_update_event = struct
  let type_ = "debugStateUpdate"

  module Payload = struct
    type t = debug_state [@@deriving yojson]
  end
end

module Debugger_state_command = struct
  let type_ = "debuggerState"

  module Arguments = struct
    type t = Empty_dict.t [@@deriving yojson]
  end

  module Result = struct
    type t = debug_state [@@deriving yojson]
  end
end

module Unification_command = struct
  let type_ = "unification"

  module Arguments = struct
    type t = { id : Id.t } [@@deriving yojson]
  end

  module Result = struct
    type t = {
      unify_id : Id.t; [@key "unifyId"]
      unify_map : Unify_map.t; [@key "unifyMap"]
    }
    [@@deriving yojson, make]
  end
end

module Jump_command = struct
  let type_ = "jump"

  module Arguments = struct
    type t = { id : Id.t } [@@deriving yojson]
  end

  module Result = struct
    type t = { success : bool; err : string option [@default None] }
    [@@deriving make, yojson]
  end
end

module Step_specific_command = struct
  let type_ = "stepSpecific"

  module Arguments = struct
    type t = {
      prev_id : Id.t; [@key "prevId"]
      branch_case : Exec_map.Packaged.branch_case option; [@key "branchCase"]
    }
    [@@deriving yojson]
  end

  module Result = struct
    type t = { success : bool; err : string option [@default None] }
    [@@deriving make, yojson]
  end
end

module Start_proc_command = struct
  let type_ = "startProc"

  module Arguments = struct
    type t = { proc_name : string [@key "procName"] } [@@deriving yojson]
  end

  module Result = struct
    type t = { success : bool; err : string option [@default None] }
    [@@deriving make, yojson]
  end
end
end