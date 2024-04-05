
(* Mini CPU emulator *)

signature MEMWORD =
sig
   type memWord

   val fromInt: int     -> memWord
   val toInt  : memWord -> int
end


structure MemWord :> MEMWORD =
struct
   type memWord = Word32.word
   val fromInt = Word32.fromInt
   val toInt   = Word32.toInt
end


signature REGISTER =
sig
   type register
   structure W : MEMWORD
   type memWord = W.memWord


   val available_regs : register list (* Registers available for normal use *)
   val esp: register                  (* Stack pointer *)
   val ip: register                   (* Program counter *)
   val toString: register -> string   (* string representation of a register *)
   val get: register -> memWord
   val set: register -> memWord -> unit

end


structure Register :> REGISTER =
struct
   datatype register = EAX | EBX | ECX | EDX | ESP | IP
   structure W = MemWord
   type memWord = W.memWord

   val available_regs = ...
   val esp            = ...
   val ip             = ...

   fun toString reg =
      case reg of
         ...

   fun toInt reg =
      case reg of
        IP  => 0
      | ESP => 1
      | EAX => 2
      | EBX => 3
      | ECX => 4
      | EDX => 5

   val regTable = Vector.tabulate (6, fn _ => ref (W.fromInt 0))

   fun getRef reg = ...     (* Retrieves the appropriate reference from the table *)

   fun get reg   = ...      (* Get the value stored in the appropriate reference *)
   fun set reg v = ...      (* Set the value in the appropriate reference *)
end


signature MEMORY = 
sig
   eqtype location
   structure W : MEMWORD
   type memWord = W.memWord
   exception OutOfBounds

   val maxLocation: memWord
   val getLocation: memWord -> location

   val get: location -> memWord
   val set: location -> memWord -> unit
end


structure Memory :> MEMORY = 
struct
   structure W = MemWord
   type memWord = W.memWord

   type location = memWord
   exception OutOfBounds

   val maxLocation = W.fromInt 20000      (* Just an arbitrary number. *)

   (* Initialize memory with 0 *)
   val mem = Vector.tabulate (toInt maxLocation) (fn _ => ref (W.fromInt 0))

   fun getLocation i = ...        (* Need to make sure i is valid memory location *)

   fun getRef loc    = ...
   fun get loc       = ...
   fun set loc value = ... 
end


signature INSTRUCTION =
sig

   exception InvalidInstruction

   structure R : REGISTER
   structure M : MEMORY

   type memWord  = M.memWord
   type location = M.location
   type register = R.register

   datatype operand = Reg of register
                    | Imm of memWord
                    | Mem of { D : memWord,
                               Rb: register,
                               Ri: register option }
   datatype cond  = EQ | NEQ | LE | LEQ
   datatype instr = HALT
                  | MOV of { src: operand, dest: operand }
                  | ADD of { src: operand, dest: operand }
                  | SUB of { src: operand, dest: operand }
                  | CMP of { src: operand, dest: operand }
                  | TST of { src: operand, dest: operand }
                  | OR  of { src: operand, dest: operand }
                  | AND of { src: operand, dest: operand }
                  | NOT of operand
                  | JMP of cond option * location
                  | PUSH of operand
                  | POP of operand
                  | SHL of operand * int
                  | SHR of operand * int

   val encode: instr   -> memWord
   val decode: memWord -> instr
end


structure Instruction :> INSTRUCTION =
struct
   exception InvalidInstruction

   structure R = Register
   structure M = Memory

   type memWord  = M.memWord
   type location = M.location
   type register = R.register
   ...
end


signature CPU =
sig
   structure R : REGISTER
   structure M : MEMORY
   structure I : INSTRUCTION
   structure W : MEMWORD

   type memWord  = M.memWord
   type register = R.register

   type program = I.instr list

   val run: program -> int
end


structure Cpu :> CPU =
struct

   structure R = Register
   structure M = Memory
   structure I = Instruction
   structure W = M.W

   type memWord  = W.memWord
   type location = M.location
   type register = R.register

   type program = I.instr list

   val EAX = hd R.available_regs    (* bit of a hack *)

   (* flags *)
   datatype flags = { ZF: bool, CF: bool, SF: bool, OF: bool }

   val flagsRef = ref { ZG: false, CF: false, SF: false, OF: false }
   fun setFlags fs = flagsRef := fs
   fun getZF () = #ZF (!flagsRef)
   fun getCF () = #CF (!flagsRef)
   fun getSF () = #SF (!flagsRef)
   fun getOF () = #OF (!flagsRef)


   fun load prog       = ... (* program -> unit --- Loads program in memory *)
   fun fetch ()        = ... (* unit -> I.instr --- Loads current instr and increments IP *)
   fun exec instr      = ... (* I.instr -> unit --- Executes instr *)
   fun read_exec_loop  = ... (* unit -> unit    --- Read/execute loop. Stops at HALT *)
   fun reset           = ... (* unit -> unit    --- Reset ip/esp *)

   fun run prog = (
      reset ();
      load prog;
      read_exec_loop ();
      W.toInt (R.get EAX) )
end