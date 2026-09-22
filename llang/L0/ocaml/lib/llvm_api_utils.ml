open Compiler_llvm

(* --- Emission of LLVM IR via the LLVM builder API --- *)

let context = Llvm.global_context ()
let i32_type = Llvm.i32_type context
let i1_type = Llvm.i1_type context
let i8_ptr_type = Llvm.pointer_type context
let printf_type = Llvm.var_arg_function_type i32_type [| i8_ptr_type |]

(* Bare (no leading '%') names for the LLVM API, as opposed to
   [string_of_label]/[unparse_register] which are for our own printer
   and already include the '%' sigil. *)
let llvm_block_name l = "label_" ^ string_of_int l
let llvm_reg_name r = "r" ^ string_of_int r

(* Pre-create one LLVM basic block per label so that forward branches
   (a [BrI1]/[Br]/[Phi] referring to a label not yet emitted) can be
   resolved regardless of emission order. *)
let create_blocks the_function labels =
  let tbl : (label, Llvm.llbasicblock) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun l ->
      if not (Hashtbl.mem tbl l) then
        Hashtbl.add tbl l (Llvm.append_block context (llvm_block_name l) the_function))
    labels;
  tbl

let block_of_label blocks l =
  match Hashtbl.find_opt blocks l with
  | Some bb -> bb
  | None -> failwith ("Unknown label in LLVM emission: " ^ string_of_label l)

let llvalue_of_result regs = function
  | IConst (x, I32) -> Llvm.const_int (Llvm.i32_type context) x
  | IConst (x, I1) -> Llvm.const_int (Llvm.i1_type context) x
  | Register r ->
    (match Hashtbl.find_opt regs r with
     | Some v -> v
     | None -> failwith ("Unbound register in LLVM emission: " ^ string_of_register r))

(* Emits a single instruction with [builder], recording the produced
   value (if any) under its register in [regs]. *)
let emit_instr builder blocks regs = function
  | Addi32 (r, p1, p2) ->
    let v = Llvm.build_add (llvalue_of_result regs p1) (llvalue_of_result regs p2) (llvm_reg_name r) builder in
    Hashtbl.replace regs r v
  | Subi32 (r, p1, p2) ->
    let v = Llvm.build_sub (llvalue_of_result regs p1) (llvalue_of_result regs p2) (llvm_reg_name r) builder in
    Hashtbl.replace regs r v
  | Muli32 (r, p1, p2) ->
    let v = Llvm.build_mul (llvalue_of_result regs p1) (llvalue_of_result regs p2) (llvm_reg_name r) builder in
    Hashtbl.replace regs r v
  | Divi32 (r, p1, p2) ->
    let v = Llvm.build_sdiv (llvalue_of_result regs p1) (llvalue_of_result regs p2) (llvm_reg_name r) builder in
    Hashtbl.replace regs r v
  | Cmp (r, op, p1, p2, t) ->
    let llvm_op =
      match op with
      | Eq -> Llvm.Icmp.Eq
      | Ne -> Llvm.Icmp.Ne
      | Lt -> Llvm.Icmp.Slt
      | Le -> Llvm.Icmp.Sle
      | Gt -> Llvm.Icmp.Sgt 
      | Ge -> Llvm.Icmp.Sge
  in
    let v = Llvm.build_icmp llvm_op (llvalue_of_result regs p1) (llvalue_of_result regs p2) (llvm_reg_name r) builder in
    Hashtbl.replace regs r v
  (* | _ -> failwith "Unsupported instruction in LLVM emission" *)

(* Emits every instruction of one block, positioning [builder] at its
   pre-created basic block first. *)
let emit_block builder blocks regs (l, instrs) =
  Llvm.position_at_end (block_of_label blocks l) builder;
  List.iter (emit_instr builder blocks regs) instrs

let compile_program_to_llvm ast =
  let the_module = Llvm.create_module context "llang" in
  let printf_fn = Llvm.declare_function "printf" printf_type the_module in
  let fn_type = Llvm.function_type i32_type [||] in
  let the_function = Llvm.define_function "main" fn_type the_module in
  let entry = Llvm.entry_block the_function in
  let main_builder = Llvm.builder context in
  Llvm.position_at_end entry main_builder;
  let ret, tail_label, tail_instrs, blocks = compile_program 0 [] ast in
  let all_blocks = blocks @ [ (tail_label, tail_instrs) ] in
  (* label 0 is the entry block LLVM already gave us for free; only
     pre-create blocks for the other labels. *)
  let llvm_blocks = create_blocks the_function (List.filter (( <> ) 0) (List.map fst all_blocks)) in
  Hashtbl.replace llvm_blocks 0 entry;
  let regs : (register, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16 in
  List.iter (emit_block main_builder llvm_blocks regs) all_blocks;
  Llvm.position_at_end (block_of_label llvm_blocks tail_label) main_builder;
  let ret_val = llvalue_of_result regs ret in
  let fmt_str = Llvm.build_global_stringptr "%d\n" "fmt" main_builder in
  ignore (Llvm.build_call printf_type printf_fn [| fmt_str; ret_val |] "printf_call" main_builder);
  ignore (Llvm.build_ret (Llvm.const_int i32_type 0) main_builder);
  Llvm_analysis.assert_valid_module the_module;
  the_module

let write_bitcode_file (ast : Ast_typed.ast_typed) (filename : string) =
  let the_module = compile_program_to_llvm ast in
  if not (Llvm_bitwriter.write_bitcode_file the_module filename) then
    failwith (Printf.sprintf "Failed to write bitcode file %s" filename)
