functor WasmAstFun(Pos : sig
                     type pos
                     type sourcemap
                     val fileName : sourcemap -> pos -> string option
                     val lineNo : sourcemap -> pos -> int
                     val colNo : sourcemap -> pos -> int
                   end)
= struct
  type span = Pos.pos * Pos.pos

  (* Memory *)
  type address = LargeInt.int
  type size = address
  type offset = address

  (* Expressions *)

  type var = int * span

  datatype expr =
    (* Constants *)
      I32Const of string * span
    | I64Const of string * span
    | F32Const of string * span
    | F64Const of string * span

    (* Control *)
    | Nop of span
    | Unreachable of span
    | Block of expr list * span
    | Loop of expr list * span
    | Br of var * expr option * span
    | BrIf of var * expr option * expr * span
    | BrTable of var list * var * expr option * expr * span
    | Return of expr option * span
    | If of expr * expr list * expr list * span
    | Select of expr * expr * expr * span
    | Call of var * expr list * span
    | CallImport of var * expr list * span
    | CallIndirect of var * expr * expr list * span

    (* Locals *)
    | GetLocal of var * span
    | SetLocal of var * expr * span

    (* Memory access *)
    | I32Load of offset * int * expr * span
    | I64Load of offset * int * expr * span
    | F32Load of offset * int * expr * span
    | F64Load of offset * int * expr * span
    | I32Store of offset * int * expr * expr * span
    | I64Store of offset * int * expr * expr * span
    | F32Store of offset * int * expr * expr * span
    | F64Store of offset * int * expr * expr * span
    | I32Load8S of offset * int * expr * span
    | I32Load8U of offset * int * expr * span
    | I32Load16S of offset * int * expr * span
    | I32Load16U of offset * int * expr * span
    | I32Load32S of offset * int * expr * span
    | I32Load32U of offset * int * expr * span
    | I64Load8S of offset * int * expr * span
    | I64Load8U of offset * int * expr * span
    | I64Load16S of offset * int * expr * span
    | I64Load16U of offset * int * expr * span
    | I64Load32S of offset * int * expr * span
    | I64Load32U of offset * int * expr * span
    | I32Store8 of offset * int * expr * expr * span
    | I32Store16 of offset * int * expr * expr * span
    | I32Store32 of offset * int * expr * expr * span
    | I64Store8 of offset * int * expr * expr * span
    | I64Store16 of offset * int * expr * expr * span
    | I64Store32 of offset * int * expr * expr * span

    (* Unary arithmetic *)
    | I32Clz of expr * span
    | I32Ctz of expr * span
    | I32Popcnt of expr * span
    | I64Clz of expr * span
    | I64Ctz of expr * span
    | I64Popcnt of expr * span
    | F32Neg of expr * span
    | F32Abs of expr * span
    | F32Sqrt of expr * span
    | F32Ceil of expr * span
    | F32Floor of expr * span
    | F32Trunc of expr * span
    | F32Nearest of expr * span
    | F64Neg of expr * span
    | F64Abs of expr * span
    | F64Sqrt of expr * span
    | F64Ceil of expr * span
    | F64Floor of expr * span
    | F64Trunc of expr * span
    | F64Nearest of expr * span

    (* Binary arithmetic *)
    | I32Add of expr * expr * span
    | I32Sub of expr * expr * span
    | I32Mul of expr * expr * span
    | I32DivS of expr * expr * span
    | I32DivU of expr * expr * span
    | I32RemS of expr * expr * span
    | I32RemU of expr * expr * span
    | I32And of expr * expr * span
    | I32Or of expr * expr * span
    | I32Xor of expr * expr * span
    | I32Shl of expr * expr * span
    | I32ShrS of expr * expr * span
    | I32ShrU of expr * expr * span
    | I32Rotl of expr * expr * span
    | I32Rotr of expr * expr * span
    | I64Add of expr * expr * span
    | I64Sub of expr * expr * span
    | I64Mul of expr * expr * span
    | I64DivS of expr * expr * span
    | I64DivU of expr * expr * span
    | I64RemS of expr * expr * span
    | I64RemU of expr * expr * span
    | I64And of expr * expr * span
    | I64Or of expr * expr * span
    | I64Xor of expr * expr * span
    | I64Shl of expr * expr * span
    | I64ShrS of expr * expr * span
    | I64ShrU of expr * expr * span
    | I64Rotl of expr * expr * span
    | I64Rotr of expr * expr * span
    | F32Add of expr * expr * span
    | F32Sub of expr * expr * span
    | F32Mul of expr * expr * span
    | F32Div of expr * expr * span
    | F32Min of expr * expr * span
    | F32Max of expr * expr * span
    | F32Copysign of expr * expr * span
    | F64Add of expr * expr * span
    | F64Sub of expr * expr * span
    | F64Mul of expr * expr * span
    | F64Div of expr * expr * span
    | F64Min of expr * expr * span
    | F64Max of expr * expr * span
    | F64Copysign of expr * expr * span

    (* Predicates *)
    | I32Eqz of expr * span
    | I64Eqz of expr * span

    (* Comparisons *)
    | I32Eq of expr * expr * span
    | I32Ne of expr * expr * span
    | I32LtS of expr * expr * span
    | I32LtU of expr * expr * span
    | I32LeS of expr * expr * span
    | I32LeU of expr * expr * span
    | I32GtS of expr * expr * span
    | I32GtU of expr * expr * span
    | I32GeS of expr * expr * span
    | I32GeU of expr * expr * span
    | I64Eq of expr * expr * span
    | I64Ne of expr * expr * span
    | I64LtS of expr * expr * span
    | I64LtU of expr * expr * span
    | I64LeS of expr * expr * span
    | I64LeU of expr * expr * span
    | I64GtS of expr * expr * span
    | I64GtU of expr * expr * span
    | I64GeS of expr * expr * span
    | I64GeU of expr * expr * span
    | F32Eq of expr * expr * span
    | F32Ne of expr * expr * span
    | F32Lt of expr * expr * span
    | F32Le of expr * expr * span
    | F32Gt of expr * expr * span
    | F32Ge of expr * expr * span
    | F64Eq of expr * expr * span
    | F64Ne of expr * expr * span
    | F64Lt of expr * expr * span
    | F64Le of expr * expr * span
    | F64Gt of expr * expr * span
    | F64Ge of expr * expr * span

    (* Conversions *)
    | I32WrapI64 of expr * span
    | I32TruncSF32 of expr * span
    | I32TruncUF32 of expr * span
    | I32TruncSF64 of expr * span
    | I32TruncUF64 of expr * span
    | I64ExtendSI32 of expr * span
    | I64ExtendUI32 of expr * span
    | I64TruncSF32 of expr * span
    | I64TruncUF32 of expr * span
    | I64TruncSF64 of expr * span
    | I64TruncUF64 of expr * span
    | F32ConvertSI32 of expr * span
    | F32ConvertUI32 of expr * span
    | F32ConvertSI64 of expr * span
    | F32ConvertUI64 of expr * span
    | F32DemoteF64 of expr * span
    | F64ConvertSI32 of expr * span
    | F64ConvertUI32 of expr * span
    | F64ConvertSI64 of expr * span
    | F64ConvertUI64 of expr * span
    | F64PromoteF32 of expr * span
    | I32ReinterpretF32 of expr * span
    | I64ReinterpretF64 of expr * span
    | F32ReinterpretI32 of expr * span
    | F64ReinterpretI64 of expr * span

    (* Host queries *)
    | MemorySize of span
    | GrowMemory of expr * span

  (* Types *)

  datatype valueType = Int32Type | Int64Type | Float32Type | Float64Type
  type exprType = valueType option
  type funcType = { ins : valueType list,
                    out : exprType }

  (* Functions *)

  type func = { ftype : var,
                locals : valueType list,
                body : expr list,
                span : span }

  type segment = { addr : address,
                   data : string }

  type memory = { initial : size,
                  max : size,
                  segments : segment list,
                  span : span }

  datatype export = ExportFunc of string * var
                  | ExportMemory of string

  type import = { itype : var,
                  moduleName : string,
                  funcName : string }

  (* Modules *)

  type module = { memory : memory option,
                  types : funcType list,
                  funcs : func list,
                  start : var option,
                  imports : import list,
                  exports : export list,
                  table : var list,
                  span : span }
end

structure WasmAstEmptyPos = WasmAstFun(struct
  type pos = unit
  type sourcemap = unit
  fun fileName sourcemap pos = NONE
  fun lineNo sourcemap pos = ~1
  fun colNo sourcemap pos = ~1
end)
