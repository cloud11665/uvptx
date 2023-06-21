from pyparsing import *
ParserElement.enable_packrat()


# 4.4. Identifiers
FOLLOWSYM = srange("[a-zA-Z0-9$_]")
IDENTIFIER = (Word(srange("[a-zA-Z]"), FOLLOWSYM) | Word("_$%", FOLLOWSYM))

# 4.5.1. Integer Constants
HEX_LIT = Regex(r"0[xX][0-9a-fA-F]+U?")
OCT_LIT = Regex(r"0[0-8]+U?")
BIN_LIT = Regex(r"0[bB][01]+U?")
DEC_LIT = Regex(r"[1-9]\d*U?")
F32_LIT = Regex(r"0[fF][0-9a-fA-F]{8}")
F64_LIT = Regex(r"0[dD][0-9a-fA-F]{16}")

BOOL_LIT = Literal("True") | Literal("False")
INT_LIT = HEX_LIT | OCT_LIT | BIN_LIT | DEC_LIT
FLT_LIT = F32_LIT | F64_LIT
NUMERIC = BOOL_LIT | INT_LIT | FLT_LIT
INTEGER = pyparsing_common.integer

EXPR = infixNotation(NUMERIC | IDENTIFIER,
    [
        (oneOf('+ - ! ~'),       1, opAssoc.RIGHT),
        (oneOf('(.s64) (.u64)'), 1, opAssoc.RIGHT),
        (oneOf('* / %'),         2, opAssoc.LEFT),
        (oneOf('+ -'),           2, opAssoc.LEFT),
        (oneOf('>> <<'),         2, opAssoc.LEFT),
        (oneOf('< > <= >='),     2, opAssoc.LEFT),
        (oneOf('== !='),         2, opAssoc.LEFT),
        (oneOf('&'),             2, opAssoc.LEFT),
        (oneOf('^'),             2, opAssoc.LEFT),
        (oneOf('|'),             2, opAssoc.LEFT),
        (oneOf('&&'),            2, opAssoc.LEFT),
        (oneOf('||'),            2, opAssoc.LEFT),
        (('?', ':'),             3, opAssoc.LEFT),
    ],
    lpar=Literal("("),
    rpar=Literal(")")
)


TYPE = (
    oneOf(".s8 .s16 .s32 .s64") ^
    oneOf(".u8 .u16 .u32 .u64") ^
    oneOf(".f16 .f16x2 .f32 .f64") ^
    oneOf(".b8 .b16 .b32 .b64") ^
    oneOf(".pred")
)

SPACE = oneOf(".const .global .local .shared")
ALIGN = (Literal(".align") + INTEGER)
TYPENAME = (IDENTIFIER + Optional(Literal("[") + INTEGER + Literal("]")))
PARAM = Group(
    Suppress(".param") + TYPE + Optional(Literal(".ptr") + Optional(SPACE) + ALIGN)
    + TYPENAME
)
ARGLIST = Group(delimitedList(PARAM))

KERNELDEF = (
    Literal(".entry") + IDENTIFIER + Suppress("(") + ARGLIST + Suppress(")")
)

REGDEF = Group(
    Literal(".reg") + TYPE + Group(delimitedList(IDENTIFIER + Optional(Suppress("<") + INTEGER + Suppress(">")))) + Suppress(";")
)

TOKEN = delimitedList(IDENTIFIER | NUMERIC, ".")

instructions = [
    "abs", "discard", "min", "shf", "vadd", "activemask", "div",
    "mma", "shfl", "vadd2", "add", "dp2a", "mov", "shl", "vadd4", "addc", "dp4a",
    "movmatrix", "shr", "vavrg2", "alloca", "elect", "mul", "sin", "vavrg4",
    "and", "ex2", "mul24", "slct", "vmad", "applypriority", "exit", "multimem",
    "sqrt", "vmax", "atom", "fence", "nanosleep", "st", "vmax2", "bar", "fma",
    "neg", "stackrestore", "vmax4", "barrier", "fns", "not", "stacksave",
    "vmin", "bfe", "getctarank", "or", "stmatrix", "vmin2", "bfi",
    "griddepcontrol", "pmevent", "sub", "vmin4", "bfind", "isspacep", "popc",
    "subc", "vote", "bmsk", "istypep", "prefetch", "suld", "vset", "bra", "ld",
    "prefetchu", "suq", "vset2", "brev", "ldmatrix", "prmt", "sured", "vset4",
    "brkpt", "ldu", "rcp", "sust", "vshl", "brx", "lg2", "red", "szext", "vshr",
    "call", "lop3", "redux", "tanh", "vsub", "clz", "mad", "rem", "testp",
    "vsub2", "cnot", "mad24", "ret", "tex", "vsub4", "copysign", "madc", "rsqrt",
    "tld4", "wgmma", "cos", "mapa", "sad", "trap", "wmma", "cp", "match", "selp",
    "txq", "xor", "createpolicy", "max", "set", "vabsdiff", "cvt", "mbarrier",
    "setmaxnreg", "vabsdiff2", "cvta", "membar", "setp", "vabsdiff4",
]

def rangeNum(a:int, b:int):
    return Word(nums).setParseAction(lambda t: a <= int(t[0]) <= b)

# https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#special-registers
SPECIAL_REGISTER = (
    (Literal("%tid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%ntid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%laneid")) ^
    (Literal("%warpid")) ^
    (Literal("%nwarpid")) ^
    (Literal("%ctaid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%nctaid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%simd")) ^
    (Literal("%nsimd")) ^
    (Literal("%gridd")) ^
    (Literal("%is_explicit_cluster")) ^
    (Literal("%clusterid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%nclusterid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%cluster_ctaid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%cluster_nctaid") + Optional(Literal(".") + oneOf("x y z"))) ^
    (Literal("%cluster_ctarank")) ^
    (Literal("%cluster_nctarank")) ^
    (Literal("%lanemask_eq")) ^
    (Literal("%lanemask_le")) ^
    (Literal("%lanemask_lt")) ^
    (Literal("%lanemask_ge")) ^
    (Literal("%lanemask_gt")) ^
    (Literal("%clock")) ^
    (Literal("%clock_hi")) ^
    (Literal("%clock64")) ^
    (Literal("%pm") + rangeNum(0, 7)) ^
    (Literal("%pm") + rangeNum(0, 7) + Literal("_64")) ^
    (Literal("%envreg") + rangeNum(0, 31)) ^
    (Literal("%globaltimer")) ^
    (Literal("%globaltimer_lo")) ^
    (Literal("%globaltimer_hi")) ^
    (Literal("%reserved_smem_offset_begin")) ^
    (Literal("%reserved_smem_offset_end")) ^
    (Literal("%reserved_smem_offset_cap")) ^
    (Literal("%reserved_smem_offset_") + rangeNum(0, 1)) ^
    (Literal("%total_smem_size")) ^
    (Literal("%aggr_smem_size")) ^
    (Literal("%dynamic_smem_size")) ^
    (Literal("%current_graph_exec"))
)

REGISTER_LIKE = (SPECIAL_REGISTER ^ IDENTIFIER)

INSTR_OPERAND = Combine(
    REGISTER_LIKE ^
    (Literal("[") + EXPR + Literal("]"))
)

INSTRUCTION = Group(
    Combine(IDENTIFIER + Literal(":")) ^
    (
        Optional(Literal("@") + Optional("!") + REGISTER_LIKE) + 
        delimitedList(IDENTIFIER, ".", True, min=1) +
        Optional(Group(delimitedList(INSTR_OPERAND))) +
        Suppress(";")
    )
)

KERNEL = (
    KERNELDEF +
    Suppress("{") +
    Group(ZeroOrMore(REGDEF)) +
    Group(ZeroOrMore(INSTRUCTION)) +
    Suppress("}") 
)

PTX = (
    KERNEL
)



kern = """
.entry square_kernel(
	.param .u64 square_kernel_param_0,
	.param .u64 square_kernel_param_1,
	.param .u32 square_kernel_param_2
)
{
	.reg .pred 	%p<2>;
	.reg .b32 	%r<6>;
	.reg .f32 	%f<3>;
	.reg .b64 	%rd<8>;

	ld.param.u32 	%r1, [square_kernel_param_2];
	mov.u32 	%r2, %tid.x;
	mov.u32 	%r3, %ctaid.x;
	mov.u32 	%r4, %ntid.x;
	mad.lo.s32 	%r5, %r3, %r4, %r2;
	setp.lt.s32 	%p1, %r5, %r1;
	bra.uni 	$L__BB0_1;
$L__BB0_2:
	ld.param.u64 	%rd3, [square_kernel_param_0];
	ld.param.u64 	%rd4, [square_kernel_param_1];
	cvta.to.global.u64 	%rd5, %rd4;
	cvta.to.global.u64 	%rd6, %rd3;
	add.s64 	%rd1, %rd5, %rd7;
	add.s64 	%rd2, %rd6, %rd7;
	ld.global.f32 	%f1, [%rd2];
	st.global.f32 	[%rd1], %f2;
$L__BB0_1:
	ret;
}
"""

for i in range(10):
    PTX.parseString(kern, True)

print(PTX.parseString(kern, True).pprint())

#	mul.wide.s32 	%rd7, %r5, 4;
#	mul.rn.f32 	%f2, %f1, %f1;

