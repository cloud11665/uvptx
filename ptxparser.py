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
INT_LIT = HEX_LIT ^ OCT_LIT ^ BIN_LIT ^ DEC_LIT
FLT_LIT = F32_LIT ^ F64_LIT
NUMERIC = BOOL_LIT ^ INT_LIT ^ FLT_LIT
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

ATTRIBUTE = (
    (Literal(".managed") + Optional(Suppress("(") + Group(delimitedList(NUMERIC)) + Suppress(")"))) ^
    (Literal(".unified") + Optional(Suppress("(") + Group(delimitedList(NUMERIC)) + Suppress(")")))
)

DERIVATIVE = Group(
    (Literal(".version") + INTEGER("major") + Literal(".") + INTEGER("minor")) ^
    (Literal(".target") + delimitedList(IDENTIFIER)) ^
    (Literal(".address_size") + INTEGER) ^
    (Literal(".entry")) ^ 
    (Literal(".func")) ^ 
    ALIGN ^
    SPACE ^
    (Literal(".attribute") + Suppress("(") + Group(ATTRIBUTE) + Suppress(")")) ^
    (Literal(".noreturn")) ^ 
    (Literal(".visible"))
)

TYPENAME = (IDENTIFIER + Optional(Literal("[") + INTEGER + Literal("]")))
PARAM = Group(
    Suppress(".param") + TYPE + Optional(Literal(".ptr") + Optional(SPACE) + ALIGN)
    + TYPENAME
)
ARGLIST = Group(delimitedList(PARAM))


KERNELDEF = (
    Literal(".entry") + 
    IDENTIFIER + Suppress("(") + ARGLIST + Suppress(")")
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
    NUMERIC ^
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
    ZeroOrMore(
        KERNEL ^ DERIVATIVES
    )
).ignore(cppStyleComment)



# todo: make this more clean.
"""
def parse(tokens)
    token = tokens[0]
    if token == ".visible":
        res = parse(tokens[1:])
        res.visible = True
    elif token == ".entry":
        
    
        
        
        
        """

kern = """
.version 7.5
.target sm_75
.address_size 64

        // .globl       _Z4E_n2PfPKfS1_

.visible .entry _Z4E_n2PfPKfS1_(
        .param .u64 _Z4E_n2PfPKfS1__param_0,
        .param .u64 _Z4E_n2PfPKfS1__param_1,
        .param .u64 _Z4E_n2PfPKfS1__param_2
)
{
        .reg .pred      %p<3>;
        .reg .f32       %f<14>;
        .reg .b64       %rd<7>;


        ld.param.u64    %rd1, [_Z4E_n2PfPKfS1__param_0];
        ld.param.u64    %rd2, [_Z4E_n2PfPKfS1__param_1];
        ld.param.u64    %rd3, [_Z4E_n2PfPKfS1__param_2];
        cvta.to.global.u64      %rd4, %rd1;
        cvta.to.global.u64      %rd5, %rd3;
        cvta.to.global.u64      %rd6, %rd2;
        ld.global.f32   %f1, [%rd6];
        ld.global.f32   %f2, [%rd5];
        mov.f32         %f3, 0f00000000;
        sub.f32         %f4, %f3, %f1;
        max.f32         %f5, %f4, %f3;
        setp.eq.f32     %p1, %f5, 0f00000000;
        selp.f32        %f6, 0f00000000, 0f3F800000, %p1;
        add.f32         %f7, %f2, 0f3F800000;
        add.f32         %f8, %f7, %f7;
        mul.f32         %f9, %f8, %f6;
        sub.f32         %f10, %f3, %f9;
        max.f32         %f11, %f1, %f3;
        setp.eq.f32     %p2, %f11, 0f00000000;
        selp.f32        %f12, 0f00000000, 0f3F800000, %p2;
        fma.rn.f32      %f13, %f8, %f12, %f10;
        st.global.f32   [%rd4], %f13;
        ret;
}
"""

print(PTX.parseString(kern, True).pprint())



tests = [
"""
.func foo ( .param .b32 N, .param .b32 buffer[32] )
{
    .reg .u32  %n, %r;
    .reg .f32  %f;
    .reg .pred %p;

    ld.param.u32 %n, [N];
    mov.u32      %r, buffer;  // forces buffer to .local state space
Loop:
    setp.eq.u32  %p, %n, 0;
@%p: bra         Done;
    ld.local.f32 %f, [%r];
    add.u32      %r, %r, 4;
    sub.u32      %n, %n, 1;
    bra          Loop;
Done:
    ret;
}
""",
"""
.entry foo ( .param .u32 param1,
             .param .u32 .ptr.global.align 16 param2,
             .param .u32 .ptr.const.align 8 param3,
             .param .u32 .ptr.align 16 param4  // generic address
                                               // pointer
) { }
""",
"""
// pass object of type struct { double d; int y; };
.func foo ( .reg .b32 N, .param .align 8 .b8 buffer[12] )
{
    .reg .f64 %d;
    .reg .s32 %y;

    ld.param.f64 %d, [buffer];
    ld.param.s32 %y, [buffer+8];
}

// code snippet from the caller
// struct { double d; int y; } mystruct; is flattened, passed to foo
.func bar () {
    .reg .f64 dbl;
    .reg .s32 x;
    .param .align 8 .b8 mystruct;
    st.param.f64 [mystruct+0], dbl;
    st.param.s32 [mystruct+8], x;
    call foo, (4, mystruct);

}
"""
]