from pyparsing import *



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

expr = infixNotation(NUMERIC,
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
# expr.parseString("(s64)123", True).pprint()



# directives = [

# ]

# DIRECTIVE = oneOf(directives)

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
ARGLIST = Group(DelimitedList(PARAM))

KERNELDEF = (
    Literal(".entry") + IDENTIFIER + Suppress("(") + ARGLIST + Suppress(")")
)

REGDEF = (
    Literal(".reg") + TYPE + IDENTIFIER + Literal("<") + INTEGER + Literal(">") + Literal(";")
)

TOKEN = delimitedList(IDENTIFIER | NUMERIC, ".")

INSTRUCTION = (
    (Literal("@") + IDENTIFIER + IDENTIFIER + IDENTIFIER + Literal(";")) |
    (IDENTIFIER + Literal(":")) |
    (TOKEN + Optional(DelimitedList(TOKEN | (Literal("[") + IDENTIFIER + Literal("]")))) + Literal(";"))
)

KERNEL = (
    KERNELDEF +
    Suppress("{") +
    ZeroOrMore(REGDEF) +
    ZeroOrMore(INSTRUCTION) +
    Suppress("}") 
)

PTX = (
    KERNEL
)

PTX.parseString("""
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
	@%p1 bra 	$L__BB0_2;
	bra.uni 	$L__BB0_1;
$L__BB0_2:
	ld.param.u64 	%rd3, [square_kernel_param_0];
	ld.param.u64 	%rd4, [square_kernel_param_1];
	cvta.to.global.u64 	%rd5, %rd4;
	cvta.to.global.u64 	%rd6, %rd3;
	mul.wide.s32 	%rd7, %r5, 4;
	add.s64 	%rd1, %rd5, %rd7;
	add.s64 	%rd2, %rd6, %rd7;
	ld.global.f32 	%f1, [%rd2];
	mul.rn.f32 	%f2, %f1, %f1;
	st.global.f32 	[%rd1], %f2;
$L__BB0_1:
	ret;
}
""", True).pprint()

