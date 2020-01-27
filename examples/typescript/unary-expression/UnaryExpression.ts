
///////////////////////////////////////////////////////////////////////////////

// grammar definition

// UnaryPostfixExpression : unaryExpression -> postfixExpression
// UnaryPlusExpression : unaryExpression -> "plusPlus()" unaryExpression
// UnaryMinusExpression : unaryExpression -> "minusMinus()" unaryExpression
// UnaryOperatorCastExpression : unaryExpression -> unaryOperator castExpression
// UnarySizeofExpression : unaryExpression -> "sizeof()" unaryExpression
// AbstractPointerDeclarator : abstractDeclarator -> pointer
// AbstractPointerDirectDeclarator : abstractDeclarator -> pointer directAbstractDeclarator
// AbstractDirectDeclarator : abstractDeclarator -> directAbstractDeclarator
// ConditionalAssignmentExpression : assignmentExpression -> conditionalExpression
// AssignmentExpression : assignmentExpression -> unaryExpression assignmentOperator assignmentExpression
// AssignmentExpressions : assignmentExpressions -> assignmentExpression assignmentExpressions
// EpsilonAssignmentExpressions : assignmentExpressions -> eps
// AssignmentEqualOperator : assignmentOperator -> "euaql()"
// AssignmentMultiEqualOperator : assignmentOperator -> "multiEqual()"
// AssignmentDivEqualOperator : assignmentOperator -> "divEqual()"
// AssignmentPercentEqualOperator : assignmentOperator -> "percentEqual()"
// AssignmentPlusEqualOperator : assignmentOperator -> "plusEqual()"
// AssignmentMinuxEqualOperator : assignmentOperator -> "minusEqual()"
// AssignmentLeftShiftOperator : assignmentOperator -> "leftShiftEqual()"
// AssignmentRightShiftOperator : assignmentOperator -> "rifhtShiftEqual()"
// AssignmentAndEqualOperator : assignmentOperator -> "andEqual()"
// AssignmentXorEqualOperator : assignmentOperator -> "xorEqual()"
// AssignmentOrEqualOperator : assignmentOperator -> "orEqual()"
// CastExpression : castExpression -> unaryExpression
// ConstantInteger : constant -> "integer(number)"
// ConstantString : constant -> "character(string)"
// ConstantFloat : constant -> "floating(number)"
// EnumIdentifierListSpecifier : enumSpecifier -> "enum()" identifier enumeratorList
// EnumListSpecifier : enumSpecifier -> "enum()" enumeratorList
// EnumIdentifierSpecifier : enumSpecifier -> "enum()" identifier
// EnumeratorIdentifier : enumerator -> identifier
// Identifier : identifier -> constantExpression
// DeclaratorParameterDeclaration : parameterDeclaration -> declarationSpecifier declarationSpecifiers declarator
// AbstractParameterDeclaration : parameterDeclaration -> declarationSpecifier declarationSpecifiers abstractDeclarator
// ParameterDeclaration : parameterDeclaration -> declarationSpecifier declarationSpecifiers
// ParameterList : parameterList -> parameterDeclaration
// ParameterLists : parameterList -> parameterList parameterDeclaration
// PostfixPrimaryExpression : postfixExpression -> primaryExpression
// PostfixExpressions : postfixExpression -> postfixExpression expression
// PostfixAssignmentExpressions : postfixExpression -> postfixExpression assignmentExpressions
// PostfixDotExpressions : postfixExpression -> postfixExpression "dot()" identifier
// PostfixArrowExpressions : postfixExpression -> postfixExpression "arrow()" identifier
// PostfixPlusExpressions : postfixExpression -> postfixExpression "plusPlus()"
// PostfixMinusExpressions : postfixExpression -> postfixExpression "minusMinus()"
// PrimaryIdentifierExpression : primaryExpression -> identifier
// PrimaryConstantExpression : primaryExpression -> constant
// UnaryAndOperator : unaryOperator -> "and()"
// UnaryMultiOperator : unaryOperator -> "multi()"
// UnaryPlusOperator : unaryOperator -> "plus()"
// UnaryMinusOperator : unaryOperator -> "minus()"
// UnaryComplementOperator : unaryOperator -> "complement()"
// UnaryNotOperator : unaryOperator -> "not()"

///////////////////////////////////////////////////////////////////////////////

// util scripts

type Length<T extends unknown[]> = T['length']
type Prepend<Elm, T extends unknown[]> = ((
	arg: Elm,
	...rest: T
) => void) extends ((...args: infer T2) => void)
	? T2
	: never

type Rest<T extends unknown[]> = ((
	...rest: T
) => void) extends ((head: unknown, ...args: infer T2) => void)
	? T2
	: never
declare const None: unique symbol
type None = typeof None
type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]
type AddUnknownRest<Tuple extends unknown[], Result extends unknown[] = [...unknown[]]> = {
	empty: Result,
	nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Next) => unknown)
		? Prepend<First, AddUnknownRest<Rest<Tuple>, Result>>
		: never
}[
	Tuple extends [unknown, ...unknown[]]
		? 'nonEmpty'
		: 'empty'
]

type CompareLength<Left extends any[], Right extends any[]> = {
	fitBoth: 'equal'
	fitLeft: 'shorterLeft'
	fitRight: 'shorterRight'
	unfit: ((..._: Left) => any) extends ((_: any, ..._1: infer LeftRest) => any) ?
		 ((..._: Right) => any) extends ((_: any, ..._1: infer RightRest) => any) ?
					CompareLength<LeftRest, RightRest>
			: never
			: never
}[
	Left['length'] extends Right['length'] ? 'fitBoth' :
	Left extends [] ? 'fitLeft' :
	Right extends [] ? 'fitRight' :
	'unfit'
]

type StartsWith<Tuple extends unknown[], Tuple2 extends unknown[]> = {
	false: 0,
	empty: 1,
	nonEmpty: Head<Tuple> extends Head<Tuple2>
		? StartsWith<Rest<Tuple>, Rest<Tuple2>>
		: 0
}[
	CompareLength<Tuple, Tuple2> extends 'shorterLeft'
		? 'false'
		: IsFinite<Tuple2, 'finite', 'infinite'> extends 'infinite'
			? 'false'
			: Tuple2 extends [unknown, ...unknown[]]
				? 'nonEmpty'
				: 'empty'
]
type IsFinite<Tuple extends unknown[], Finite, Infinite> = {
	empty: Finite
	nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Rest) => unknown)
		? IsFinite<Rest, Finite, Infinite>
		: never
	infinite: Infinite
}[
	Tuple extends [] ? 'empty' :
	Tuple extends (infer Element)[] ?
	Element[] extends Tuple ?
		'infinite'
	: 'nonEmpty'
	: never
]

///////////////////////////////////////////////////////////////////////////////

// AST nodes

interface UnaryExpression {
	accept(v? : Visitor): void
}

interface AbstractDeclarator {
	accept(v? : Visitor): void
}

interface AssignmentExpression {
	accept(v? : Visitor): void
}

interface AssignmentExpressions {
	accept(v? : Visitor): void
}

interface AssignmentOperator {
	accept(v? : Visitor): void
}

interface CastExpression {
	accept(v? : Visitor): void
}

interface Constant {
	accept(v? : Visitor): void
}

interface EnumSpecifier {
	accept(v? : Visitor): void
}

interface Enumerator {
	accept(v? : Visitor): void
}

interface Identifier {
	accept(v? : Visitor): void
}

interface ParameterDeclaration {
	accept(v? : Visitor): void
}

interface ParameterList {
	accept(v? : Visitor): void
}

interface PostfixExpression {
	accept(v? : Visitor): void
}

interface PrimaryExpression {
	accept(v? : Visitor): void
}

interface UnaryOperator {
	accept(v? : Visitor): void
}

interface Pointer {
	accept(v? : Visitor): void
}

interface DirectAbstractDeclarator {
	accept(v? : Visitor): void
}

interface ConditionalExpression {
	accept(v? : Visitor): void
}

interface EnumeratorList {
	accept(v? : Visitor): void
}

interface ConstantExpression {
	accept(v? : Visitor): void
}

interface DeclarationSpecifier {
	accept(v? : Visitor): void
}

interface DeclarationSpecifiers {
	accept(v? : Visitor): void
}

interface Declarator {
	accept(v? : Visitor): void
}

interface Expression {
	accept(v? : Visitor): void
}

export class UnaryPostfixExpression implements UnaryExpression {
	arg1 : PostfixExpression
	constructor(arg1 : PostfixExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryPostfixExpression(this)
		} else {
			new DefaultVisitor().visitUnaryPostfixExpression(this)
		}
	}
}

export class UnaryPlusExpression implements UnaryExpression {
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryPlusExpression(this)
		} else {
			new DefaultVisitor().visitUnaryPlusExpression(this)
		}
	}
}

export class UnaryMinusExpression implements UnaryExpression {
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryMinusExpression(this)
		} else {
			new DefaultVisitor().visitUnaryMinusExpression(this)
		}
	}
}

export class UnaryOperatorCastExpression implements UnaryExpression {
	arg1 : UnaryOperator
	arg2 : CastExpression
	constructor(arg1 : UnaryOperator, arg2 : CastExpression) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryOperatorCastExpression(this)
		} else {
			new DefaultVisitor().visitUnaryOperatorCastExpression(this)
		}
	}
}

export class UnarySizeofExpression implements UnaryExpression {
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitUnarySizeofExpression(this)
		} else {
			new DefaultVisitor().visitUnarySizeofExpression(this)
		}
	}
}

export class AbstractPointerDeclarator implements AbstractDeclarator {
	arg1 : Pointer
	constructor(arg1 : Pointer) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAbstractPointerDeclarator(this)
		} else {
			new DefaultVisitor().visitAbstractPointerDeclarator(this)
		}
	}
}

export class AbstractPointerDirectDeclarator implements AbstractDeclarator {
	arg1 : Pointer
	arg2 : DirectAbstractDeclarator
	constructor(arg1 : Pointer, arg2 : DirectAbstractDeclarator) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAbstractPointerDirectDeclarator(this)
		} else {
			new DefaultVisitor().visitAbstractPointerDirectDeclarator(this)
		}
	}
}

export class AbstractDirectDeclarator implements AbstractDeclarator {
	arg1 : DirectAbstractDeclarator
	constructor(arg1 : DirectAbstractDeclarator) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAbstractDirectDeclarator(this)
		} else {
			new DefaultVisitor().visitAbstractDirectDeclarator(this)
		}
	}
}

export class ConditionalAssignmentExpression implements AssignmentExpression {
	arg1 : ConditionalExpression
	constructor(arg1 : ConditionalExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitConditionalAssignmentExpression(this)
		} else {
			new DefaultVisitor().visitConditionalAssignmentExpression(this)
		}
	}
}

export class AssignmentExpression implements AssignmentExpression {
	arg1 : UnaryExpression
	arg2 : AssignmentOperator
	arg3 : AssignmentExpression
	constructor(arg1 : UnaryExpression, arg2 : AssignmentOperator, arg3 : AssignmentExpression) {
		this.arg1 = arg1
		this.arg2 = arg2
		this.arg3 = arg3
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentExpression(this)
		} else {
			new DefaultVisitor().visitAssignmentExpression(this)
		}
	}
}

export class AssignmentExpressions implements AssignmentExpressions {
	arg1 : AssignmentExpression
	arg2 : AssignmentExpressions
	constructor(arg1 : AssignmentExpression, arg2 : AssignmentExpressions) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentExpressions(this)
		} else {
			new DefaultVisitor().visitAssignmentExpressions(this)
		}
	}
}

export class EpsilonAssignmentExpressions implements AssignmentExpressions {
	accept(v? : Visitor) {
		if (v) {
			v.visitEpsilonAssignmentExpressions(this)
		} else {
			new DefaultVisitor().visitEpsilonAssignmentExpressions(this)
		}
	}
}

export class AssignmentEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentEqualOperator(this)
		}
	}
}

export class AssignmentMultiEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentMultiEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentMultiEqualOperator(this)
		}
	}
}

export class AssignmentDivEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentDivEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentDivEqualOperator(this)
		}
	}
}

export class AssignmentPercentEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentPercentEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentPercentEqualOperator(this)
		}
	}
}

export class AssignmentPlusEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentPlusEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentPlusEqualOperator(this)
		}
	}
}

export class AssignmentMinuxEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentMinuxEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentMinuxEqualOperator(this)
		}
	}
}

export class AssignmentLeftShiftOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentLeftShiftOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentLeftShiftOperator(this)
		}
	}
}

export class AssignmentRightShiftOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentRightShiftOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentRightShiftOperator(this)
		}
	}
}

export class AssignmentAndEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentAndEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentAndEqualOperator(this)
		}
	}
}

export class AssignmentXorEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentXorEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentXorEqualOperator(this)
		}
	}
}

export class AssignmentOrEqualOperator implements AssignmentOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitAssignmentOrEqualOperator(this)
		} else {
			new DefaultVisitor().visitAssignmentOrEqualOperator(this)
		}
	}
}

export class CastExpression implements CastExpression {
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitCastExpression(this)
		} else {
			new DefaultVisitor().visitCastExpression(this)
		}
	}
}

export class ConstantInteger implements Constant {
	arg1 : number
	constructor(arg1 : number) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitConstantInteger(this)
		} else {
			new DefaultVisitor().visitConstantInteger(this)
		}
	}
}

export class ConstantString implements Constant {
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitConstantString(this)
		} else {
			new DefaultVisitor().visitConstantString(this)
		}
	}
}

export class ConstantFloat implements Constant {
	arg1 : number
	constructor(arg1 : number) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitConstantFloat(this)
		} else {
			new DefaultVisitor().visitConstantFloat(this)
		}
	}
}

export class EnumIdentifierListSpecifier implements EnumSpecifier {
	arg1 : Identifier
	arg2 : EnumeratorList
	constructor(arg1 : Identifier, arg2 : EnumeratorList) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitEnumIdentifierListSpecifier(this)
		} else {
			new DefaultVisitor().visitEnumIdentifierListSpecifier(this)
		}
	}
}

export class EnumListSpecifier implements EnumSpecifier {
	arg1 : EnumeratorList
	constructor(arg1 : EnumeratorList) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitEnumListSpecifier(this)
		} else {
			new DefaultVisitor().visitEnumListSpecifier(this)
		}
	}
}

export class EnumIdentifierSpecifier implements EnumSpecifier {
	arg1 : Identifier
	constructor(arg1 : Identifier) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitEnumIdentifierSpecifier(this)
		} else {
			new DefaultVisitor().visitEnumIdentifierSpecifier(this)
		}
	}
}

export class EnumeratorIdentifier implements Enumerator {
	arg1 : Identifier
	constructor(arg1 : Identifier) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitEnumeratorIdentifier(this)
		} else {
			new DefaultVisitor().visitEnumeratorIdentifier(this)
		}
	}
}

export class Identifier implements Identifier {
	arg1 : ConstantExpression
	constructor(arg1 : ConstantExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitIdentifier(this)
		} else {
			new DefaultVisitor().visitIdentifier(this)
		}
	}
}

export class DeclaratorParameterDeclaration implements ParameterDeclaration {
	arg1 : DeclarationSpecifier
	arg2 : DeclarationSpecifiers
	arg3 : Declarator
	constructor(arg1 : DeclarationSpecifier, arg2 : DeclarationSpecifiers, arg3 : Declarator) {
		this.arg1 = arg1
		this.arg2 = arg2
		this.arg3 = arg3
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitDeclaratorParameterDeclaration(this)
		} else {
			new DefaultVisitor().visitDeclaratorParameterDeclaration(this)
		}
	}
}

export class AbstractParameterDeclaration implements ParameterDeclaration {
	arg1 : DeclarationSpecifier
	arg2 : DeclarationSpecifiers
	arg3 : AbstractDeclarator
	constructor(arg1 : DeclarationSpecifier, arg2 : DeclarationSpecifiers, arg3 : AbstractDeclarator) {
		this.arg1 = arg1
		this.arg2 = arg2
		this.arg3 = arg3
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAbstractParameterDeclaration(this)
		} else {
			new DefaultVisitor().visitAbstractParameterDeclaration(this)
		}
	}
}

export class ParameterDeclaration implements ParameterDeclaration {
	arg1 : DeclarationSpecifier
	arg2 : DeclarationSpecifiers
	constructor(arg1 : DeclarationSpecifier, arg2 : DeclarationSpecifiers) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitParameterDeclaration(this)
		} else {
			new DefaultVisitor().visitParameterDeclaration(this)
		}
	}
}

export class ParameterList implements ParameterList {
	arg1 : ParameterDeclaration
	constructor(arg1 : ParameterDeclaration) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitParameterList(this)
		} else {
			new DefaultVisitor().visitParameterList(this)
		}
	}
}

export class ParameterLists implements ParameterList {
	arg1 : ParameterList
	arg2 : ParameterDeclaration
	constructor(arg1 : ParameterList, arg2 : ParameterDeclaration) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitParameterLists(this)
		} else {
			new DefaultVisitor().visitParameterLists(this)
		}
	}
}

export class PostfixPrimaryExpression implements PostfixExpression {
	arg1 : PrimaryExpression
	constructor(arg1 : PrimaryExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixPrimaryExpression(this)
		} else {
			new DefaultVisitor().visitPostfixPrimaryExpression(this)
		}
	}
}

export class PostfixExpressions implements PostfixExpression {
	arg1 : PostfixExpression
	arg2 : Expression
	constructor(arg1 : PostfixExpression, arg2 : Expression) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixExpressions(this)
		} else {
			new DefaultVisitor().visitPostfixExpressions(this)
		}
	}
}

export class PostfixAssignmentExpressions implements PostfixExpression {
	arg1 : PostfixExpression
	arg2 : AssignmentExpressions
	constructor(arg1 : PostfixExpression, arg2 : AssignmentExpressions) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixAssignmentExpressions(this)
		} else {
			new DefaultVisitor().visitPostfixAssignmentExpressions(this)
		}
	}
}

export class PostfixDotExpressions implements PostfixExpression {
	arg1 : PostfixExpression
	arg2 : Identifier
	constructor(arg1 : PostfixExpression, arg2 : Identifier) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixDotExpressions(this)
		} else {
			new DefaultVisitor().visitPostfixDotExpressions(this)
		}
	}
}

export class PostfixArrowExpressions implements PostfixExpression {
	arg1 : PostfixExpression
	arg2 : Identifier
	constructor(arg1 : PostfixExpression, arg2 : Identifier) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixArrowExpressions(this)
		} else {
			new DefaultVisitor().visitPostfixArrowExpressions(this)
		}
	}
}

export class PostfixPlusExpressions implements PostfixExpression {
	arg1 : PostfixExpression
	constructor(arg1 : PostfixExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixPlusExpressions(this)
		} else {
			new DefaultVisitor().visitPostfixPlusExpressions(this)
		}
	}
}

export class PostfixMinusExpressions implements PostfixExpression {
	arg1 : PostfixExpression
	constructor(arg1 : PostfixExpression) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPostfixMinusExpressions(this)
		} else {
			new DefaultVisitor().visitPostfixMinusExpressions(this)
		}
	}
}

export class PrimaryIdentifierExpression implements PrimaryExpression {
	arg1 : Identifier
	constructor(arg1 : Identifier) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPrimaryIdentifierExpression(this)
		} else {
			new DefaultVisitor().visitPrimaryIdentifierExpression(this)
		}
	}
}

export class PrimaryConstantExpression implements PrimaryExpression {
	arg1 : Constant
	constructor(arg1 : Constant) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitPrimaryConstantExpression(this)
		} else {
			new DefaultVisitor().visitPrimaryConstantExpression(this)
		}
	}
}

export class UnaryAndOperator implements UnaryOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryAndOperator(this)
		} else {
			new DefaultVisitor().visitUnaryAndOperator(this)
		}
	}
}

export class UnaryMultiOperator implements UnaryOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryMultiOperator(this)
		} else {
			new DefaultVisitor().visitUnaryMultiOperator(this)
		}
	}
}

export class UnaryPlusOperator implements UnaryOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryPlusOperator(this)
		} else {
			new DefaultVisitor().visitUnaryPlusOperator(this)
		}
	}
}

export class UnaryMinusOperator implements UnaryOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryMinusOperator(this)
		} else {
			new DefaultVisitor().visitUnaryMinusOperator(this)
		}
	}
}

export class UnaryComplementOperator implements UnaryOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryComplementOperator(this)
		} else {
			new DefaultVisitor().visitUnaryComplementOperator(this)
		}
	}
}

export class UnaryNotOperator implements UnaryOperator {
	accept(v? : Visitor) {
		if (v) {
			v.visitUnaryNotOperator(this)
		} else {
			new DefaultVisitor().visitUnaryNotOperator(this)
		}
	}
}










interface Visitor {
	visitUnaryPostfixExpression(host : UnaryPostfixExpression): void
	visitUnaryPlusExpression(host : UnaryPlusExpression): void
	visitUnaryMinusExpression(host : UnaryMinusExpression): void
	visitUnaryOperatorCastExpression(host : UnaryOperatorCastExpression): void
	visitUnarySizeofExpression(host : UnarySizeofExpression): void
	visitAbstractPointerDeclarator(host : AbstractPointerDeclarator): void
	visitAbstractPointerDirectDeclarator(host : AbstractPointerDirectDeclarator): void
	visitAbstractDirectDeclarator(host : AbstractDirectDeclarator): void
	visitConditionalAssignmentExpression(host : ConditionalAssignmentExpression): void
	visitAssignmentExpression(host : AssignmentExpression): void
	visitAssignmentExpressions(host : AssignmentExpressions): void
	visitEpsilonAssignmentExpressions(host : EpsilonAssignmentExpressions): void
	visitAssignmentEqualOperator(host : AssignmentEqualOperator): void
	visitAssignmentMultiEqualOperator(host : AssignmentMultiEqualOperator): void
	visitAssignmentDivEqualOperator(host : AssignmentDivEqualOperator): void
	visitAssignmentPercentEqualOperator(host : AssignmentPercentEqualOperator): void
	visitAssignmentPlusEqualOperator(host : AssignmentPlusEqualOperator): void
	visitAssignmentMinuxEqualOperator(host : AssignmentMinuxEqualOperator): void
	visitAssignmentLeftShiftOperator(host : AssignmentLeftShiftOperator): void
	visitAssignmentRightShiftOperator(host : AssignmentRightShiftOperator): void
	visitAssignmentAndEqualOperator(host : AssignmentAndEqualOperator): void
	visitAssignmentXorEqualOperator(host : AssignmentXorEqualOperator): void
	visitAssignmentOrEqualOperator(host : AssignmentOrEqualOperator): void
	visitCastExpression(host : CastExpression): void
	visitConstantInteger(host : ConstantInteger): void
	visitConstantString(host : ConstantString): void
	visitConstantFloat(host : ConstantFloat): void
	visitEnumIdentifierListSpecifier(host : EnumIdentifierListSpecifier): void
	visitEnumListSpecifier(host : EnumListSpecifier): void
	visitEnumIdentifierSpecifier(host : EnumIdentifierSpecifier): void
	visitEnumeratorIdentifier(host : EnumeratorIdentifier): void
	visitIdentifier(host : Identifier): void
	visitDeclaratorParameterDeclaration(host : DeclaratorParameterDeclaration): void
	visitAbstractParameterDeclaration(host : AbstractParameterDeclaration): void
	visitParameterDeclaration(host : ParameterDeclaration): void
	visitParameterList(host : ParameterList): void
	visitParameterLists(host : ParameterLists): void
	visitPostfixPrimaryExpression(host : PostfixPrimaryExpression): void
	visitPostfixExpressions(host : PostfixExpressions): void
	visitPostfixAssignmentExpressions(host : PostfixAssignmentExpressions): void
	visitPostfixDotExpressions(host : PostfixDotExpressions): void
	visitPostfixArrowExpressions(host : PostfixArrowExpressions): void
	visitPostfixPlusExpressions(host : PostfixPlusExpressions): void
	visitPostfixMinusExpressions(host : PostfixMinusExpressions): void
	visitPrimaryIdentifierExpression(host : PrimaryIdentifierExpression): void
	visitPrimaryConstantExpression(host : PrimaryConstantExpression): void
	visitUnaryAndOperator(host : UnaryAndOperator): void
	visitUnaryMultiOperator(host : UnaryMultiOperator): void
	visitUnaryPlusOperator(host : UnaryPlusOperator): void
	visitUnaryMinusOperator(host : UnaryMinusOperator): void
	visitUnaryComplementOperator(host : UnaryComplementOperator): void
	visitUnaryNotOperator(host : UnaryNotOperator): void
}

export class DefaultVisitor implements Visitor {
	visitUnaryPostfixExpression(host : UnaryPostfixExpression) {
		process.stdout.write("UnaryPostfixExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitUnaryPlusExpression(host : UnaryPlusExpression) {
		process.stdout.write("UnaryPlusExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitUnaryMinusExpression(host : UnaryMinusExpression) {
		process.stdout.write("UnaryMinusExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitUnaryOperatorCastExpression(host : UnaryOperatorCastExpression) {
		process.stdout.write("UnaryOperatorCastExpression(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitUnarySizeofExpression(host : UnarySizeofExpression) {
		process.stdout.write("UnarySizeofExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitAbstractPointerDeclarator(host : AbstractPointerDeclarator) {
		process.stdout.write("AbstractPointerDeclarator(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitAbstractPointerDirectDeclarator(host : AbstractPointerDirectDeclarator) {
		process.stdout.write("AbstractPointerDirectDeclarator(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitAbstractDirectDeclarator(host : AbstractDirectDeclarator) {
		process.stdout.write("AbstractDirectDeclarator(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitConditionalAssignmentExpression(host : ConditionalAssignmentExpression) {
		process.stdout.write("ConditionalAssignmentExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitAssignmentExpression(host : AssignmentExpression) {
		process.stdout.write("AssignmentExpression(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		host.arg3.accept(this)
		process.stdout.write(")")
	}
	visitAssignmentExpressions(host : AssignmentExpressions) {
		process.stdout.write("AssignmentExpressions(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitEpsilonAssignmentExpressions(host : EpsilonAssignmentExpressions) {
		process.stdout.write("EpsilonAssignmentExpressions(")
		process.stdout.write(")")
	}
	visitAssignmentEqualOperator(host : AssignmentEqualOperator) {
		process.stdout.write("AssignmentEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentMultiEqualOperator(host : AssignmentMultiEqualOperator) {
		process.stdout.write("AssignmentMultiEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentDivEqualOperator(host : AssignmentDivEqualOperator) {
		process.stdout.write("AssignmentDivEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentPercentEqualOperator(host : AssignmentPercentEqualOperator) {
		process.stdout.write("AssignmentPercentEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentPlusEqualOperator(host : AssignmentPlusEqualOperator) {
		process.stdout.write("AssignmentPlusEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentMinuxEqualOperator(host : AssignmentMinuxEqualOperator) {
		process.stdout.write("AssignmentMinuxEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentLeftShiftOperator(host : AssignmentLeftShiftOperator) {
		process.stdout.write("AssignmentLeftShiftOperator(")
		process.stdout.write(")")
	}
	visitAssignmentRightShiftOperator(host : AssignmentRightShiftOperator) {
		process.stdout.write("AssignmentRightShiftOperator(")
		process.stdout.write(")")
	}
	visitAssignmentAndEqualOperator(host : AssignmentAndEqualOperator) {
		process.stdout.write("AssignmentAndEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentXorEqualOperator(host : AssignmentXorEqualOperator) {
		process.stdout.write("AssignmentXorEqualOperator(")
		process.stdout.write(")")
	}
	visitAssignmentOrEqualOperator(host : AssignmentOrEqualOperator) {
		process.stdout.write("AssignmentOrEqualOperator(")
		process.stdout.write(")")
	}
	visitCastExpression(host : CastExpression) {
		process.stdout.write("CastExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitConstantInteger(host : ConstantInteger) {
		process.stdout.write("ConstantInteger(")
		process.stdout.write(host.arg1)
		process.stdout.write(")")
	}
	visitConstantString(host : ConstantString) {
		process.stdout.write("ConstantString(")
		process.stdout.write(host.arg1)
		process.stdout.write(")")
	}
	visitConstantFloat(host : ConstantFloat) {
		process.stdout.write("ConstantFloat(")
		process.stdout.write(host.arg1)
		process.stdout.write(")")
	}
	visitEnumIdentifierListSpecifier(host : EnumIdentifierListSpecifier) {
		process.stdout.write("EnumIdentifierListSpecifier(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitEnumListSpecifier(host : EnumListSpecifier) {
		process.stdout.write("EnumListSpecifier(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitEnumIdentifierSpecifier(host : EnumIdentifierSpecifier) {
		process.stdout.write("EnumIdentifierSpecifier(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitEnumeratorIdentifier(host : EnumeratorIdentifier) {
		process.stdout.write("EnumeratorIdentifier(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitIdentifier(host : Identifier) {
		process.stdout.write("Identifier(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitDeclaratorParameterDeclaration(host : DeclaratorParameterDeclaration) {
		process.stdout.write("DeclaratorParameterDeclaration(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		host.arg3.accept(this)
		process.stdout.write(")")
	}
	visitAbstractParameterDeclaration(host : AbstractParameterDeclaration) {
		process.stdout.write("AbstractParameterDeclaration(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		host.arg3.accept(this)
		process.stdout.write(")")
	}
	visitParameterDeclaration(host : ParameterDeclaration) {
		process.stdout.write("ParameterDeclaration(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitParameterList(host : ParameterList) {
		process.stdout.write("ParameterList(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitParameterLists(host : ParameterLists) {
		process.stdout.write("ParameterLists(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitPostfixPrimaryExpression(host : PostfixPrimaryExpression) {
		process.stdout.write("PostfixPrimaryExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitPostfixExpressions(host : PostfixExpressions) {
		process.stdout.write("PostfixExpressions(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitPostfixAssignmentExpressions(host : PostfixAssignmentExpressions) {
		process.stdout.write("PostfixAssignmentExpressions(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitPostfixDotExpressions(host : PostfixDotExpressions) {
		process.stdout.write("PostfixDotExpressions(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitPostfixArrowExpressions(host : PostfixArrowExpressions) {
		process.stdout.write("PostfixArrowExpressions(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitPostfixPlusExpressions(host : PostfixPlusExpressions) {
		process.stdout.write("PostfixPlusExpressions(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitPostfixMinusExpressions(host : PostfixMinusExpressions) {
		process.stdout.write("PostfixMinusExpressions(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitPrimaryIdentifierExpression(host : PrimaryIdentifierExpression) {
		process.stdout.write("PrimaryIdentifierExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitPrimaryConstantExpression(host : PrimaryConstantExpression) {
		process.stdout.write("PrimaryConstantExpression(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitUnaryAndOperator(host : UnaryAndOperator) {
		process.stdout.write("UnaryAndOperator(")
		process.stdout.write(")")
	}
	visitUnaryMultiOperator(host : UnaryMultiOperator) {
		process.stdout.write("UnaryMultiOperator(")
		process.stdout.write(")")
	}
	visitUnaryPlusOperator(host : UnaryPlusOperator) {
		process.stdout.write("UnaryPlusOperator(")
		process.stdout.write(")")
	}
	visitUnaryMinusOperator(host : UnaryMinusOperator) {
		process.stdout.write("UnaryMinusOperator(")
		process.stdout.write(")")
	}
	visitUnaryComplementOperator(host : UnaryComplementOperator) {
		process.stdout.write("UnaryComplementOperator(")
		process.stdout.write(")")
	}
	visitUnaryNotOperator(host : UnaryNotOperator) {
		process.stdout.write("UnaryNotOperator(")
		process.stdout.write(")")
	}
}

///////////////////////////////////////////////////////////////////////////////

// automaton states

type Node = Node1 | Node2 | Node3 | Node4 | Node5 | Node6 | Node7 | Node8 | Node9 | Node10 | Node11 | Node12 | Node13 | Node14 | Node15 | Node16 | Node17 | Node18 | Node19 | Node20 | Node21 | Node22 | Node23 | Node24 | Node25 | Node26 | Node27 | Node28 | Node29 | Node30 | Node31 | Node32 | Node33 | Node34 | Node35 | Node36 | Node37 | Node38 | Node39 | Node40 | Node41 | Node42 | Node43 | Node44 | Node45 | Node46 | Node47 | Node48 | Node49 | Node50

class Node1 {
	private _Node1Brand: boolean = true
}

class Node2 {
	private _Node2Brand: boolean = true
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
}

class Node3 {
	private _Node3Brand: boolean = true
}

class Node4 {
	private _Node4Brand: boolean = true
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
}

class Node5 {
	private _Node5Brand: boolean = true
}

class Node6 {
	private _Node6Brand: boolean = true
}

class Node7 {
	private _Node7Brand: boolean = true
	arg1 : AssignmentExpression
	constructor(arg1 : AssignmentExpression) {
		this.arg1 = arg1
	}
}

class Node8 {
	private _Node8Brand: boolean = true
	arg1 : AssignmentOperator
	constructor(arg1 : AssignmentOperator) {
		this.arg1 = arg1
	}
}

class Node9 {
	private _Node9Brand: boolean = true
	arg1 : AssignmentExpression
	constructor(arg1 : AssignmentExpression) {
		this.arg1 = arg1
	}
}

class Node10 {
	private _Node10Brand: boolean = true
	arg1 : PostfixExpression
	constructor(arg1 : PostfixExpression) {
		this.arg1 = arg1
	}
}

class Node11 {
	private _Node11Brand: boolean = true
	arg1 : AssignmentExpressions
	constructor(arg1 : AssignmentExpressions) {
		this.arg1 = arg1
	}
}

class Node12 {
	private _Node12Brand: boolean = true
}

class Node13 {
	private _Node13Brand: boolean = true
}

class Node14 {
	private _Node14Brand: boolean = true
}

class Node15 {
	private _Node15Brand: boolean = true
}

class Node16 {
	private _Node16Brand: boolean = true
}

class Node17 {
	private _Node17Brand: boolean = true
}

class Node18 {
	private _Node18Brand: boolean = true
}

class Node19 {
	private _Node19Brand: boolean = true
}

class Node20 {
	private _Node20Brand: boolean = true
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
}

class Node21 {
	private _Node21Brand: boolean = true
	arg1 : UnaryOperator
	constructor(arg1 : UnaryOperator) {
		this.arg1 = arg1
	}
}

class Node22 {
	private _Node22Brand: boolean = true
	arg1 : ConditionalExpression
	constructor(arg1 : ConditionalExpression) {
		this.arg1 = arg1
	}
}

class Node23 {
	private _Node23Brand: boolean = true
	arg1 : number
	constructor(arg1 : number) {
		this.arg1 = arg1
	}
}

class Node24 {
	private _Node24Brand: boolean = true
}

class Node25 {
	private _Node25Brand: boolean = true
}

class Node26 {
	private _Node26Brand: boolean = true
}

class Node27 {
	private _Node27Brand: boolean = true
}

class Node28 {
	private _Node28Brand: boolean = true
}

class Node29 {
	private _Node29Brand: boolean = true
	arg1 : number
	constructor(arg1 : number) {
		this.arg1 = arg1
	}
}

class Node30 {
	private _Node30Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node31 {
	private _Node31Brand: boolean = true
	arg1 : ConstantExpression
	constructor(arg1 : ConstantExpression) {
		this.arg1 = arg1
	}
}

class Node32 {
	private _Node32Brand: boolean = true
}

class Node33 {
	private _Node33Brand: boolean = true
}

class Node34 {
	private _Node34Brand: boolean = true
	arg1 : Identifier
	constructor(arg1 : Identifier) {
		this.arg1 = arg1
	}
}

class Node35 {
	private _Node35Brand: boolean = true
	arg1 : AssignmentExpressions
	constructor(arg1 : AssignmentExpressions) {
		this.arg1 = arg1
	}
}

class Node36 {
	private _Node36Brand: boolean = true
	arg1 : Identifier
	constructor(arg1 : Identifier) {
		this.arg1 = arg1
	}
}

class Node37 {
	private _Node37Brand: boolean = true
	arg1 : Expression
	constructor(arg1 : Expression) {
		this.arg1 = arg1
	}
}

class Node38 {
	private _Node38Brand: boolean = true
	arg1 : PrimaryExpression
	constructor(arg1 : PrimaryExpression) {
		this.arg1 = arg1
	}
}

class Node39 {
	private _Node39Brand: boolean = true
	arg1 : Constant
	constructor(arg1 : Constant) {
		this.arg1 = arg1
	}
}

class Node40 {
	private _Node40Brand: boolean = true
	arg1 : Identifier
	constructor(arg1 : Identifier) {
		this.arg1 = arg1
	}
}

class Node41 {
	private _Node41Brand: boolean = true
}

class Node42 {
	private _Node42Brand: boolean = true
}

class Node43 {
	private _Node43Brand: boolean = true
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
}

class Node44 {
	private _Node44Brand: boolean = true
}

class Node45 {
	private _Node45Brand: boolean = true
}

class Node46 {
	private _Node46Brand: boolean = true
}

class Node47 {
	private _Node47Brand: boolean = true
	arg1 : CastExpression
	constructor(arg1 : CastExpression) {
		this.arg1 = arg1
	}
}

class Node48 {
	private _Node48Brand: boolean = true
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
}

class Node49 {
	private _Node49Brand: boolean = true
}

class Node50 {
	private _Node50Brand: boolean = true
	arg1 : UnaryExpression
	constructor(arg1 : UnaryExpression) {
		this.arg1 = arg1
	}
}

///////////////////////////////////////////////////////////////////////////////

// transitions

function startsWithNode2(arg: any): arg is AddUnknownRest<[Node2]> {
	return arg[0] && arg[0]._Node2Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode3Node4(arg: any): arg is AddUnknownRest<[Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode6Node4(arg: any): arg is AddUnknownRest<[Node6, Node4]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode7Node8Node4Node8(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node8]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node8Brand
}


function startsWithNode7Node8Node4Node9(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node9]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode7Node8Node4Node10(arg: any): arg is AddUnknownRest<[Node7, Node8, Node4, Node10]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node10Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode8(arg: any): arg is AddUnknownRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	r