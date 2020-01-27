
///////////////////////////////////////////////////////////////////////////////

// grammar definition

// Add : E -> E "add()" T
// TToE : E -> T
// Num : F -> "num(Int)"
// Paren : F -> "lp()" E "rp()"
// Mul : T -> T "mul()" F
// FToT : T -> F

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

interface E {
	accept(v? : Visitor): void
}

interface F {
	accept(v? : Visitor): void
}

interface T {
	accept(v? : Visitor): void
}

export class Add implements E {
	arg1 : E
	arg2 : T
	constructor(arg1 : E, arg2 : T) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAdd(this)
		} else {
			new DefaultVisitor().visitAdd(this)
		}
	}
}

export class TToE implements E {
	arg1 : T
	constructor(arg1 : T) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitTToE(this)
		} else {
			new DefaultVisitor().visitTToE(this)
		}
	}
}

export class Num implements F {
	arg1 : Int
	constructor(arg1 : Int) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitNum(this)
		} else {
			new DefaultVisitor().visitNum(this)
		}
	}
}

export class Paren implements F {
	arg1 : E
	constructor(arg1 : E) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitParen(this)
		} else {
			new DefaultVisitor().visitParen(this)
		}
	}
}

export class Mul implements T {
	arg1 : T
	arg2 : F
	constructor(arg1 : T, arg2 : F) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitMul(this)
		} else {
			new DefaultVisitor().visitMul(this)
		}
	}
}

export class FToT implements T {
	arg1 : F
	constructor(arg1 : F) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitFToT(this)
		} else {
			new DefaultVisitor().visitFToT(this)
		}
	}
}

interface Visitor {
	visitAdd(host : Add): void
	visitTToE(host : TToE): void
	visitNum(host : Num): void
	visitParen(host : Paren): void
	visitMul(host : Mul): void
	visitFToT(host : FToT): void
}

export class DefaultVisitor implements Visitor {
	visitAdd(host : Add) {
		process.stdout.write("Add(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitTToE(host : TToE) {
		process.stdout.write("TToE(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitNum(host : Num) {
		process.stdout.write("Num(")
		process.stdout.write(host.arg1)
		process.stdout.write(")")
	}
	visitParen(host : Paren) {
		process.stdout.write("Paren(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitMul(host : Mul) {
		process.stdout.write("Mul(")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitFToT(host : FToT) {
		process.stdout.write("FToT(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
}

///////////////////////////////////////////////////////////////////////////////

// automaton states

type Node = Node1 | Node2 | Node3 | Node4 | Node5 | Node6 | Node7 | Node8 | Node9 | Node10 | Node11 | Node12

class Node1 {
	private _Node1Brand: boolean = true
}

class Node2 {
	private _Node2Brand: boolean = true
	arg1 : E
	constructor(arg1 : E) {
		this.arg1 = arg1
	}
}

class Node3 {
	private _Node3Brand: boolean = true
	arg1 : T
	constructor(arg1 : T) {
		this.arg1 = arg1
	}
}

class Node4 {
	private _Node4Brand: boolean = true
}

class Node5 {
	private _Node5Brand: boolean = true
}

class Node6 {
	private _Node6Brand: boolean = true
	arg1 : E
	constructor(arg1 : E) {
		this.arg1 = arg1
	}
}

class Node7 {
	private _Node7Brand: boolean = true
	arg1 : F
	constructor(arg1 : F) {
		this.arg1 = arg1
	}
}

class Node8 {
	private _Node8Brand: boolean = true
	arg1 : F
	constructor(arg1 : F) {
		this.arg1 = arg1
	}
}

class Node9 {
	private _Node9Brand: boolean = true
}

class Node10 {
	private _Node10Brand: boolean = true
	arg1 : T
	constructor(arg1 : T) {
		this.arg1 = arg1
	}
}

class Node11 {
	private _Node11Brand: boolean = true
	arg1 : Int
	constructor(arg1 : Int) {
		this.arg1 = arg1
	}
}

class Node12 {
	private _Node12Brand: boolean = true
}

///////////////////////////////////////////////////////////////////////////////

// transitions

function startsWithNode2(arg: any): arg is AddUnknownRest<[Node2]> {
	return arg[0] && arg[0]._Node2Brand
}


function startsWithNode2(arg: any): arg is AddUnknownRest<[Node2]> {
	return arg[0] && arg[0]._Node2Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode3Node5Node2Node1(arg: any): arg is AddUnknownRest<[Node3, Node5, Node2, Node1]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node2Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode3Node5Node6Node4(arg: any): arg is AddUnknownRest<[Node3, Node5, Node6, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node6Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode3(arg: any): arg is AddUnknownRest<[Node3]> {
	return arg[0] && arg[0]._Node3Brand
}


function startsWithNode3Node5Node2Node1(arg: any): arg is AddUnknownRest<[Node3, Node5, Node2, Node1]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node2Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode3Node5Node6Node4(arg: any): arg is AddUnknownRest<[Node3, Node5, Node6, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node6Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode3Node5Node2Node1(arg: any): arg is AddUnknownRest<[Node3, Node5, Node2, Node1]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node2Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode3Node5Node6Node4(arg: any): arg is AddUnknownRest<[Node3, Node5, Node6, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node6Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode5(arg: any): arg is AddUnknownRest<[Node5]> {
	return arg[0] && arg[0]._Node5Brand
}


function startsWithNode5(arg: any): arg is AddUnknownRest<[Node5]> {
	return arg[0] && arg[0]._Node5Brand
}


function startsWithNode6(arg: any): arg is AddUnknownRest<[Node6]> {
	return arg[0] && arg[0]._Node6Brand
}


function startsWithNode6(arg: any): arg is AddUnknownRest<[Node6]> {
	return arg[0] && arg[0]._Node6Brand
}


function startsWithNode7Node1(arg: any): arg is AddUnknownRest<[Node7, Node1]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode7Node4(arg: any): arg is AddUnknownRest<[Node7, Node4]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode7Node5(arg: any): arg is AddUnknownRest<[Node7, Node5]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode7Node1(arg: any): arg is AddUnknownRest<[Node7, Node1]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode7Node4(arg: any): arg is AddUnknownRest<[Node7, Node4]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode7Node5(arg: any): arg is AddUnknownRest<[Node7, Node5]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode7Node1(arg: any): arg is AddUnknownRest<[Node7, Node1]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode7Node4(arg: any): arg is AddUnknownRest<[Node7, Node4]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode7Node5(arg: any): arg is AddUnknownRest<[Node7, Node5]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode7Node1(arg: any): arg is AddUnknownRest<[Node7, Node1]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode7Node4(arg: any): arg is AddUnknownRest<[Node7, Node4]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode7Node5(arg: any): arg is AddUnknownRest<[Node7, Node5]> {
	return arg[0] && arg[0]._Node7Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode8Node9Node10Node1(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node1]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode8Node9Node10Node4(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node4]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode8Node9Node3Node5(arg: any): arg is AddUnknownRest<[Node8, Node9, Node3, Node5]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node3Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode8Node9Node10Node1(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node1]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode8Node9Node10Node4(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node4]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode8Node9Node3Node5(arg: any): arg is AddUnknownRest<[Node8, Node9, Node3, Node5]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node3Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode8Node9Node10Node1(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node1]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode8Node9Node10Node4(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node4]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode8Node9Node3Node5(arg: any): arg is AddUnknownRest<[Node8, Node9, Node3, Node5]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node3Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode8Node9Node10Node1(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node1]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode8Node9Node10Node4(arg: any): arg is AddUnknownRest<[Node8, Node9, Node10, Node4]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode8Node9Node3Node5(arg: any): arg is AddUnknownRest<[Node8, Node9, Node3, Node5]> {
	return arg[0] && arg[0]._Node8Brand
		&& arg[1] && arg[1]._Node9Brand
		&& arg[2] && arg[2]._Node3Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode9(arg: any): arg is AddUnknownRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}


function startsWithNode10Node1(arg: any): arg is AddUnknownRest<[Node10, Node1]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode10Node4(arg: any): arg is AddUnknownRest<[Node10, Node4]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode10(arg: any): arg is AddUnknownRest<[Node10]> {
	return arg[0] && arg[0]._Node10Brand
}


function startsWithNode10Node1(arg: any): arg is AddUnknownRest<[Node10, Node1]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode10Node4(arg: any): arg is AddUnknownRest<[Node10, Node4]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode10Node1(arg: any): arg is AddUnknownRest<[Node10, Node1]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode10Node4(arg: any): arg is AddUnknownRest<[Node10, Node4]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode11Node1(arg: any): arg is AddUnknownRest<[Node11, Node1]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode11Node4(arg: any): arg is AddUnknownRest<[Node11, Node4]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode11Node5(arg: any): arg is AddUnknownRest<[Node11, Node5]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode11Node9(arg: any): arg is AddUnknownRest<[Node11, Node9]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node9Brand
}


function startsWithNode11Node1(arg: any): arg is AddUnknownRest<[Node11, Node1]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode11Node4(arg: any): arg is AddUnknownRest<[Node11, Node4]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode11Node5(arg: any): arg is AddUnknownRest<[Node11, Node5]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode11Node9(arg: any): arg is AddUnknownRest<[Node11, Node9]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node9Brand
}


function startsWithNode11Node1(arg: any): arg is AddUnknownRest<[Node11, Node1]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode11Node4(arg: any): arg is AddUnknownRest<[Node11, Node4]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode11Node5(arg: any): arg is AddUnknownRest<[Node11, Node5]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode11Node9(arg: any): arg is AddUnknownRest<[Node11, Node9]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node9Brand
}


function startsWithNode11Node1(arg: any): arg is AddUnknownRest<[Node11, Node1]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode11Node4(arg: any): arg is AddUnknownRest<[Node11, Node4]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node4Brand
}


function startsWithNode11Node5(arg: any): arg is AddUnknownRest<[Node11, Node5]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node5Brand
}


function startsWithNode11Node9(arg: any): arg is AddUnknownRest<[Node11, Node9]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node9Brand
}


function startsWithNode12Node6Node4Node1(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node1]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode12Node6Node4Node4(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node4]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode12Node6Node4Node5(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node5]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode12Node6Node4Node9(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node9]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode12Node6Node4Node1(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node1]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode12Node6Node4Node4(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node4]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode12Node6Node4Node5(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node5]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode12Node6Node4Node9(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node9]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode12Node6Node4Node1(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node1]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode12Node6Node4Node4(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node4]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode12Node6Node4Node5(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node5]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode12Node6Node4Node9(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node9]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}


function startsWithNode12Node6Node4Node1(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node1]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node1Brand
}


function startsWithNode12Node6Node4Node4(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node4]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node4Brand
}


function startsWithNode12Node6Node4Node5(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node5]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node5Brand
}


function startsWithNode12Node6Node4Node9(arg: any): arg is AddUnknownRest<[Node12, Node6, Node4, Node9]> {
	return arg[0] && arg[0]._Node12Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node9Brand
}

type Fluent<Stack extends unknown[]> = (
	StartsWith<Stack, [Node2]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<Prepend<Node5, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node2]> extends 1 ?
		{ end: () => Node2['arg1'] } :
		{}
) & (
	StartsWith<Stack, [Node1]> extends 1 ?
		{ lp: () => Fluent<AddUnknownRest<Prepend<Node4, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node1]> extends 1 ?
		{ num: (arg1: Int) => Fluent<AddUnknownRest<Prepend<Node11, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node3, Node5, Node2, Node1]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}

	StartsWith<Stack, [Node3, Node5, Node6, Node4]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node6, Node4]>> } :
		{}
) & (
	StartsWith<Stack, [Node3]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<Prepend<Node9, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node3, Node5, Node2, Node1]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}

	StartsWith<Stack, [Node3, Node5, Node6, Node4]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node6, Node4]>> } :
		{}
) & (
	StartsWith<Stack, [Node3, Node5, Node2, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}

	StartsWith<Stack, [Node3, Node5, Node6, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node6, Node4]>> } :
		{}
) & (
	StartsWith<Stack, [Node4]> extends 1 ?
		{ lp: () => Fluent<AddUnknownRest<Prepend<Node4, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node4]> extends 1 ?
		{ num: (arg1: Int) => Fluent<AddUnknownRest<Prepend<Node11, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node5]> extends 1 ?
		{ lp: () => Fluent<AddUnknownRest<Prepend<Node4, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node5]> extends 1 ?
		{ num: (arg1: Int) => Fluent<AddUnknownRest<Prepend<Node11, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node6]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<Prepend<Node5, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node6]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<Prepend<Node12, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node7, Node1]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node7, Node4]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node7, Node5]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node7, Node1]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node7, Node4]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node7, Node5]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node7, Node1]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node7, Node4]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node7, Node5]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node7, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node7, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node7, Node5]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node8, Node9, Node10, Node1]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node10, Node4]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node3, Node5]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node8, Node9, Node10, Node1]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node10, Node4]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node3, Node5]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node8, Node9, Node10, Node1]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node10, Node4]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node3, Node5]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node8, Node9, Node10, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node10, Node1]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node10, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node10, Node4]>> } :
		{}

	StartsWith<Stack, [Node8, Node9, Node3, Node5]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node3, Node5]>> } :
		{}
) & (
	StartsWith<Stack, [Node9]> extends 1 ?
		{ lp: () => Fluent<AddUnknownRest<Prepend<Node4, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node9]> extends 1 ?
		{ num: (arg1: Int) => Fluent<AddUnknownRest<Prepend<Node11, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node10, Node1]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}

	StartsWith<Stack, [Node10, Node4]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node6, Node4]>> } :
		{}
) & (
	StartsWith<Stack, [Node10]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<Prepend<Node9, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node10, Node1]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}

	StartsWith<Stack, [Node10, Node4]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node6, Node4]>> } :
		{}
) & (
	StartsWith<Stack, [Node10, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}

	StartsWith<Stack, [Node10, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node6, Node4]>> } :
		{}
) & (
	StartsWith<Stack, [Node11, Node1]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node11, Node4]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node11, Node5]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node11, Node9]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node11, Node1]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node11, Node4]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node11, Node5]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node11, Node9]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node11, Node1]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node11, Node4]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node11, Node5]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node11, Node9]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node11, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node11, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node11, Node5]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node11, Node9]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node12, Node6, Node4, Node1]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node4]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node5]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node9]> extends 1 ?
		{ add: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node12, Node6, Node4, Node1]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node4]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node5]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node9]> extends 1 ?
		{ mul: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node12, Node6, Node4, Node1]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node4]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node5]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node9]> extends 1 ?
		{ rp: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
) & (
	StartsWith<Stack, [Node12, Node6, Node4, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node7, Node1]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node7, Node4]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node5]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node7, Node5]>> } :
		{}

	StartsWith<Stack, [Node12, Node6, Node4, Node9]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node8, Node9]>> } :
		{}
)

///////////////////////////////////////////////////////////////////////////////

export function begin(): Fluent<[Node1]> {
	return new FluentImpl() as any
}

///////////////////////////////////////////////////////////////////////////////

