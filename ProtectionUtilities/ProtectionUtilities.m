(* ::Package:: *)

BeginPackage["ProtectionUtilities`"]


(* ::Section:: *)
(*Usage messages*)


ProtectSyntax::usage =
"\
ProtectSyntax[symbol] or ProtectSyntax[\"symbol\"] \
sets definition of symbol with most general argument pattern. Usage of symbol \
with arguments other than defined somewhere will result in warning message.\

ProtectSyntax[sym1, sym2, ...] or ProtectSyntax[{sym1, sym2, ...}] \
protects syntax of all given symbols symi, symi can be a symbol or string \
with symbol name. \

ProtectSyntax[\"form1\", \"form2\", ...] or \
ProtectSyntax[{\"form1\", \"form2\", ...}] \
protects syntax of all symbols whose names match any of the string patterns \
formi.

Due to the way mathematica patterns are matched if ProtectSyntax is used with \
\"Unevaluated\"->True (default value) use it *after* all other definitions \
for symbol, otherwise warrning message can be thrown when you don't want it.\

To generate custom message define it like this: \
symbol::wrongSyntax = \"something\", two variables are passed to wrongSyntax \
message first is symbol second is symbol with given arguments inside HoldForm."


ProtectContextNonVariables::usage =
"\
ProtectContextNonVariables[] \
protects all symbols in current context, except \"variables\" i.e. symbols \
with names starting with $."


(* ::Section:: *)
(*Implementation*)


(*
	Unprotect all symbols in this context
	(all public symbols provided by this package)
*)
Unprotect["`*"]


Begin["`Private`"]


(* ::Subsection:: *)
(*General messages*)


General::wrongSyntax = "`1` used with incorrect pattern of arguments: `2`."


(* ::Subsection:: *)
(*ProtectSyntax*)


Options[ProtectSyntax] = {
	"Unevaluated" -> True,
	"Returned" -> $Failed
}


ProtectSyntax[symbolList:{(_Symbol | _String)..}, OptionsPattern[]] :=
	Check[
		If[OptionValue["Unevaluated"],
			#[args___] := "nothing" /;
				Message[#::wrongSyntax, #, HoldForm[#[args]]];
		(* else *),
			#[args___] := (
				Message[#::wrongSyntax, #, HoldForm[#[args]]];
				OptionValue["Returned"]
			);
		];
		
		SymbolName[#]
		,
		$Failed
		,
		SetDelayed::write
	]& /@
		Cases[
			Flatten[symbolList /. str_String :> (Symbol /@ Names[str])],
			_Symbol
		]

ProtectSyntax[symbols:(_Symbol | _String).., opts:OptionsPattern[]] :=
	ProtectSyntax[{symbols}, opts]


(* ::Subsection:: *)
(*ProtectContextNonVariables*)


Options[ProtectContextNonVariables] = {
	"ProtectFunction" -> Protect
}


(*
	Use Names inside Protect since Protect doesn't accept StringExpression as
	argument.
*)
ProtectContextNonVariables[OptionsPattern[]] :=
	OptionValue["ProtectFunction"] @ Evaluate[
		Names["`"~~ Except["$"] ~~ Repeated[WordCharacter, {0, Infinity}]]
	]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
