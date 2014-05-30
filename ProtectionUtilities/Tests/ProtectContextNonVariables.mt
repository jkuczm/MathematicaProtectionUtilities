(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`ProtectContextNonVariables`"];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No arguments*)


Begin["`MockContext`"];

MockConst = "MockConst from mock context"
$MockVar = "MockVar from mock context"
MockFunction[] := "MockFunction from mock context"

Begin["`Private`"];

MockPrivateConst = "MockPrivateConst from mock context"
$MockPrivateVar = "MockPrivateVar from mock context"
MockPrivateFunction[] := "MockPrivateFunction from mock context"

End[];


Test[
	ProtectContextNonVariables[]
	,
	{"MockConst", "MockFunction"}
	,
	TestID -> "No args: ProtectContextNonVariables evaluation"
];


End[];


Test[
	Attributes[`MockContext`MockConst]
	,
	{Protected}
	,
	TestID -> "No args: public constant attributes"
];
Test[
	Attributes[`MockContext`$MockVar]
	,
	{}
	,
	TestID -> "No args: public variable attributes"
];
Test[
	Attributes[`MockContext`MockFunction]
	,
	{Protected}
	,
	TestID -> "No args: public function attributes"
];

Test[
	Attributes[`MockContext`Private`MockPrivateConst]
	,
	{}
	,
	TestID -> "No args: private constant attributes"
];
Test[
	Attributes[`MockContext`Private`$MockPrivateVar]
	,
	{}
	,
	TestID -> "No args: private variable attributes"
];
Test[
	Attributes[`MockContext`Private`MockPrivateFunction]
	,
	{}
	,
	TestID -> "No args: private function attributes"
];


(* ::Subsection:: *)
(*"ProtectFunction" option*)


$MockContainer = {}

Begin["`MockContext2`"];

MockConst = "MockConst from mock context 2"
$MockVar = "MockVar from mock context 2"
MockFunction[] := "MockFunction from mock context 2"

Begin["`Private`"];

MockPrivateConst = "MockPrivateConst from mock context 2"
$MockPrivateVar = "MockPrivateVar from mock context 2"
MockPrivateFunction[] := "MockPrivateFunction from mock context 2"

End[];


Test[
	ProtectContextNonVariables[
		"ProtectFunction" ->
			((
				TestEnvironment`ProtectContextNonVariables`$MockContainer =
					{##}
			)&)
	]
	,
	{{"MockConst", "MockFunction"}}
	,
	TestID ->
		"\"ProtectFunction\" option: ProtectContextNonVariables evaluation"
];


End[];


Test[
	$MockContainer
	,
	{{"MockConst", "MockFunction"}}
	,
	TestID ->
		"\"ProtectFunction\" option: arguments passed to function from option"
];


(* ::Section:: *)
(*TearDown*)


(* Remove all symbols defined in current context and mock contexts. *)
With[
	{
		contexts = {
			"`*",
			"`MockContext`*", "`MockContext`Private`*",
			"`MockContext2`*", "`MockContext2`Private`*"
		}
	}
	,
	Unprotect @@ contexts;
	Quiet[Remove @@ contexts, {Remove::rmnsm}];
]


End[];
