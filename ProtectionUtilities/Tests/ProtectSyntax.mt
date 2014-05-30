(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


Begin["TestEnvironment`ProtectSyntax`"];


(* ::Section:: *)
(*Tests*)


Module[
	{testFunction}
	,
	
	testFunction[x_] := x;
	
	
	Test[
		ProtectSyntax[testFunction, "Unevaluated" -> True]
		,
		{SymbolName[testFunction]}
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> True: \
ProtectSyntax evaluation: \
list containing protected functions name is returned, \
no messages are generated"
	];
	Test[
		testFunction[1]
		,
		1
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> True: \
protected function evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	TestMatch[
		testFunction[1, 2]
		,
		HoldPattern[testFunction[1, 2]]
		,
		{testFunction::wrongSyntax}
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> True: \
protected function evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
];


Module[
	{testFunction, testFunctionName}
	,
	
	testFunction[x_] := x;
	testFunctionName = SymbolName[testFunction];
	
	
	Test[
		ProtectSyntax[testFunctionName, "Unevaluated" -> True]
		,
		{testFunctionName}
		,
		TestID -> "ProtectSyntax 1 string, \"Unevaluated\" -> True: \
ProtectSyntax evaluation: \
list containing protected functions name is returned, \
no messages are generated"
	];
	Test[
		testFunction[1]
		,
		1
		,
		TestID -> "ProtectSyntax 1 string, \"Unevaluated\" -> True: \
protected function evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	TestMatch[
		testFunction[1, 2]
		,
		HoldPattern[testFunction[1, 2]]
		,
		{testFunction::wrongSyntax}
		,
		TestID -> "ProtectSyntax 1 string, \"Unevaluated\" -> True: \
protected function evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
];


Module[
	{testFunction1, testFunction2}
	,
	
	testFunction1[x_] := x;
	testFunction2[x_] := 2 x;
	
	
	Test[
		ProtectSyntax[testFunction1, testFunction2, "Unevaluated" -> True]
		,
		{SymbolName[testFunction1], SymbolName[testFunction2]}
		,
		TestID -> "ProtectSyntax 2 func, \"Unevaluated\" -> True: \
ProtectSyntax evaluation: \
list containing protected functions names is returned, \
no messages are generated"
	];
	
	Test[
		testFunction1[1]
		,
		1
		,
		TestID -> "ProtectSyntax 2 func, \"Unevaluated\" -> True: \
protected function 1 evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	Test[
		testFunction2[1]
		,
		2
		,
		TestID -> "ProtectSyntax 2 func, \"Unevaluated\" -> True: \
protected function 2 evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	
	TestMatch[
		testFunction1[1, 2]
		,
		HoldPattern[testFunction1[1, 2]]
		,
		{testFunction1::wrongSyntax}
		,
		TestID -> "ProtectSyntax 2 func, \"Unevaluated\" -> True: \
protected function 1 evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
	TestMatch[
		testFunction2[1, 2]
		,
		HoldPattern[testFunction2[1, 2]]
		,
		{testFunction2::wrongSyntax}
		,
		TestID -> "ProtectSyntax 2 func, \"Unevaluated\" -> True: \
protected function 2 evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
];


Module[
	{testFunction1, testFunction2, testFunction2Name}
	,
	
	testFunction1[x_] := x;
	testFunction2[x_] := 2 x;
	
	testFunction2Name = SymbolName[testFunction2];
	
	
	Test[
		ProtectSyntax[testFunction1, testFunction2Name, "Unevaluated" -> True]
		,
		{SymbolName[testFunction1], testFunction2Name}
		,
		TestID -> "ProtectSyntax 2 func (symbol and string), \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
list containing protected functions names is returned, \
no messages are generated"
	];
	
	Test[
		testFunction1[1]
		,
		1
		,
		TestID -> "ProtectSyntax 2 func (symbol and string), \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 1 evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	Test[
		testFunction2[1]
		,
		2
		,
		TestID -> "ProtectSyntax 2 func (symbol and string), \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 2 evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	
	TestMatch[
		testFunction1[1, 2]
		,
		HoldPattern[testFunction1[1, 2]]
		,
		{testFunction1::wrongSyntax}
		,
		TestID -> "ProtectSyntax 2 func (symbol and string), \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 1 evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
	TestMatch[
		testFunction2[1, 2]
		,
		HoldPattern[testFunction2[1, 2]]
		,
		{testFunction2::wrongSyntax}
		,
		TestID -> "ProtectSyntax 2 func (symbol and string), \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 2 evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
];


Module[
	{testFunction1, testFunction2, testFunction2Name}
	,
	
	testFunction1[x_] := x;
	testFunction2[x_] := 2 x;
	
	testFunction2Name = SymbolName[testFunction2];
	
	
	Test[
		ProtectSyntax[
			{testFunction1, testFunction2Name},
			"Unevaluated" -> True
		]
		,
		{SymbolName[testFunction1], testFunction2Name}
		,
		TestID -> "ProtectSyntax 2 func (symbol and string) given in List, \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
list containing protected functions names is returned, \
no messages are generated"
	];
	
	Test[
		testFunction1[1]
		,
		1
		,
		TestID -> "ProtectSyntax 2 func (symbol and string) given in List, \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 1 evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	Test[
		testFunction2[1]
		,
		2
		,
		TestID -> "ProtectSyntax 2 func (symbol and string) given in List, \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 2 evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	
	TestMatch[
		testFunction1[1, 2]
		,
		HoldPattern[testFunction1[1, 2]]
		,
		{testFunction1::wrongSyntax}
		,
		TestID -> "ProtectSyntax 2 func (symbol and string) given in List, \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 1 evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
	TestMatch[
		testFunction2[1, 2]
		,
		HoldPattern[testFunction2[1, 2]]
		,
		{testFunction2::wrongSyntax}
		,
		TestID -> "ProtectSyntax 2 func (symbol and string) given in List, \
\"Unevaluated\" -> True: ProtectSyntax evaluation: \
protected function 2 evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
];


Module[
	{testFunction}
	,
	
	testFunction[x_] := x;
	Protect[testFunction];
	
	
	Test[
		ProtectSyntax[testFunction, "Unevaluated" -> True]
		,
		{$Failed}
		,
		{SetDelayed::write}
		,
		TestID -> "ProtectSyntax 1 func protected, \"Unevaluated\" -> True: \
ProtectSyntax evaluation: \
list containing $Failed is returned, \
warning message is generated"
	];
	Test[
		testFunction[1]
		,
		1
		,
		TestID -> "ProtectSyntax 1 func protected, \"Unevaluated\" -> True: \
protected function evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	TestMatch[
		testFunction[1, 2]
		,
		HoldPattern[testFunction[1, 2]]
		,
		TestID -> "ProtectSyntax 1 func protected, \"Unevaluated\" -> True: \
protected function evaluation (wrong syntax): \
function remains unevaluated, no messages are generated"
	];
	
	Unprotect[testFunction];
];


Module[
	{testFunction}
	,
	
	testFunction[x_] := x;
	
	
	Test[
		ProtectSyntax[testFunction, "Unevaluated" -> False]
		,
		{SymbolName[testFunction]}
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> False: \
ProtectSyntax evaluation: \
list containing protected functions name is returned, \
no messages are generated"
	];
	Test[
		testFunction[1]
		,
		1
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> False: \
protected function evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	Test[
		testFunction[1, 2]
		,
		$Failed
		,
		{testFunction::wrongSyntax}
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> False: \
protected function evaluation (wrong syntax): \
$Failed is returned, warrning message is generated"
	];
];


Module[
	{testFunction, errorTag}
	,
	
	testFunction[x_] := x;
	
	
	Test[
		ProtectSyntax[
			testFunction,
			"Unevaluated" -> False,
			"Returned" -> errorTag
		]
		,
		{SymbolName[testFunction]}
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> False, \
\"Returned\" -> given value: \
ProtectSyntax evaluation: \
list containing protected functions name is returned, \
no messages are generated"
	];
	Test[
		testFunction[1]
		,
		1
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> False, \
\"Returned\" -> given value: \
protected function evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	Test[
		testFunction[1, 2]
		,
		errorTag
		,
		{testFunction::wrongSyntax}
		,
		TestID -> "ProtectSyntax 1 func, \"Unevaluated\" -> False, \
\"Returned\" -> given value: \
protected function evaluation (wrong syntax): \
given value of \"Returned\" option is returned, warrning message is generated"
	];
];


(* Mock package *)
BeginPackage["MyPackage`"];
Options[func] = {"funcOpt" -> "funcOptValue"};
func[] = "funcValue";
sym = "symValue";
EndPackage[];


Module[
	{}
	,
	
	
	Test[
		ProtectSyntax["MyPackage`*", "Unevaluated" -> True]
		,
		{"func"}
		,
		TestID -> "ProtectSyntax 1 string (context name with wildcard), \
\"Unevaluated\" -> True: \
ProtectSyntax evaluation: \
list containing protected functions name is returned, \
no messages are generated"
	];
	Test[
		func[]
		,
		"funcValue"
		,
		TestID -> "ProtectSyntax 1 string (context name with wildcard), \
\"Unevaluated\" -> True: \
protected function evaluation (correct syntax): \
evaluates to expected no messages are generated"
	];
	TestMatch[
		func[1, 2]
		,
		HoldPattern[func[1, 2]]
		,
		{func::wrongSyntax}
		,
		TestID -> "ProtectSyntax 1 string (context name with wildcard), \
\"Unevaluated\" -> True: \
protected function evaluation (wrong syntax): \
function remains unevaluated, warrning message is generated"
	];
];


(* ::Section:: *)
(*TearDown*)


(* Remove all symbols defined in current context and in mock package. *)
With[
	{contexts = {"`*", "MyPackage`*"}}
	,
	Unprotect @@ contexts;
	Quiet[Remove @@ contexts, {Remove::rmnsm}];
]


End[];
