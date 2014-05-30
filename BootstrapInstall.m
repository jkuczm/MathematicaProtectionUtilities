(* ::Package:: *)

Module[
	{$PIImportResult}
	,
	
	Quiet[
		$PIImportResult = Needs["ProjectInstaller`"],
		{Get::noopen, Needs::nocont}
	];
	
	If[$PIImportResult === $Failed,
		Print["ProjectInstaller not found, installing it:"];
		Print @ Import[
			"https://raw.github.com/lshifr/ProjectInstaller/master/BootstrapInstall.m"
		];
		$PIImportResult = Needs["ProjectInstaller`"];
	];

	If[$PIImportResult === $Failed,
		Print[
			"Unable to load ProjectInstaller.\n",
			"Please ",
			Hyperlink[
				"install ProtectionUtilities package manually",
				"https://github.com/jkuczm/MathematicaProtectionUtilities#manual-installation"
			],
			".\n",
			"We would be grateful for ",
			Hyperlink[
				"reporting this issue",
				"https://github.com/jkuczm/MathematicaProtectionUtilities/issues"
			],
			"."
		];
	(* else *),
		Print["Installing ProtectionUtilities:"];
		Print @ ProjectInstaller`ProjectInstall @ URL[
			"https://github.com/jkuczm/MathematicaProtectionUtilities/releases/download/v0.1.0/ProtectionUtilities.zip"
		];
	];
]
