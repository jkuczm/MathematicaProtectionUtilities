(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"ProtectionUtilities",
	"https://github.com/jkuczm/MathematicaProtectionUtilities/releases/download/v0.1.0/ProtectionUtilities.zip",
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install ProtectionUtilities package manually",
				"https://github.com/jkuczm/MathematicaProtectionUtilities#manual-installation"
			],
			"."
		]
]
