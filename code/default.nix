{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
	pname = "semtool";
	version = "v0.1";

	src = ./.;

	buildInputs = [
    	(pkgs.haskellPackages.ghcWithPackages (hpkgs: [
        	hpkgs.optparse-applicative
        	hpkgs.megaparsec
    	]))
	];
}
