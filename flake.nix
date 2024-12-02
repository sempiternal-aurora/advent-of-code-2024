{
    description = "Haskell Flake devShell";
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    };

    outputs = { self, nixpkgs, ... }@inputs:
        let
            pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
        {
            devShells."x86_64-linux".default = pkgs.haskellPackages.developPackage {
                root = ./.;
                returnShellEnv = true;
                modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; 
                    [   cabal-install
                        haskell-language-server
                    ]
                );
            };
        };
}
