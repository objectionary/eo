{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.devshell.url = "github:deemp/devshell/add-interpolate-menu";
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs.extend inputs.devshell.overlays.default) devshell;
      devShells.default = devshell.mkShell {
        commands = {
          tools = [
            pkgs.stack
          ];
        };
      };
    in
    {
      inherit devShells;
    }
  );
}
