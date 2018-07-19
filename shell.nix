let
  pkgs = import <nixpkgs> { inherit config; };
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      Urho3D = pkgs.callPackage ./Urho3D.nix {};
    };
  };
in with pkgs; haskell.lib.buildStackProject {
  name = "Urho3D-Haskell";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ SDL2 Urho3D ];
  extraArgs = "--extra-lib-dirs=${Urho3D}/lib/Urho3D";
  hardeningDisable = [ "bindnow" ];
  shellHook = ''
    export hardeningDisable=bindnow
  '';
}
