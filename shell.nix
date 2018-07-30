let
  pkgs = import <nixpkgs> { inherit config; };
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: with pkgs; let
      libGLSupported = lib.elem system lib.platforms.mesaPlatforms;
      in rec {
        Urho3D = pkgs.callPackage ./Urho3D {};
        SDL2 = pkgs.callPackage ./SDL2 {
          openglSupport = libGLSupported;
          alsaSupport = stdenv.isLinux;
          x11Support = !stdenv.isCygwin;
          waylandSupport = stdenv.isLinux;
          udevSupport = stdenv.isLinux;
          pulseaudioSupport = config.pulseaudio or stdenv.isLinux;
          inherit (darwin.apple_sdk.frameworks) AudioUnit Cocoa CoreAudio CoreServices ForceFeedback OpenGL;
        };
    };
  };
in with pkgs; haskell.lib.buildStackProject {
  name = "Urho3D-Haskell";
  nativeBuildInputs = [ git pkgconfig ];
  buildInputs = [ SDL2.dev Urho3D ];
  extraArgs = "--extra-lib-dirs=${Urho3D}/lib/Urho3D --extra-include-dirs=${Urho3D}/include/Urho3D/ThirdParty";
  hardeningDisable = [ "bindnow" ];
  shellHook = ''
    export hardeningDisable=bindnow
  '';
}
