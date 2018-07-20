{ stdenv, pkgs, fetchgit, cmake, alsaLib, libGLU_combined, xorg, libpressureaudio, ibus, fcitx, dbus, editline, systemd_with_lvm2
  , wayland, wayland-protocols, libxkbcommon, libGL, udev
  , libiconv, pkgconfig }:
stdenv.mkDerivation rec {

  name = "Urho3D";

  src = fetchgit {
    url = "https://github.com/urho3d/Urho3D.git";
    rev = "b0f2b5a94f567465bfb1f88427e5e2924552a2bb";
    sha256 = "1ak8vvn9xj8zscyfhd84czdnh7jn2swnqw1ljp1gzw4y8wb9i1bv";
    fetchSubmodules = true;
  };

  setupHook = ./setup-hook.sh;
  nativeBuildInputs = [ pkgconfig cmake ];
  buildInputs = [
    alsaLib
    libGL
    libGLU_combined
    xorg.libX11
    xorg.libXcursor
    xorg.libXext
    xorg.libXi
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXrender
    xorg.libXScrnSaver
    xorg.libXxf86vm
    xorg.libICE
    libpressureaudio
    ibus
    dbus.dev
    udev.dev
    editline
    systemd_with_lvm2
    wayland
    wayland-protocols
    libxkbcommon
    libiconv
    ];

  buildPhase = ''
      cmake . -DURHO3D_EXTRAS=1 -DURHO3D_LIB_TYPE=SHARED -DCMAKE_INSTALL_PREFIX:PATH=$out
      make
    '';

  installPhase = "make install";

  meta = with stdenv.lib; {
    description = "Urho3D is a free lightweight, cross-platform 2D and 3D game engine implemented in C++ and released under the MIT license. Greatly inspired by OGRE and Horde3D.";
    homepage = "https://urho3d.github.io";
    maintainers = [ ];
    license = licenses.mit;
    platforms = platforms.linux;
  };

}
