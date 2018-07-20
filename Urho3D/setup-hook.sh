addUrho3DPath () {
  addToSearchPath PKG_CONFIG_PATH $1/lib/pkgconfig
  export URHO3D_PREFIX_PATH="$1/share/Urho3D/Resources"
}

addEnvHooks "$hostOffset" addUrho3DPath
