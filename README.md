# Urho3D-Haskell
Haskell bindings to Urho3D engine

## Current state

* Partial bindings
* Sample class is ported to Haskell
* Hello-World sample is ported

## Compilation of Urho3D

Urho3D was configured with following options for development of the bindings:

```
cmake .. -DURHO3D_SAMPLES=1 -DURHO3D_EXTRAS=1 -DURHO3D_LIB_TYPE=SHARED -DCMAKE_INSTALL_PREFIX:PATH=/usr
```

Also you need to adjust paths at the end of `stack.yml`:

```
extra-lib-dirs:
- /usr/lib64/Urho3D
extra-include-dirs:
- /usr/include
- /usr/include/Urho3D/ThirdParty
```