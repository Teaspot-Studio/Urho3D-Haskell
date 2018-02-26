# Urho3D-Haskell
Haskell bindings to Urho3D engine

## Current state

* Partial bindings
* Sample class is ported to Haskell
* 9 samples are ported (total is 46).
* Custom logic components in Haskell
* Shared pointers support

## Compilation of Urho3D

Urho3D was configured with following options for development of the bindings:

``` bash
git clone https://github.com/urho3d/Urho3D.git
cd Urho3D
mkdir build
cd build

cmake .. -DURHO3D_SAMPLES=1 -DURHO3D_EXTRAS=1 -DURHO3D_LIB_TYPE=SHARED -DCMAKE_INSTALL_PREFIX:PATH=/usr

make
sudo make install
```

Urho3D commit the bindings are tested with: `b65423cc6e13d9cef8142394e0b882544a138fc3`

Also you need to adjust paths at the end of `stack.yml`:

```
extra-lib-dirs:
- /usr/lib64/Urho3D
extra-include-dirs:
- /usr/include
- /usr/include/Urho3D/ThirdParty
```

And you need to setup `URHO3D_PREFIX_PATH` environment variable to run samples. For instance:

```
URHO3D_PREFIX_PATH=/usr/share/Urho3D/Resources
```

## Compilation of the bindings

You need [stack](http://stackage.org):

1. `stack install`

You can enable debugging printing for shared pointers with:

`stack install --flag urho3d-haskell:debug-shared`

In order to build examples:

`stack install --flag urho3d-haskell:examples`
