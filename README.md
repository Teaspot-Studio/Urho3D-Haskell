# Urho3D-Haskell
Haskell bindings to Urho3D engine

## Current state

* Partial bindings
* Sample class is ported to Haskell
* Five samples are ported
* Custom logic components in Haskell

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

Urho3D commit the bindings are tested with: `d492d32f4135c26409e93d0f76c429f36c242602`

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
