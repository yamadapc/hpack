# hpack-convert
Convert Cabal manifests into [hpack's package.yamls](https://github.com/sol/hpack).

## Installing from source
```
git clone https://github.com/yamadapc/hpack-convert
cd hpack-convert
stack install
```

## Download a pre-built binary
- [OSX](https://github.com/yamadapc/hpack-convert/releases/download/0.14.3/hpack-convert_x86_64-osx.tar.gz)
- [Linux 64-bits (_Requires **libgmp**_)](https://github.com/yamadapc/hpack-convert/releases/download/0.14.3/hpack-convert_x86_64-linux.tar.gz)

## Usage
```bash
# Inside a directory with a .cabal file, run:
hpack-convert
# This will convert your .cabal file into a `package.yaml`
```


## License
MIT
