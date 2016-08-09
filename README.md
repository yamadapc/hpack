# hpack-convert
Convert Cabal manifests into [hpack's package.yamls](https://github.com/sol/hpack).

## Installing from source
```
git clone https://github.com/yamadapc/hpack-convert
cd hpack-convert
stack install
```

## Usage
```bash
# Inside a directory with a .cabal file, run:
hpack-convert
# This will convert your .cabal file into a `package.yaml`
```


## License
MIT
