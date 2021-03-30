
Attempt to reproduce
```
doctest: mmap 131072 bytes at (nil): Cannot allocate memory
3508doctest: Try specifying an address with +RTS -xm<addr> -RTS
3509doctest: internal error: m32_allocator_init: Failed to map
3510    (GHC version 8.10.4 for x86_64_unknown_linux)
3511    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
3512Makefile:68: recipe for target 'doctest-fum-types' failed
3513make: *** [doctest-fum-types] Aborted (core dumped)
```

```
docker run -v $(pwd):/doctest -w /doctest -t -i --rm ubuntu:16.04 bash
```

Inside the container:
```
apt-get install build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source /root/.ghcup/env
cabal install doctest
make doctest
```
