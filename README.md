

# Building GHC for RedPitaya

This example builds cross ghc-7.10.2-release on debian stretch. We need to build
ghc running on our system (x64 for example) that generates binaries for arm.

It is procedure as explained on [ghc wiki](https://ghc.haskell.org/trac/ghc/wiki/Building/CrossCompiling) 

Get ghc source from git.haskell or just download [tarball](https://www.haskell.org/ghc/download_ghc_7_10_2)

    git clone http://git.haskell.org/ghc.git
    cd ghc
    git checkout ghc-7.10.2-release
    git submodule update --recursive --init


First we need to comment out building of `terminfo` stage 1 package, because it is 
single package that brakes building. Comment out line number 421 in `ghc.mk`

    PACKAGES_STAGE1 += terminfo

Next make `mk/build.mk` but use 
    
    cp mk/build.mk.sample mk/build.mk
    edit mk/build.mk

Here we need uncomment line 27 to say

    BuildFlavour  = quick-cross

This will build faster, but later if everyting goes well it can be recompiled
using fully optised binararies commenting some other line. In any case only stage1 is required.

Before configure make sure to have 

-  working arm gcc and you are able to build c files working on rp (gcc-arm-linux-gnueabi)
-  ghc
-  autoconf

For me it was

    sudo apt-get install gcc-5-arm-linux-gnueabi binutils-arm-linux-gnueabi
    sudo apt-get install ghc
    sudo apt-get install autoconf

gcc-5 is not needed and should work with 4.x too, but on my system it
was package I just happend to found and tried with. Better instructions how to obtain
gcc for arm are available on [redpitaya wiki](http://wiki.redpitaya.com/index.php?title=Developer_Guide).

now we can configure ghc from top dir

    autoconf
    ./boot
    ./configure --target=arm-linux-gnueabi --with-gcc=arm-linux-gnueabi-gcc-5

In my case I had to pass `--with-gcc` because its name was not detected
from configure altough it should (bug?). Try first without explicitly passing gcc name.

    make
    make install

after 1 hour, binaries are build and we are ready to build hello_world

Make `hello_world.hs`

    import Control.Monad

    primes = filterPrime [2..] 
      where filterPrime (p:xs) = 
              p : filterPrime [x | x <- xs, x `mod` p /= 0]

    main = forM primes print




## installing library and using cabal

We can use cabal to install packages. 
manual way

    cabal --with-ghc=arm-unknown-linux-gnueabi-ghc \
            --with-ghc-pkg=arm-unknown-linux-gnueabi-ghc-pkg \
            --with-ld=arm-linux-gnueabi-ld \
            --with-strip=arm-linux-gnueabi-strip \
            -j1 \
            install redpitaya

Using this command is possible to install varous hackage packages. Make sure to use `-j1`
flag to avoid error `/lib/ld-linux.so.3: No such file or directory`

Other way is to put all this configuration in configfile. For example create new directory
cp `~./.cabal/config` in this dir and then modify config file. 

    cabal --with-confif=CONFIGFILE install redpitaya

again make sure that `jobs` is set on 1
also cabal uses strip by default on libraries when using config file 


relevant configs in /home/user/.cabal-arm/config

    remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive
    remote-repo-cache: /home/user/.cabal-arm/packages
    world-file: /home/user/.cabal-arm/world
    verbose: 3
    with-compiler: arm-unknown-linux-gnueabi-ghc
    with-hc-pkg: /usr/local/bin/arm-unknown-linux-gnueabi-ghc-pkg
    executable-stripping: False
    library-stripping: False
    extra-prog-path: /home/user/.cabal-arm/bin
    build-summary: /home/user/.cabal-arm/logs/build.log
    remote-build-reporting: anonymous
    jobs: 1

    program-locations
        ld-location: arm-linux-gnueabi-ld
        strip-location: arm-linux-gnueabi-ld

