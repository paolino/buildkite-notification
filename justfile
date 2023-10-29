install:
    cabal install \
        --enable-executable-static \
        --installdir=build \
        --overwrite-policy=always \
        --install-method=copy \
          --ghc-options=-Werror \
          --ghc-options=-Wall
image: install
    docker build -t builds-2 . -f Dockerfile.local
run: image
    docker run -p 8081:8081 --rm builds-2