# error: cabal-0.0.0.0
nix-shell \
    -p "haskellPackages.ghcWithPackages (p: [p.haskell-say p.brick p.cabal])" \
    --run "runhaskell cabal run"

