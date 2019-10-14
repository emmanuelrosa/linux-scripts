{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [ 
      (pkgs.haskellPackages.ghcWithPackages (p: [p.turtle])) 
    ];

    shellHook = ''
      rm -fR ./out
      mkdir ./out

      ghc -cpp \
        -D_ER_PATH_WAL_=\"${pkgs.pywal}/bin/wal\" \
        -D_ER_PATH_BETTERLOCKSCREEN_=\"${pkgs.betterlockscreen}/bin/betterlockscreen\" \
        -o ./out/er-wallpaper ./src/er-wallpaper.hs
    '';
}
