{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [ 
      (pkgs.haskellPackages.ghcWithPackages (p: [
        p.turtle
        p.sysinfo
        p.formatting
      ])) 
    ];

    shellHook = ''
      rm -fR ./out
      mkdir ./out

      ghc -cpp \
        -D_ER_PATH_WAL_=\"${pkgs.pywal}/bin/wal\" \
        -D_ER_PATH_BETTERLOCKSCREEN_=\"${pkgs.betterlockscreen}/bin/betterlockscreen\" \
        -o ./out/er-wallpaper ./src/er-wallpaper.hs

      ghc -cpp -o ./out/er-i3memory ./src/er-i3memory.hs
      ghc -cpp -o ./out/er-i3swap ./src/er-i3swap.hs
    '';
}
