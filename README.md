# linux-scripts
A collection of misc. Linux scripts.

## Scripts ##

* `er-wallpaper` - sets the wallpaper, color scheme, and lockscreen image

## How to build ##

The [Nix](https://nixos.org/nix) package manager is used as the build system. More specifically, `nix-shell`

```
cd linux-scripts
nix-shell
```

`nix-shell` will build the scripts and put them in the `./out` directory.

```
cd out
./er-wallpaper ~/my-awesome-wallpaper
```
