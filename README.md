# `astow`: copy-based alternative to stow

`astow` is an alternative to stow that, by default, copies files instead
of creating symlinks.  Its use cases are similar to those of `stow`, but
its copy-based approach chooses a different trade off.

## Differences to `stow`

### Why copy instead of linking?

This allows for a more controlled separation of staging direcory (stow
origin) and production (stow destination).  It thus also works in corner
cases where symlinks won't do or aren't available, like Windows.

It also does not peform stow's folding, which is not only complicated to
implement, but more importantly causes a number of problems, some of
which can be dangerous and unobvious to track down.

### How else does `astow` differ from GNU `stow`?

`astow` can be built into a single static binary.  GNU `stow` is written
in perl, meaning that it requires a working perl installation to work.
`astow` is written in Haskell, and can be compiled into a single static
binary with no dependencies.
```
nix build '.#astow-static'
```
