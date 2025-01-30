# warp-t00-init

## References

- <https://crypto.stanford.edu/~blynn/haskell/warp.html>
- <https://stackoverflow.com/questions/22620294/minimal-warp-webserver-example>
- <https://wiki.haskell.org/Web/Servers#Warp>

## Packages

- `http-types`
- `wai`
- `warp`

## Dockerfile

### How to build

```bash
$ docker build --tag warp:0.1 --file Dockerfile .
```

### How to run

```bash
$ docker run --detach --name warp --publish 0.0.0.0:4000:4000 warp:0.1
```
