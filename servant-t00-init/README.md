# servant-t00-init

## References

- <https://docs.servant.dev/en/stable/tutorial/index.html>
- <https://www.aosabook.org/en/posa/warp.html>
- <https://www.yesodweb.com/book/web-application-interface>

## Prerequsites

```bash
sudo apt install -y zlib1g-dev  # for zlib
```

## How to create a new servant project

```bash
stack new <project-name> servant
cd <project-name>
stack build
stack test
stack run 
```

## How to build this project

```bash
stack build # build
stack exec servant-t00-init-exe # start server to port:8080
```

## How to call API from this project

```bash
curl http://localhost:8080/users
```
