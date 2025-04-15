# servant-t01-sqlite

## How to create a project

```bash
stack new <project-name> mingyuchoo/new-template
```

## How to build

```bash
stack build
# or
stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"
```

## How to test as watch mode

```bash
stack test --fast --file-watch --watch-all
# or
stack test --coverage --fast --file-watch --watch-all --haddock
# or
ghcid --command "stack ghci test/Spec.hs"
```

## How to run

```bash
stack run
```
You can also use `Makefile` for these works.

# REST API Usage

The following REST endpoints are available once the server is running (default: http://localhost:4000):

| Method | Path              | Description                 |
|--------|-------------------|-----------------------------|
| GET    | /users            | Get all users               |
| POST   | /users            | Create a new user           |
| GET    | /users/:userId    | Get a user by ID            |
| PUT    | /users/:userId    | Update a user by ID         |
| DELETE | /users/:userId    | Delete a user by ID         |

### Example Usage

#### Get all users
```bash
curl http://localhost:4000/users
```

#### Create a new user
```bash
curl -X POST http://localhost:4000/users \
  -H "Content-Type: application/json" \
  -d '{"userId": 1, "userName": "Alice"}'
```

#### Get a user by ID
```bash
curl http://localhost:4000/users/1
```

#### Update a user by ID
```bash
curl -X PUT http://localhost:4000/users/1 \
  -H "Content-Type: application/json" \
  -d '{"userId": 1, "userName": "Bob"}'
```

#### Delete a user by ID
```bash
curl -X DELETE http://localhost:4000/users/1
```

# References

- <https://marketsplash.com/tutorials/haskell/haskell-servant/>
