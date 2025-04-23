# servant-t02-todolist

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
| GET    | /todos            | Get all todos               |
| POST   | /todos            | Create a new todo           |
| GET    | /todos/:todoId    | Get a todo by ID            |
| PUT    | /todos/:todoId    | Update a todo by ID         |
| DELETE | /todos/:todoId    | Delete a todo by ID         |

### Example Usage

#### Get all todos
```bash
curl http://localhost:4000/todos
```

#### Create a new todo
```bash
curl -X POST http://localhost:4000/todos \
  -H "Content-Type: application/json" \
  -d '{"todoId": 1, "todoTitle": "Alice"}'
```

#### Get a todo by ID
```bash
curl http://localhost:4000/todos/1
```

#### Update a todo by ID
```bash
curl -X PUT http://localhost:4000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"todoId": 1, "todoTitle": "Bob"}'
```

#### Delete a todo by ID
```bash
curl -X DELETE http://localhost:4000/todos/1
```

# References

- <https://marketsplash.com/tutorials/haskell/haskell-servant/>
