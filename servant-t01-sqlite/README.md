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

## API Endpoints

The server runs on port 4000. The following endpoints are available:

### Users API

- `GET http://localhost:4000/users` - Get all users
- `POST http://localhost:4000/users` - Create a new user
  - Request body: `{"userId": <id>, "userName": "<name>"}`
- `GET http://localhost:4000/users/:userId` - Get a specific user by ID
- `PUT http://localhost:4000/users/:userId` - Update a specific user
  - Request body: `{"userId": <id>, "userName": "<name>"}`
- `DELETE http://localhost:4000/users/:userId` - Delete a specific user

All endpoints return JSON responses with an array of User objects.

## Example Usage

```bash
# Get all users
curl http://localhost:4000/users

# Create a new user
curl -X POST -H "Content-Type: application/json" -d '{"userId": 1, "userName": "John"}' http://localhost:4000/users

# Get user with ID 1
curl http://localhost:4000/users/1

# Update user with ID 1
curl -X PUT -H "Content-Type: application/json" -d '{"userId": 1, "userName": "Jane"}' http://localhost:4000/users/1

# Delete user with ID 1
curl -X DELETE http://localhost:4000/users/1
```

# References

- <https://marketsplash.com/tutorials/haskell/haskell-servant/>
