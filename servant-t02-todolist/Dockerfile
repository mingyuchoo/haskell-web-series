# Stage 1: Build the application
# 프로젝트 이름을 ARG로 받아서 사용
ARG PROJECT_NAME=servant-t02-todolist
FROM fpco/stack-build:lts-23.18 AS builder
WORKDIR /app
COPY stack.yaml stack.yaml.lock package.yaml ./
COPY src/ src/
COPY static/ static/

# Build dependencies first for better caching
RUN stack build --dependencies-only
# Build the project (statically linked if desired)
ARG PROJECT_NAME
RUN stack install --local-bin-path /app/bin --ghc-options="-optl-static -fPIC"

# Stage 2: Create the final runtime image
FROM ubuntu:latest
ARG PROJECT_NAME
WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 \
    zlib1g \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/bin/${PROJECT_NAME}-exe /app/
COPY --from=builder /app/static/ /app/static/

# Expose the port the app listens on
EXPOSE 8080

# Set the entrypoint
ENTRYPOINT ["/app/${PROJECT_NAME}-exe"]
