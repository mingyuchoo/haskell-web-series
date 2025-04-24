# Stage 1: Build the application
# 프로젝트 이름을 ARG로 받아서 사용
ARG PROJECT_NAME=servant-t02-todolist
FROM fpco/stack-build:lts-23.18 AS builder
WORKDIR /app
# 프로젝트에 필요한 모든 파일들을 복사
# 설정 파일들
COPY stack.yaml stack.yaml.lock package.yaml servant-t02-todolist.cabal Setup.hs ./
COPY README.md CHANGELOG.md LICENSE ./
# 소스 디렉토리들
COPY src/ src/
COPY app/ app/
COPY test/ test/
COPY static/ static/

# 프로젝트 설정 및 의존성 설치
# 의존성 설치 시 Docker 사용하지 않음
RUN stack setup --no-docker && stack build --dependencies-only --no-docker
# Build the project (statically linked if desired)
ARG PROJECT_NAME
# 정적 링킹 관련 오류 해결을 위해 옵션 수정
RUN stack install --no-docker --local-bin-path /app/bin

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
