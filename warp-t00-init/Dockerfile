FROM haskell:9.0.2

RUN mkdir /app
COPY . /app
RUN cd /app && stack build --system-ghc && stack install

EXPOSE 3000

WORKDIR /app
ARG EXECUTABLE

ENTRYPOINT ["stack", "run"]
