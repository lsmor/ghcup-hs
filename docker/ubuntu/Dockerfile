# Use an official Ubuntu base image
FROM ubuntu:latest

ENV LANG C.UTF-8

# Avoid prompts from apt
ENV DEBIAN_FRONTEND=noninteractive

# Update apt and install only the necessary packages
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    openssl \
    ca-certificates \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libffi8ubuntu1 \
    libbz2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    openssl \
    ca-certificates \
    git \
    zlib1g-dev \
    pkg-config


ARG GHCUP_VERSION=0.1.22.0
ARG GPG_KEY="7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C FFEB7CE81E16A36B3E2DED6F2DE04D4E97DB64AD 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4 EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF"

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_MINIMAL=1


# install ghcup. --insecure because cert errors.
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


ARG GHC=9.4.8
ARG CABAL_INSTALL=3.10.2.0
ARG STACK=2.13.1

ENV GHCUP_CURL_OPTS="--silent"
ENV NO_COLOR=1

ENV PATH /root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin:$PATH

# install haskell toolchain
RUN ghcup --verbose install ghc   --isolate=/usr     --force ${GHC} && \
  	ghcup --verbose install cabal --isolate=/usr/bin --force ${CABAL_INSTALL}
# 	find "/usr/lib/ghc-${GHC}/" \( -name "*_p.a" -o -name "*.p_hi" \) -type f -delete && \
# 	rm -rf "/usr/share/doc/ghc-${GHC}" && \
# 	rm -rf /tmp/ghcup* && \
# 	ghcup gc -p -s -c -t

COPY . /usr/local/ghcup-hs

WORKDIR /usr/local/ghcup-hs
RUN cabal update \
    && cabal build --only-dependencies all --project-file=cabal.advance-options.project

# Any additional commands
CMD ["bash"]

