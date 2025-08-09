# Multi-stage build for psyop.ca Haskell website
FROM ubuntu:22.04 AS build

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libffi-dev \
    libgmp-dev \
    libtinfo-dev \
    zlib1g-dev \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Set working directory
WORKDIR /app

# Copy stack configuration first for better Docker layer caching
COPY stack.yaml stack.yaml.lock ./
COPY package.yaml ./
COPY psyop-website.cabal ./

# Setup stack and install dependencies
RUN stack setup --install-ghc
RUN stack build --dependencies-only

# Copy source code
COPY app/ ./app/
COPY src/ ./src/
COPY test/ ./test/

# Build the application
RUN stack build --copy-bins

# Production stage
FROM ubuntu:22.04

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp10 \
    netbase \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN groupadd -g 1001 psyop && \
    useradd -r -u 1001 -g psyop psyop

# Create app directory and copy binary
WORKDIR /app
COPY --from=build /root/.local/bin/psyop-website-exe /usr/local/bin/psyop-website-exe

# Copy static assets
COPY static/ ./static/

# Set permissions
RUN chown -R psyop:psyop /app

# Switch to non-root user
USER psyop

# Expose port 8080
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/ || exit 1

# Start the Haskell application
CMD ["/usr/local/bin/psyop-website-exe"]
