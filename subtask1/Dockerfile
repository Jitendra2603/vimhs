FROM haskell:8.10.7

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libncurses5-dev \
    libtinfo-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy project files
COPY . .

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Build the project and run tests
RUN stack build --test

# Set the entrypoint
ENTRYPOINT ["stack", "exec", "vimhs-exe"]
