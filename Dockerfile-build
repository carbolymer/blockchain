FROM carbolymer/haskell:stack-1.5.1
WORKDIR /workspace
RUN groupadd -g 1000 buildusr && \
    useradd -u 1000 -g buildusr buildusr && \
    mkdir -p /home/buildusr/.stack && \
    chown -R buildusr:buildusr /home/buildusr
USER buildusr:buildusr
