FROM ghcr.io/gleam-lang/gleam:v1.6.0-rc1-erlang
RUN apt-get update && \
    apt-get install inotify-tools -y

CMD gleam run -m lustre/dev start
