# syntax=docker/dockerfile:1

FROM fpco/stack-build:lts-18.6 AS build

LABEL authors="cigaret"
LABEL email="kcigaret@outlook.com"

COPY ./configs/docker-build-env /root/docker-build/docker-build-env
COPY ./configs/docker-build-layer /root/docker-build/docker-build-layer

RUN cd /root/docker-build/docker-build-layer \
    && stack build --only-snapshot

RUN mv /root/.stack/config.yaml /root/.stack/configs.yaml.back \
    && cp /root/docker-build/docker-build-env/tuna-stack-config.yaml /root/.stack/config.yaml

COPY . /usr/insights-grappler

WORKDIR /usr/insights-grappler

RUN stack setup \
    && stack build \
    && stack install

FROM ubuntu:20.04

COPY --from=build /root/.local/bin/grappler-exe /root/.local/bin/grappler-exe

RUN export PATH=$PATH:/root/.local/bin \
    && cd /root/.local/bin/ \
    && chmod u+x grappler-exe

CMD ["grappler-exe"]
