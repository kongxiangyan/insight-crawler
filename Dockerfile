# syntax=docker/dockerfile:1

####################################################################################################
#                                             构建阶段
####################################################################################################
FROM fpco/stack-build:lts-18.6 AS build

LABEL authors="cigaret"
LABEL email="kcigaret@outlook.com"

COPY ./configs/docker-build-env /root/docker-build/docker-build-env
COPY ./configs/docker-build-layer /root/docker-build/docker-build-layer

# 构建 stack snapshot for docker build 缓存层
RUN cd /root/docker-build/docker-build-layer \
    && stack build --only-snapshot

# 这一层为 stack 指定了清华的源，本来应该移动到缓存层前面，但只修改 stackage 源并不能完全解决网络问题
# TODO: 待优化
RUN mv /root/.stack/config.yaml /root/.stack/configs.yaml.back \
    && cp /root/docker-build/docker-build-env/tuna-stack-config.yaml /root/.stack/config.yaml

COPY . /usr/insights-grappler

WORKDIR /usr/insights-grappler

RUN stack setup \
    && stack build \
    && stack install

# 生产阶段
FROM ubuntu:20.04

COPY --from=build /root/.local/bin/grappler-exe /root/.local/bin/grappler-exe

RUN apt-get update \
    && apt-get install -y ca-certificates \
    && update-ca-certificates

ENV PATH="$PATH:/root/.local/bin"

RUN cd /root/.local/bin/ \
    && chmod u+x grappler-exe

ENTRYPOINT ["grappler-exe"]

CMD []
