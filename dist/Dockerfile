FROM alpine:latest
MAINTAINER lierdakil

RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories && apk --no-cache --update add curl ghc cabal libc-dev zlib zlib-dev linux-headers && curl -sSL https://get.haskellstack.org/ | sh && stack config set system-ghc --global true

RUN stack update
WORKDIR /var/tmp/server
COPY server/*.cabal server/stack.yaml /var/tmp/server/
RUN stack install --only-dependencies
COPY server/ /var/tmp/server
RUN mkdir dist
RUN stack install --local-bin-path=./dist

FROM alpine:latest

ENV USER markco
ENV DATA /var/lib/$USER
#VOLUME $DATA

RUN addgroup $USER -S -g 199 && \
	adduser -S -u 199 -h $DATA -s /bin/false -G $USER -D $USER

RUN echo http://dl-cdn.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories && apk --no-cache --update add gmp libffi supervisor

COPY --from=0 /var/tmp/server/dist/* /usr/bin

COPY markco/ /usr/share/markco
COPY server/users.txt $DATA
RUN mkdir $DATA/data && chown $USER:$USER $DATA/data
COPY markco.ini /etc/supervisor.d/

ENV MARKCO_DATA_DIR $DATA/data
ENV MARKCO_STATIC_DIR /usr/share/markco
ENV MARKCO_DATA_URI /data
ENV MARKCO_PORT 2015
#ENV MARKCO_ORIGINS
ENV MARKCO_USER_FILE users.txt
ENV MARKCO_NEWCOMMANDS /usr/share/markco/newcommands.tex

WORKDIR $DATA
CMD ["supervisord"]
