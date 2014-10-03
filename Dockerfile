
# build for tradio.adellica.com and other tests
FROM adellica/cplay

RUN chicken-install -s\
 nrepl spiffy matchable intarweb uri-common medea clojurian filepath test \
 bitstring setup-helper check-errors variable-item \
 miscmacros record-variants synch lookup-table string-utils blob-utils \
 message-digest md5 http-client iset make utf8 fmt udp \
 uuid  input-parse ssax sxpath sxml-serializer \
 lru-cache foreigners sql-de-lite srfi-37 args \
 s48-modules feature-test socket

# trickier eggs
RUN apt-get install -y libssl-dev
RUN chicken-install -s openssl

EXPOSE 5055
ENTRYPOINT ["cube-server"]
CMD []

ADD . /tmp/cube-server
RUN cd /tmp/cube-server ; make ; rm -r /tmp/cube-server
