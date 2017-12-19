FROM jackfirth/racket

WORKDIR /cclp

ADD . /cclp

RUN raco pkg install --auto

CMD ["raco", "test", "-p", "cclp"]
