FROM jackfirth/racket

WORKDIR /cclp

ADD . /cclp

RUN apt-get update && apt-get install -y libglib2.0-0 libfontconfig1 libcairo2 libpango1.0-0 && raco pkg install --auto

CMD ["raco", "test", "-p", "cclp"]
