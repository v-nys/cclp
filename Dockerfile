FROM jackfirth/racket

WORKDIR /cclp

ADD . /cclp

RUN apt-get update && \
    apt-get -y install git && \
    git clone https://github.com/v-nys/racket-list-utils.git && \ 
    cd racket-list-utils && \
    raco pkg install && \
    cd .. && \
    git clone https://github.com/v-nys/racket-tree-utils.git && \
    cd racket-tree-utils && \
    raco pkg install && \
    cd .. \
    raco pkg install --auto

CMD ["raco", "test", "-p", "cclp"]
