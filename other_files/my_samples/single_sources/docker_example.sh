#!/bin/bash

set -e

REPO_DIR=/media/files/workspace/repo_root

TOOLCHAIN_DIR=/opt/marvell
CONTAINER=my_container
DOCKER_DIR=/tmp/docker
SUCCESS_FILE=$DOCKER_DIR/success
CONTAINER_FILE=$DOCKER_DIR/${CONTAINER}_Dockerfile
OLD_CONTAINER_FILE=${CONTAINER_FILE}_old
USER=$(whoami)

mkdir -p $(dirname $CONTAINER_FILE)
if [ -f $SUCCESS_FILE ]; then
    if [ -f $CONTAINER_FILE ]; then
        mv $CONTAINER_FILE $OLD_CONTAINER_FILE
    fi
    rm $SUCCESS_FILE
else
    rm -f $CONTAINER_FILE $OLD_CONTAINER_FILE
fi

cat << EOF > $CONTAINER_FILE
FROM ubuntu:18.04
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get upgrade --no-install-recommends --yes
RUN DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends \
    --yes apt-utils locales dialog sudo file vim-tiny apt-file tree \
    build-essential gcc make git
RUN apt-file update

# ARG user=$USER

RUN useradd -G sudo $USER
RUN echo 'root:root_p' | chpasswd
RUN echo '${USER}:${USER}_p' | chpasswd
RUN echo 'set -o vi' >> /root/.bashrc
RUN mkdir /home/$USER
RUN cp /root/.bashrc /home/$USER/
RUN cp /root/.profile /home/$USER/
RUN chown -R $USER:$USER /home/$USER

# Install bear:
# RUN DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends \
#     --yes bear
RUN DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends \
    --yes cmake
RUN wget --no-check-certificate https://raw.githubusercontent.com/v-slava/dotrc/d833f3df2070094cabe25cf47229520aa82ef990/other_files/build_or_install_scripts/bear/bear.patch
RUN git -c http.sslVerify=false clone https://github.com/rizsotto/Bear
RUN cd Bear && git checkout 2.4.2 && \
    git config user.email viacheslav.volkov.1@gmail.com && \
    git am < ../bear.patch && mkdir out && cd out && \
    cmake -DCMAKE_BUILD_TYPE=Release .. && make -j9 && make install

# RUN echo '$USER ALL= NOPASSWD: ALL' > /etc/sudoers.d/all_no_password
# ADD some_file /opt/some_file
# VOLUME $REPO_DIR
WORKDIR $REPO_DIR
# USER $USER
CMD echo "+ ls" && ls && bash
EOF

if ! diff "$CONTAINER_FILE" "$OLD_CONTAINER_FILE" 1>/dev/null 2>&1 ; then
    if docker ps -a | grep -q "$CONTAINER" ; then
        docker ps -a | grep "$CONTAINER" | cut -d' ' -f1 | xargs docker rm
    fi
    echo "Building the container..."
    cat $CONTAINER_FILE | docker build -t $CONTAINER - # --no-cache
    echo "Building the container: done."
fi

set +e
docker run --user $USER -e TERM=$TERM -e USER=$USER \
    --mount type=bind,source=$REPO_DIR,destination=$REPO_DIR \
    --mount type=bind,source=$TOOLCHAIN_DIR,destination=$TOOLCHAIN_DIR,readonly \
    -it $CONTAINER

RET=$?
if [ $RET -eq 0 ]; then
    touch $SUCCESS_FILE
fi
exit $RET
