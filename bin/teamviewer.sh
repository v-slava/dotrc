#!/bin/bash

CONTAINER=teamviewer

TMP=/media/files/workspace/tmp
REPO=docker-ubuntu-vnc-desktop

set -e

if [ "$1" = "stop" ]; then
    exec docker stop $CONTAINER 1>/dev/null
fi

if ! docker ps -a | grep -q $CONTAINER ; then
    if [ ! -d $TMP ]; then
        mkdir $TMP
    fi

    if [ ! -f $TMP/$REPO/prepared ]; then
        echo "Initializing repository.."
        rm -rf $TMP/$REPO
        git -C $TMP clone --recursive https://github.com/fcwu/$REPO.git
        cd $TMP/$REPO
        git checkout e4922ce92f945fc482994b7a0fd95ca5de7295b3
        (
echo 'diff --git a/Dockerfile.amd64 b/Dockerfile.amd64'
echo '--- a/Dockerfile.amd64'
echo '+++ b/Dockerfile.amd64'
echo '@@ -78,6 +78,23 @@ RUN apt-get update \'
echo '     && rm -rf /var/lib/apt/lists/* \'
echo '     && rm -rf /var/cache/apt/* /tmp/a.txt /tmp/b.txt'
echo ' '
echo '+# teamviewer'
echo '+RUN apt-get update \'
echo '+    && apt-get upgrade --with-new-pkgs --yes \'
echo '+    && apt-get install --no-install-recommends -y wget \'
echo '+    && wget https://download.teamviewer.com/download/linux/teamviewer_amd64.deb \'
echo '+    && dpkg -i teamviewer_amd64.deb || apt-get -f install -y \'
echo '+    && dpkg -l | grep -q teamviewer \'
echo '+    && rm teamviewer_amd64.deb \'
echo '+    && apt-get autoclean -y \'
echo '+    && apt-get autoremove --purge --yes \'
echo '+    && rm -rf /var/lib/apt/lists/* \'
echo '+    && rm -rf /var/cache/apt/* \'
echo '+    && echo -e "#!/bin/bash\n\'
echo '+    teamviewer --daemon start \'
echo '+    && teamviewer license accept \'
echo '+    ; teamviewer" > /root/teamviewer.sh \'
echo '+    && chmod +x /root/teamviewer.sh'
echo ' '
echo ' ################################################################################'
echo ' # builder'
        ) > /tmp/$REPO.patch
        git apply /tmp/$REPO.patch
        touch $TMP/$REPO/prepared
    fi

    cd $TMP/$REPO
    echo "Building image.."
    make
fi


if ! docker ps | grep -q $CONTAINER ; then
    OLD_CONTAINER=$(docker ps -a | grep $CONTAINER | cut -d' ' -f1 || true)
    if [ -n "$OLD_CONTAINER" ]; then
        echo "Removing old container.."
        docker rm $OLD_CONTAINER
    fi
    # make run &
    echo "Starting new container.."
    docker run -p 6080:80 -e OPENBOX_ARGS="--startup /root/teamviewer.sh" \
        --name $CONTAINER dorowu/ubuntu-desktop-lxde-vnc:latest \
        1>/tmp/teamviewer.log 2>&1 &
fi

echo "Opening WEB browser window.."
exec x-www-browser http://localhost:6080/ 1>/dev/null
