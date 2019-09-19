#!/bin/bash

VERSION="26.3"
SKIP_DEPEND=""
SKIP_INSTALL=""

if [ "$SKIP_DEPEND" == "1" ]
then
    echo "[skip] depend"
else
    sudo apt update
    sudo apt install -y curl
    sudo apt install -y libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses-dev
    # gnutls
    sudo apt install -y libgnutls-dev
    RET=$?
    if [ $RET != 0 ]
    then sudo apt install -y libgnutls28-dev
	 if [ $? != 0 ]
	 then echo "Failed!" ; exit 1
	 fi
    fi
fi

if [ "$SKIP_INSTALL" == "1" ]
then
    echo "[skip] install"
else
    pushd .
    mkdir -p /tmp/emacs
    cd /tmp/emacs
    curl -OL http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-$VERSION.tar.gz
    tar xvf emacs-$VERSION.tar.gz
    cd emacs-$VERSION
    ./configure --with-x-toolkit=no
    make -j
    sudo make install
    rm -rf /tmp/emacs
    popd
fi

emacs --version

if [ ! -f "$HOME/.emacs" ]
then
    echo "[copy] .emacs to '$HOME'"
    cp .emacs "$HOME"
else
    echo "[skip] .emacs exists"
fi
