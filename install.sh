#!/bin/bash

VERSION="26.3"
SKIP_DEPEND=""
SKIP_INSTALL=""
BASE_URL=http://ftpmirror.gnu.org/emacs
WITH_X=1
CONFIGURE_FLAGS=

if [ `id -u` == '0' ]
then
    SUDO=""
    echo "[NOTE] You are root"
else
    SUDO="sudo"
    echo "[NOTE] You are not root"
fi


# resolve dependencies
if [ x$SKIP_DEPEND == x"1" ]
then
    echo "[SKIP] Resolving dependencies"
else
    $SUDO apt update
    $SUDO apt install -y curl
    $SUDO apt install -y libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses-dev
    # gnutls
    $SUDO apt install -y libgnutls-dev
    RET=$?
    if [ $RET != 0 ]
    then $SUDO apt install -y libgnutls28-dev
	 if [ $? != 0 ]
	 then echo "Failed!" ; exit 1
	 fi
    fi
fi

if [ x$WITH_X == x"1" ]
then
    CONFIGURE_FLAGS=
else
    CONFIGURE_FLAGS=--with-x-toolkit=no
fi



# install
if [ x$SKIP_INSTALL == x"1" ]
then
    echo "[SKIP] Building & Installation"
else
    pushd .
    mkdir -p /tmp/emacs
    cd /tmp/emacs
    curl -OL $BASE_URL/emacs-$VERSION.tar.gz
    tar xvf emacs-$VERSION.tar.gz
    cd emacs-$VERSION
    ./configure $CONFIGURE_FLAGS
    make
    if [ $? != 0 ]
    then
	echo "[ERROR] build failed"
	echo "[ERROR] '--security-opt seccomp=unconfined' may be helpful"
	exit 1
    fi
    $SUDO make install
    rm -rf /tmp/emacs
    popd
fi

# copy .emacs
if [ ! -f "$HOME/.emacs" ]
then
    echo "[.emacs] copy .emacs to '$HOME'"
    cp .emacs "$HOME"
else
    echo "[.emacs] .emacs exists already"
fi

# print emacs version
emacs --version
