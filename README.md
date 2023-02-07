# emacs init file

`.emacs` file and installation script


# Using and running mirrors #

<https://www.gnu.org/server/mirror.html>


# Using install script #

e.g.)

```bash
$ ./install.sh
```

e.g.) with version

```bash
$ VERSION=28.2 ./install.sh
```


# Install GNU Emacs for Linux using the Snap Store | Snapcraft #

<https://snapcraft.io/emacs>

```bash
sudo snap install emacs --classic
```

NOTE) In my case, daemon is not working well.


# Emacs Daemon as Service #

<https://www.emacswiki.org/emacs/EmacsAsDaemon>

copy `emacs.service`

```bash
mkdir -p ~/.config/systemd/user/
cp emacs.service ~/.config/systemd/user/
```

start service

```bash
systemctl daemon-reload
systemctl enable --user emacs
systemctl start --user emacs
```
