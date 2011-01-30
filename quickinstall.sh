#!/bin/sh

if [ "$UID" -ne 0 ]; then
  echo ">> You need to run this script as root."
  exit 1;
fi

REPO=$(grep paktahn /etc/pacman.conf)
if [ ! -z "$REPO" ]; then
  echo ">> The Paktahn repository is already configured in pacman.conf."
else
  unset CARCH
  source /etc/makepkg.conf

  REPO=

  if [ "$CARCH" = "i686" ]; then
    echo "Detected 32-bit arch."
    REPO="http://viridian-project.de/~sky/paktahn/repo"
  fi

  if [ "$CARCH" = "x86_64" ]; then
    echo "Detected 64-bit arch."
    REPO="http://viridian-project.de/~sky/paktahn/repo/x86_64"
  fi

  if [ -z $REPO ]; then
    echo "Uh oh, don't know where to find the Paktahn repo for arch $CARCH. Exiting."
    exit 1
  fi

  echo -n ">> Adding Paktahn repository to pacman.conf... "
  cat >> /etc/pacman.conf <<EOF
[paktahn]
Server = $REPO
EOF
  echo done.
fi

echo ">> Syncing Paktahn..."
pacman -Sy paktahn
