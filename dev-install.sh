
set -e

./build-tarball.sh
makepkg -g >> PKGBUILD
cd pkg
makepkg -f
sudo pacman -U paktahn*.pkg.tar.gz
cd ..

