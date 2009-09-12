
set -e

./build-tarball.sh
makepkg -g >> PKGBUILD
mkdir -p pkg
cd pkg
ln -sf ../PKGBUILD
ln -sf ../paktahn.install
ln -sf ../paktahn-0.8.tar.bz2
makepkg -f
sudo pacman -U paktahn*.pkg.tar.gz
cd ..

