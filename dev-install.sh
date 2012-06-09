set -e
set -x

./build-tarball.sh
source ./PKGBUILD
makepkg -g >> PKGBUILD
mkdir -p pkg
cd pkg
ln -sf ../PKGBUILD
ln -sf ../paktahn.install
ln -sf ../paktahn-$pkgver.tar.bz2
makepkg -f
sudo pacman -U paktahn*$pkgver*.pkg.tar.*
cd ..
