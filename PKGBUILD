# Contributor: Leslie P. Polzer <polzer@gnu.org>
# Maintainer: Leslie P. Polzer <polzer@gnu.org>

### IMPORTANT ###
#
# Current SBCL versions require a patch to make Pacman
# and interactive editors work fully.
#
# I've submitted a patch to upstream but it's not in yet.
#
# For now install sbcl-run-program-fix to compile Paktahn.
#

pkgname=paktahn
pkgver=0.8
pkgrel=1
pkgdesc="Package manager similar to yaourt and tupac"
arch=('i686' 'x86_64')
depends=('pacman' 'readline' 'sudo')
license=('GPL')
url="http://blog.viridian-project.de/2009/09/19/announcement-paktahn-a-successor-to-yaourt/"
makedepends=('sbcl-run-program-fix')
options=(!strip)
install="paktahn.install"
source=("http://viridian-project.de/~sky/paktahn/paktahn-makedeps-$pkgver.tbz2"
        "http://viridian-project.de/~sky/paktahn/paktahn-${pkgver}.tar.bz2")

build() {
  cd "$srcdir"

  # compile
  SBCL=sbcl

  $SBCL \
       --noinform --lose-on-corruption --end-runtime-options \
       --eval "(pushnew :paktahn-deploy *features*)" \
       --eval "(require :asdf)" \
       --eval "(setf asdf:*central-registry*
                    '(\"$srcdir/clbuild.paktahn/systems/\"))" \
       --load main.lisp \
       --eval "(pak::build-core :forkp nil)"

  # install binary
  mkdir -p $pkgdir/usr/bin
  install -m755 paktahn $pkgdir/usr/bin

  # install helper scripts
  mkdir -p $pkgdir/usr/lib/paktahn
  for helper in pkgbuild-helper.sh makepkg-helper.sh; do
    install -m755 "$helper" $pkgdir/usr/lib/paktahn
  done

  # create convenience symlink
  cd $pkgdir/usr/bin
  ln -s paktahn pak
}
md5sums=('1c1336880231e88a4e90b32222272ed1'
         'f37220894cdcc1d6bacfa2a715eba72c')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '17819f69fb0b8ba656c9de30775f19850395b694')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '23d28b8629b2b446d0a348c7336ff319')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '8e9990660a6cab30703f65bc6872a2f91957f589')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '0b2cb9bea20e108179f0f3093469f962')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          'ec9711c113c16b7d894b28ba443acd7872389d7f')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '36fce7057c5247f0ceca4c853406a155')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          '7df84182eee215981e7951d873c440edb643087d')
md5sums=('1c1336880231e88a4e90b32222272ed1'
         '3abf83fa0ff45414af5272b11d63b29f')
sha1sums=('643e60fe54c56038e03823280e6d9173ad273f00'
          'f14b3aba02ee50a16b016ecdfd0e442c0ab813b1')
