

TARGET_SOURCES=$(echo $((echo target/golang.bin; ls src | grep -v -E -e '(boot\.sh)' | sed 's/^/target\//g'; ls help | sed 's/^/target\/help-/g'; echo target/help-version.txt) | LC_ALL=C sort))
GOLANG_SOURCES=$(echo $(find golang -type f -name "*.go" | LC_ALL=C sort))

RM_TARGET=$(diff -u <(ls $TARGET_SOURCES 2>/dev/null) <(ls target/* 2>/dev/null) | grep -E '^\+target' | cut -b2-)
if [ -n "$RM_TARGET" ]; then
    echo rm $RM_TARGET >&2
    rm $RM_TARGET >&2
fi

if ! which go >/dev/null; then
    bash etc/install-golang.sh >&2 || exit $?
    cat <<EOF
export PATH=$(pwd)/var/golang/bin:$PATH
export GOROOT=$(pwd)/var/golang
export GOPATH=$(pwd)/var/golang_packages

EOF
fi

cat <<\EOF
build: xsvutils

xsvutils: src/boot.sh var/TARGET_VERSION_HASH
	cat src/boot.sh | sed "s/XXXX_VERSION_HASH_XXXX/$$(cat var/TARGET_VERSION_HASH)/g" > var/xsvutils
	(cd target; tar cz *) >> var/xsvutils
	chmod 755 var/xsvutils
	mv var/xsvutils xsvutils

EOF

cat <<EOF
var/TARGET_VERSION_HASH: $TARGET_SOURCES
	cat $TARGET_SOURCES | shasum | cut -b1-40 > var/TARGET_VERSION_HASH.tmp
	mv var/TARGET_VERSION_HASH.tmp var/TARGET_VERSION_HASH

EOF

for f in $(ls src | grep -v -E -e '(boot\.sh)'); do
cat <<EOF
target/$f: src/$f
	cp src/$f target/$f

EOF
done

cat <<EOF
target/help-version.txt: version.txt
	cp version.txt target/help-version.txt

EOF

ls help/*.txt | grep -v 'main\.txt' | sed -E 's/^help\/([^.]+)\.txt$/\1/g' | column -c 80 > var/help-list.txt.tmp
if [ ! -e var/help-list.txt.tmp ] || ! diff -q var/help-list.txt var/help-list.txt.tmp >/dev/null; then
    mv var/help-list.txt.tmp var/help-list.txt
fi

cat <<EOF
target/help-main.txt: etc/build-help-main.sh help/main.txt var/help-list.txt
	bash etc/build-help-main.sh > target/help-main.txt.tmp
	mv target/help-main.txt.tmp target/help-main.txt

EOF

for f in $(ls help | grep -v 'main\.txt'); do
cat <<EOF
target/help-$f: help/$f
	cp help/$f target/help-$f

EOF
done


cat <<EOF
gobuild: target/golang.bin

target/golang.bin: var/GOLANG_VERSION_HASH
	go version
	if ! diff var/GOLANG_VERSION_HASH var/GOLANG_VERSION_HASH-build >/dev/null 2>&1; then cd golang; go vet ./...; go get github.com/spf13/cobra; go run ./generator/generator.go && go build; fi
	mv golang/golang target/golang.bin
	cp target/golang.bin golang/golang
	chmod 777 target/golang.bin
	cp var/GOLANG_VERSION_HASH var/GOLANG_VERSION_HASH-build

var/GOLANG_VERSION_HASH: $GOLANG_SOURCES
	cat $GOLANG_SOURCES | shasum | cut -b1-40 > var/GOLANG_VERSION_HASH.tmp
	mv var/GOLANG_VERSION_HASH.tmp var/GOLANG_VERSION_HASH

EOF

