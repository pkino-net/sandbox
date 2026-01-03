#!/usr/bin/env bash
cat << "END"
aaa
bbb
ccc
END

cat <<-"END"
  aaa
  bbb
  ccc
END

echo '複数行のコメント1'

: << "COMMENT"
これは
複数行の
コメント
です
COMMENT
