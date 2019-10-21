#!/bin/bash

curl "https://beta.quicklisp.org/release-key.txt" > /tmp/quicklisp-release-key.txt \
 && curl "https://beta.quicklisp.org/quicklisp.lisp" > /tmp/quicklisp.lisp \
 && curl "https://beta.quicklisp.org/quicklisp.lisp.asc" > /tmp/quicklisp.lisp.asc \
 && export GNUPGHOME="$(mktemp -d)" \
 && gpg --batch --import /tmp/quicklisp-release-key.txt \
 && gpg --batch --verify /tmp/quicklisp.lisp.asc /tmp/quicklisp.lisp \
 && sync \
 && sleep 1 \
 && rm -rf "$GNUPGHOME" /tmp/quicklisp.lisp.asc \
 && ${HOST_ECL}/bin/ecl -norc \
         --load /tmp/quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)" \
         --eval "(ql::without-prompting (dolist (imp '(:sbcl :ccl :abcl :ecl)) (ql:add-to-init-file imp)))" \
         --eval "(quit)" \
 && rm -rf /tmp/quicklisp*
