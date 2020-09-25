#!/bin/bash

curl -s "imap://$(cat ~/.hydroxide_user):$(cat ~/.hydroxide_pass)@localhost:1143" -X 'STATUS INBOX (UNSEEN)' |
    grep -Eo '[0-9]{1,5}'

