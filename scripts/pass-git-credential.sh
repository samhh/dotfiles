#!/usr/bin/env bash

pass_path=${1}

echo "password=$(pass show $pass_path)"
