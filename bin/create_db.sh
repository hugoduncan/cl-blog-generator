#!/usr/bin/env sh

PATH=$PATH:/opt/local/lib/postgresql83/bin
db=blog_test
#dropdb $db;
createdb $db;
psql -c 'create language plpgsql' $db;
