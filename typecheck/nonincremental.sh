#!/bin/sh

./sbt "run-main benchmark.pcf.Incremental report $@"
