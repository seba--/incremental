#!/bin/sh

./sbt "run-main benchmark.pcf.Nonincremental report $@"
