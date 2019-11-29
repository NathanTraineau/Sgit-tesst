#!/bin/bash
sbt clean
sbt assembly
chmod u+x jar/sgit
export PATH=$PATH:`pwd`/jar