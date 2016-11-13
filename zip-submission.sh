#!/bin/bash

SUBMISSION=$(ls -1 *.java *.scala)
SUBMISSION="$SUBMISSION halite runGame.sh"

7z a -tzip submission.zip $SUBMISSION
