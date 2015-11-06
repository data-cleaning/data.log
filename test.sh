#!/bin/bash

R -e "devtools::run_examples('pkg')"
R -e "devtools::test('pkg')"

