#! /bin/bash

stack clean
stack build
stack exec servant-t00-init-exe

