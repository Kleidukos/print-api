#!/usr/bin/env bash

PRINTAPI_PATH="distribution/print-api"

case "$(uname -s)" in
        Linux*) 
          strip $PRINTAPI_PATH
          upx -9 $PRINTAPI_PATH
          ;;
        Darwin*)
            echo "upx crashes on macOS Ventura and above" ;;
esac
