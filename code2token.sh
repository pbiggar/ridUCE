#!/bin/bash

redirect=http://localhost:9004/code

## first open this url in the browser, then copy the code to a file named code
if [[ $# -eq 1 ]]; then
  scope="https://www.googleapis.com/auth/carddav"
  escope=$(echo $scope | perl -MURI::Escape -ne 'chomp;print uri_escape($_),"\n"')
  open "https://accounts.google.com/o/oauth2/v2/auth?scope=${escope}&redirect_uri=$redirect&response_type=code&client_id=$(cat gmail-client-id)"
else
  curl \
    --data-urlencode client_id=$(cat gmail-client-id) \
    --data-urlencode client_secret=$(cat gmail-client-secret) \
    --data-urlencode redirect_uri="$redirect" \
    --data-urlencode grant_type=authorization_code \
    --data-urlencode code=$(cat code) \
    https://www.googleapis.com/oauth2/v4/token > gmail-token-object
  grep access gmail-token-object | awk '{print $2}' | sed s/[\",]//g > gmail-access-token
fi
