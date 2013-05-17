run() {
  touch -r "$1" "/tmp/wisp-file.timestamp"
  time ./wisp $1
  echo "Waiting for changes in $1..."
}

sbt assembly
run $1

while :; do
  sleep 0.5
  if [ "$1" -nt "/tmp/wisp-file.timestamp" ]
    then
      run $1
  fi
done
