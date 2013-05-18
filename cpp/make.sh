set -eux
printf '#include "%s"\n' *.cc reader/*.cc | g++ -O3 -o repl -xc++ -
