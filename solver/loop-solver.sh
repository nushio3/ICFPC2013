while :
do
    yes | ./dist/build/smt-synth/smt-synth auto 16 bonus
    # echo "kill others"
    # ../mapssh S '(4,9)' 'killall smt-synth' | sh
done
