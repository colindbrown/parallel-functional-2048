
for option in "-a" "-b" "-c";
do
  echo "$option";
  time bash -c "for i in {1..10}; do ./2048 $option 1>/dev/null; done";
done
