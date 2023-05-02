mvn clean test
max=100
for i in `seq 2 $max`
do
  mvn surefire:test -Dtest="PlaceMojoTest"
done
