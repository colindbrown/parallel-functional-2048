import os
import time
import sys

if len(sys.argv) != 4:
    print("Usage: python3 test/searchtest.py  <number of iterations> <maximum depth> <show games>")
    exit(1)

num_iterations = int(sys.argv[1])
max_depth = int(sys.argv[2])
show_games = sys.argv[3] == "true"

algorithms = ["mixed +RTS -N"] #["simple", "ab", "par +RTS -N", "mixed +RTS -N"]
cores = ["1", "2", "3", "4", "5" "6", "7", "8"]

def time_call(algorithm, core, max_depth, suppress_output):

    command = "./spec " + str(max_depth) + " " + algorithm + core
    if suppress_output:
        command += " 1>/dev/null"

    start = time.time()
    exitcode = os.system(command)
    end = time.time()

    return end - start

def avg_time_successful_calls(algorithm, num_iterations, max_depth, suppress_output):
    for core in cores:
        total_time = 0
        for i in range(num_iterations):
            print("Running command for " + algorithm + " (iteration " + str(i) + "), cores: " + core)
            total_time += time_call(algorithm, core, max_depth, suppress_output)
        print(total_time / num_iterations)

    return total_time / num_iterations


algorithm_results = []

for algorithm in algorithms:
    algorithm_results.append(avg_time_successful_calls(algorithm, num_iterations, max_depth, not show_games))

print("Algorithm Results:\n")

for algorithm, result in zip(algorithms, algorithm_results):
    print(algorithm + ": Average successful runtime " + str(result))
