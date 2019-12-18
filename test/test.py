import os
import time
import sys

if len(sys.argv) != 4:
    print("Usage: ./test.py <number of iterations> <maximum depth> <show games>")
    exit(1)

num_iterations = int(sys.argv[1])
max_depth = int(sys.argv[2])
show_games = sys.argv[3] == "true"

algorithms = ["simple", "ab", "par +RTS -N -s", "mixed +RTS -N -s"]

def time_successful_call(algorithm, max_depth, suppress_output):

    command = "stack exec pf2048-exe "  + algorithm + " " + str(max_depth)
    if suppress_output:
        command += " 1>/dev/null"

    attempts = 0

    while True:
        attempts += 1
        print("Running command for " + algorithm + " (attempt " + str(attempts) + ")")
        start = time.time()
        exitcode = os.system(command)
        end = time.time()
        #print("Command terminated with exit code " + str(exitcode))
        if exitcode == 0:
            return (end - start, attempts)

def avg_time_successful_calls(algorithm, num_iterations, max_depth, suppress_output):
    total_attempts = 0
    total_time = 0
    for _ in range(num_iterations):
        call_results = time_successful_call(algorithm, max_depth, suppress_output)
        total_time += call_results[0]
        total_attempts += call_results[1]

    return (total_time / num_iterations, num_iterations / total_attempts)


algorithm_results = []

for algorithm in algorithms:
    algorithm_results.append(avg_time_successful_calls(algorithm, num_iterations, max_depth, not show_games))

print("Algorithm Results:\n")

for algorithm, result in zip(algorithms, algorithm_results):
    print(algorithm + ": Average successful runtime " + str(result[0]) + ", Success Rate " + str(result[1] * 100))
