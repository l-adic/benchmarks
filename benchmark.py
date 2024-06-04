import subprocess
import statistics
import time
import re

def parse_time_output(output):
    try:
        user_time = float(output.split('User time (seconds):')[1].split('\n')[0].strip())
        system_time = float(output.split('System time (seconds):')[1].split('\n')[0].strip())
        max_memory = int(output.split('Maximum resident set size (kbytes):')[1].split('\n')[0].strip())
        elapsed_time = output.split('Elapsed (wall clock) time (h:mm:ss or m:ss):')[1].split('\n')[0].strip()

        # Convert elapsed time to seconds
        time_parts = list(map(float, re.findall(r'\d+\.\d+|\d+', elapsed_time)))
        if len(time_parts) == 3:  # h:mm:ss
            elapsed_time_sec = time_parts[0] * 3600 + time_parts[1] * 60 + time_parts[2]
        elif len(time_parts) == 2:  # m:ss
            elapsed_time_sec = time_parts[0] * 60 + time_parts[1]
        else:  # ss
            elapsed_time_sec = time_parts[0]

        return user_time, system_time, max_memory, elapsed_time_sec
    except (IndexError, ValueError) as e:
        print(f"Error parsing resource usage: {e}")
        return None, None, None, None

def run_benchmark(executable, runs=10):
    real_times = []
    user_times = []
    system_times = []
    max_memories = []

    for _ in range(runs):
        result = subprocess.run(f'/usr/bin/time -v {executable}', stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True, shell=True)


        user_time, system_time, max_memory, elapsed_time = parse_time_output(result.stderr)

        if user_time is None:
            continue

        real_times.append(elapsed_time)
        user_times.append(user_time)
        system_times.append(system_time)
        max_memories.append(max_memory)

        if user_time > elapsed_time:
            print(f"Warning: User time ({user_time:.2f}s) is greater than real time ({elapsed_time:.2f}s). This may be due to multi-threading or multi-processing.")

    if real_times:
        print(f"Real Time (seconds):")
        print(f"  Mean: {statistics.mean(real_times):.3f}")
        print(f"  Median: {statistics.median(real_times):.3f}")
        print(f"  Stddev: {statistics.stdev(real_times):.3f}")

    if user_times:
        print(f"User Time (seconds):")
        print(f"  Mean: {statistics.mean(user_times):.3f}")
        print(f"  Median: {statistics.median(user_times):.3f}")
        print(f"  Stddev: {statistics.stdev(user_times):.3f}")

    if system_times:
        print(f"System Time (seconds):")
        print(f"  Mean: {statistics.mean(system_times):.3f}")
        print(f"  Median: {statistics.median(system_times):.3f}")
        print(f"  Stddev: {statistics.stdev(system_times):.3f}")

    if max_memories:
        print(f"Max Memory (KB):")
        print(f"  Mean: {statistics.mean(max_memories):.2f}")
        print(f"  Median: {statistics.median(max_memories):.2f}")
        print(f"  Stddev: {statistics.stdev(max_memories):.2f}")

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Benchmark an executable for time and memory consumption.')
    parser.add_argument('executable', type=str, help='The path to the executable to benchmark.')
    parser.add_argument('-r', '--runs', type=int, default=10, help='The number of times to run the executable (default: 10).')

    args = parser.parse_args()

    run_benchmark(args.executable, args.runs)

