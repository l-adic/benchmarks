import subprocess
import resource
import statistics
import time

def run_benchmark(executable, runs=10):
    real_times = []
    user_times = []
    system_times = []
    max_memories = []

    for _ in range(runs):
        start_time = time.time()
        proc = subprocess.Popen(executable, shell=True)
        proc.wait()
        end_time = time.time()

        usage = resource.getrusage(resource.RUSAGE_CHILDREN)
        real_time = end_time - start_time
        user_time = usage.ru_utime
        system_time = usage.ru_stime
        max_memory = usage.ru_maxrss

        real_times.append(real_time)
        user_times.append(user_time)
        system_times.append(system_time)
        max_memories.append(max_memory)

    print(f"Real Time (seconds):")
    print(f"  Mean: {statistics.mean(real_times):.2f}")
    print(f"  Median: {statistics.median(real_times):.2f}")
    print(f"  Stddev: {statistics.stdev(real_times):.2f}")

    print(f"User Time (seconds):")
    print(f"  Mean: {statistics.mean(user_times):.2f}")
    print(f"  Median: {statistics.median(user_times):.2f}")
    print(f"  Stddev: {statistics.stdev(user_times):.2f}")

    print(f"System Time (seconds):")
    print(f"  Mean: {statistics.mean(system_times):.2f}")
    print(f"  Median: {statistics.median(system_times):.2f}")
    print(f"  Stddev: {statistics.stdev(system_times):.2f}")

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
