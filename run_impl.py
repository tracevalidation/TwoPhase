import os
import argparse
import ndjson
import time
import signal
from subprocess import Popen

jar_name = "TwoPhase-1.1-noabort-demo-jar-with-dependencies.jar"

def read_json(filename):
    with open(filename) as f:
        return ndjson.load(f)

def run(RMs, TM):
    print("--- Run server ---")
    server_process = Popen([
        "java",
        "-cp",
        f"target/{jar_name}",
        "org.lbee.network.Server",
        "6869", "unordered"])

    print("--- Run server ---")
    clock_process = Popen([
        "java",
        "-cp",
        f"target/{jar_name}",
        "org.lbee.instrumentation.clock.ServerClock",
        "6666"])

    # Wait the server to run, if not some manager might start 
    # running before the server, leading to an error
    # This behavior might be interesting for trace validation
    time.sleep(2)

    print("--- Run TM client ---")
    # set initialisation duration long enough to make sure all 
    # RMs are working and already sent the prepare message
    # (if we try to log init state shared by RMs and TM, like messages,
    # this results in a false negative)
    duration = 10
    args = [
        "java",
        "-cp",
        f"target/{jar_name}",
        "org.lbee.Client",
        "localhost", "6869", "tm", f"{TM}"]
    for rm in RMs:
        args += [f"{rm}"]
    args += [f"{duration}"]
    tm_process = Popen(args)

    print("--- Run RM clients ---")
    rm_processes = []
    duration = 10
    for rm in RMs:
        args = [
            "java",
            "-cp",
            f"target/{jar_name}",
            "org.lbee.Client",
            "localhost", "6869", "rm", f"{rm}", f"{TM}", f"{duration}"]
        rm_process = Popen(args)
        # if duration is the same for all RMs the bug (in TM) has much less chances to appear
        # can keep the same duration for the benchmarks when the bug is fixed
        # duration += 20
        rm_processes.append(rm_process)

    # Wait for all clients to be finished
    tm_process.wait()
    for rm_process in rm_processes:
        rm_process.wait()
    # terminate
    server_process.terminate()
    tm_process.terminate()
    for rm_process in rm_processes:
        rm_process.terminate()
    # Kill server
    os.kill(server_process.pid, signal.SIGINT)
    os.kill(clock_process.pid, signal.SIGINT)

if __name__ == "__main__":
    # Read program args
    parser = argparse.ArgumentParser(description="")
    parser.add_argument('--config', type=str, required=False,
                        default="conf.ndjson", help="Config file")
    args = parser.parse_args()
    # Read config and run
    config = read_json(args.config)
    rms = config[0]["RM"]
    tm = config[1]["TM"][0]
    run(rms, tm)
