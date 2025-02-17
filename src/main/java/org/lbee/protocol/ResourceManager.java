package org.lbee.protocol;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;

import org.lbee.helpers.Helper;
import org.lbee.instrumentation.trace.TLATracer;
import org.lbee.instrumentation.trace.VirtualField;
import org.lbee.network.NetworkManager;
import org.lbee.network.TimeOutException;

public class ResourceManager extends Manager {

    /**
     * Possible states of resource manager
     */
    enum ResourceManagerState {
        WORKING,
        PREPARED,
        COMMITTED,
        ABORTED
    }

    private final static int PROBABILITY_TO_ABORT = 100;
    private static final int MAX_TASK_DURATION = 100;
    // Abort if no message from TM for ABORT_TIMEOUT
    private final static int ABORT_TIMEOUT = 2000; //200

    // Transaction manager (to send message)
    private final String transactionManagerName;
    // Current state
    private ResourceManagerState state;
    // Duration of underlying task
    private final int taskDuration;

    // tracing
    private final VirtualField traceMessages;
    private final VirtualField traceState;
    // just for showing alternative way of tracing
    private final VirtualField traceStateRMs;

    /**
     * Construct a resource manager
     *
     * @param NetworkManager Network support (for send/receive messages)
     * @param name Resource manager name
     * @param transactionManagerName Attached transaction manager name
     * @param taskDuration Duration of underlying task
     * @param tracer Trace instrumentation
     */
    public ResourceManager(NetworkManager networkManager, String name, String transactionManagerName,
            int taskDuration, TLATracer tracer) {
        super(name, networkManager, tracer);
        this.transactionManagerName = transactionManagerName;
        this.state = ResourceManagerState.WORKING;
        if (taskDuration == -1) {
            this.taskDuration = Helper.next(MAX_TASK_DURATION);
        } else {
            this.taskDuration = taskDuration;
        }
        // prepare tracing
        this.traceMessages = tracer.getVariableTracer("msgs");
        this.traceState = tracer.getVariableTracer("rmState").getField(this.name);
        this.traceStateRMs = tracer.getVariableTracer("rmState");

        System.out.println("RM " + name + " WORKING - " + taskDuration + " ms");
    }

    @Override
    public void run() throws IOException {
        long startTime = System.currentTimeMillis();
        // trace the initial state of the RM (not necessary)
        // this.traceState.update(this.state.toString().toLowerCase(Locale.ROOT));
        // tracer.log();
        // Simulate a crash of the RM
        int possibleAbort = Helper.next(PROBABILITY_TO_ABORT);
        if (possibleAbort == 1) {
            System.out.println("-- RM " + this.name + " ABORT ");
            return;
        }
        // work
        working();
        // Continuously send prepared while not committed
        while (true) {
            // send Prepared message
            this.sendPrepared();
            // block on receiving message until timeout, send again if timeout
            try {
                Message message = networkManager.receive(this.name, this.taskDuration * 2);
                this.handleMessage(message);
                System.out.println("-- RM " + this.name + " shutdown");
                return;
            } catch (TimeOutException e) {
                System.out.println("-- RM " + this.name + " received TIMEOUT ");
            }

            if (System.currentTimeMillis() - startTime > ABORT_TIMEOUT) {
                System.out.println("-- RM " + this.name + " aborted (timeout)");
                return;
            }
        }
    }

    private void working() {
        // Simulate task
        try {
            Thread.sleep(this.taskDuration);
        } catch (InterruptedException ex) {
        }
    }

    private void handleMessage(Message message) throws IOException {
        if (message.getContent().equals(TwoPhaseMessage.Commit.toString())) {
            this.state = ResourceManagerState.COMMITTED;
            this.traceState.update(this.state.toString().toLowerCase(Locale.ROOT));
            tracer.log("RMRcvCommitMsg", new Object[]{this.name});
            // tracer.log("RMRcvCommitMsg");
            // tracer.log();
        } else if (message.getContent().equals(TwoPhaseMessage.Abort.toString())) {
            this.state = ResourceManagerState.ABORTED;
            this.traceState.update(this.state.toString().toLowerCase(Locale.ROOT));
            tracer.log("RMRcvAbortMsg", new Object[]{this.name});
            // tracer.log("RMRcvAbortMsg");
            // tracer.log();
        }

        System.out.println("RM " + this.name
                + " received: " + message.getContent() + " from " + message.getFrom());
    }

    private void sendPrepared() throws IOException {
        // if the RM already sent the prepared message, it is a stuttering
        // and thus, doesn't worth logging the event
        if (this.state != ResourceManagerState.PREPARED) {
            this.state = ResourceManagerState.PREPARED;
            this.traceState.update(state.toString().toLowerCase(Locale.ROOT));
            // alternative log with apply
            // this.traceState.apply("Update", state.toString().toLowerCase(Locale.ROOT));
            // alternative log: get the state of all RMs and then field for this RM
            // this.traceStateRMs.getField(this.name).update(state.toString().toLowerCase(Locale.ROOT));
            // alternative explicit recording of the state change
            // tracer.notifyChange("rmState", List.of(name), "Update",
            //         List.of(state.toString().toLowerCase(Locale.ROOT)));
            traceMessages.add(Map.of("type", TwoPhaseMessage.Prepared.toString(), "rm", this.name)); // add Add op for
            // tracer.notifyChange("msgs", new ArrayList<String>(), "AddElement",
            //         List.of(Map.of("type", TwoPhaseMessage.Prepared.toString(), "rm", this.name)));

            // should log before the message is sent
            tracer.log("RMPrepare", new Object[]{this.name});
            // tracer.log("RMPrepare");
            // if we log the event name the potential stuttering steps are not detected as
            // such (since RMPrepare demands the state to be necessarily WORKING, no PREPARED)
            // tracer.log();
        }

        this.networkManager.send(new Message(
                this.name, transactionManagerName, TwoPhaseMessage.Prepared.toString(), 0));

        System.out.println("RM " + this.name + " sent " + TwoPhaseMessage.Prepared);
    }

}
