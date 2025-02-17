package org.lbee.network;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import org.lbee.protocol.Message;

/**
 * Bucket where processes can pick some received messages
 */
public class MessageBucket<TMessageBox extends MessageBox> {

    // Map of message boxes by recipient
    private final ConcurrentHashMap<String, TMessageBox> messageBoxes;
    private final Supplier<TMessageBox> messageBoxCtor;

    public MessageBucket(Supplier<TMessageBox> messageBoxCtor) {
        this.messageBoxes = new ConcurrentHashMap<>();
        this.messageBoxCtor = Objects.requireNonNull(messageBoxCtor);
    }

    /**
     * Send a message to another process
     * @param message Message to send
     */
    public synchronized void put(Message message) {
        if (!this.messageBoxes.containsKey(message.getTo()))
            this.messageBoxes.put(message.getTo(), messageBoxCtor.get());

        this.messageBoxes.get(message.getTo()).put(message);
    }

    /**
     * Take message of some addressee process (if any)
     * @param recipientName Recipient process name
     * @return Received message
     */
    public synchronized Message take(String recipientName) {
        // Get message queue of recipient
        MessageBox messageBox = this.messageBoxes.get(recipientName);
        // No message, return null
        if (messageBox == null) {
            return null;
        }

        return messageBox.take();
    }

}
