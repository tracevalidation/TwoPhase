package org.lbee.network;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    public static void main(String[] args) throws IOException {
        if (args.length < 1) return;

        int port = Integer.parseInt(args[0]);

        final MessageBucket<MessageBox> messageBucket;
        if (args.length > 1 && args[1].equals("unordered"))
            messageBucket = new MessageBucket<>(UnorderedMessageBox::new);
        else
            messageBucket = new MessageBucket<>(QueueMessageBox::new);

        try (ServerSocket serverSocket = new ServerSocket(port)) {
            // System.out.println("Server is listening on port " + port);
            while (true) {
                Socket socket = serverSocket.accept();
                // System.out.println("New client connected");

                new ServerThread(socket, messageBucket).start();
            }
        } catch (IOException ex) {
            System.out.println("Server exception: " + ex.getMessage());
        }
    }
}
