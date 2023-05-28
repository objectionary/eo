package org.eolang.maven.log;

import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class Logs {

    private final Collection<String> captured = new ConcurrentLinkedQueue<>();

    public Collection<String> captured() {
        return Collections.unmodifiableCollection(this.captured);
    }

    /**
     * Since logging is usually asynchronous, we need to wait for the message to appear in the
     * output. Moreover, logging system can take extra time to initialize.
     * @param message Expected part of the message
     * @return Logged message with formatting
     */
    public String waitForMessage(final String message) {
        try {
            return Executors.newSingleThreadExecutor().submit(
                () -> {
                    while (true) {
                        final Optional<String> full = this.captured.stream()
                            .filter(s -> s.contains(message))
                            .findFirst();
                        if (full.isPresent()) {
                            return full.get();
                        }
                    }
                }
            ).get(10, TimeUnit.SECONDS);
        } catch (final InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                String.format(
                    "Waiting thread was interrupted, can't read '%s' msg",
                    message
                ),
                exception
            );
        } catch (final ExecutionException exception) {
            throw new IllegalStateException(
                String.format(
                    "Some problem happened, can't read '%s' msg",
                    message
                ),
                exception
            );
        } catch (final TimeoutException exception) {
            throw new IllegalStateException(
                String.format("Timeout limit exceed to read msg %s", message),
                exception
            );
        }
    }

    void append(final String log) {
        this.captured.add(log);
    }
}
