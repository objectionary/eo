/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import javax.json.Json;
import javax.json.JsonObject;

/**
 * The lowering engine, the program phino fires our atoms through.
 *
 * <p>phino starts it once per run, as an {@code exec} entry of the
 * registry with {@code serve} on, and talks to it over stdin and stdout,
 * one JSON object per line: the universe first under {@code 𝑒}, then
 * every fire as a request with an {@code id}, the λ name under {@code λ}
 * and the formation without its λ under {@code 𝑏}. Every fire is served
 * on a thread of its own, because a question suspends the fire that asked
 * it and not the engine: an answer arrives on the same reader that
 * delivers the next fire, so the loop does nothing but dispatch each line
 * on its shape. Nothing computes concurrently, since a suspended fire is
 * waiting on phino, which is why the rows of the table come out in the
 * order the fragment has them.</p>
 *
 * <p>A fire still waiting when stdin closes can never be answered, so the
 * engine says so and fails rather than waiting, and a fire that fails
 * takes the process down, so that phino sees a broken run and not a
 * silence.</p>
 *
 * @since 0.76.0
 */
public final class Engine {

    /**
     * The wire.
     */
    private final Channel channel;

    /**
     * The fires.
     */
    private final Fires fires;

    /**
     * What to do when a fire fails.
     */
    private final Thread.UncaughtExceptionHandler crash;

    /**
     * Ctor.
     *
     * @param wire The wire
     * @param served The fires
     * @param handler What to do when a fire fails
     */
    public Engine(final Channel wire, final Fires served,
        final Thread.UncaughtExceptionHandler handler) {
        this.channel = wire;
        this.fires = served;
        this.crash = handler;
    }

    /**
     * Entry point.
     *
     * @param args Command line arguments, ignored
     * @throws Exception If the run fails
     */
    public static void main(final String... args) throws Exception {
        final Channel channel = new Channel(
            new OutputStreamWriter(System.out, StandardCharsets.UTF_8)
        );
        new Engine(
            channel,
            new Fires(
                new Symbols(Paths.get(System.getenv("SYMBOLS"))),
                new Boxes(Paths.get(System.getenv("BOXES"))),
                channel
            ),
            (thread, error) -> {
                error.printStackTrace(System.err);
                System.exit(1);
            }
        ).serve(new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8)));
    }

    /**
     * Serve every fire until the input ends.
     *
     * @param input The lines phino writes
     * @throws IOException If the wire fails
     * @throws InterruptedException If a wait is interrupted
     */
    public void serve(final BufferedReader input) throws IOException, InterruptedException {
        final Collection<Thread> live = new ArrayList<>(0);
        while (true) {
            final String line = input.readLine();
            if (line == null) {
                break;
            }
            final JsonObject message = Json.createReader(new StringReader(line)).readObject();
            if (message.containsKey("λ")) {
                final Thread fire = new Thread(() -> this.fired(message));
                fire.setDaemon(true);
                fire.setUncaughtExceptionHandler(this.crash);
                live.add(fire);
                fire.start();
            } else if (message.containsKey("𝑛")) {
                this.channel.answered(message.getInt("id"), message.getString("𝑛"));
            }
        }
        for (final Thread fire : live) {
            fire.join(1000L);
            if (fire.isAlive()) {
                throw new IllegalStateException(
                    "The input is closed and a fire is still waiting for an answer, which only 'serve' can give"
                );
            }
        }
    }

    /**
     * Serve one fire.
     *
     * @param message The request
     */
    private void fired(final JsonObject message) {
        final int id = message.getInt("id");
        final String lambda = message.getString("λ");
        try {
            this.channel.answer(
                id, this.fires.at(id, lambda, message.getString("𝑏")).answer()
            );
        } catch (final IOException | IllegalStateException | IllegalArgumentException ex) {
            throw new IllegalStateException(
                String.format("The fire #%d of '%s' failed", id, lambda), ex
            );
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException(
                String.format("The fire #%d of '%s' was interrupted", id, lambda), ex
            );
        }
    }
}
