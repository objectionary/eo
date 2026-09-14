/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Engine}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class EngineTest {

    @Test
    void answersFireWithBoundOperands(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        MatcherAssert.assertThat(
            "the fire must be answered on its own id with the folded node, but it wasnt",
            EngineTest.talked(
                EngineTest.engine(new Channel(out), temp),
                out,
                String.join(
                    System.lineSeparator(),
                    "{\"𝑒\":\"⟦ ⟧\"}",
                    "{\"id\":7,\"λ\":\"L_number_times\",\"𝑏\":\"⟦ ρ ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ) ), x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) ) ⟧\"}"
                ),
                "{\"id\":1000001,\"𝑛\":\"⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧\",\"Δ\":\"40-00-00-00-00-00-00-00\"}",
                "{\"id\":1000002,\"𝑛\":\"⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧\",\"Δ\":\"40-08-00-00-00-00-00-00\"}"
            ),
            Matchers.endsWith(
                """
                {"id":7,"𝑛":"Φ.number( φ ↦ Φ.bytes( φ ↦ \
                ⟦ Δ ⤍ 40-18-00-00-00-00-00-00 ⟧ ) )"}
                """
            )
        );
    }

    @Test
    void routesAnswerToAskingFire(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        MatcherAssert.assertThat(
            "the reply to the question must reach the fire and shape its answer, but it didnt",
            EngineTest.talked(
                EngineTest.engine(new Channel(out), temp),
                out,
                "{\"id\":3,\"λ\":\"L_dataized\",\"𝑏\":\"⟦ target ↦ ξ.ρ.x ⟧\"}",
                "{\"id\":1000001,\"𝑛\":\"ξ.ρ.x\"}",
                "{\"id\":1000002,\"𝑛\":\"⟦ Δ ⤍ 2A- ⟧\",\"Δ\":\"2A-\"}"
            ),
            Matchers.endsWith(
                """
                {"id":3,"𝑛":"Φ.bytes( φ ↦ ⟦ Δ ⤍ 2A- ⟧ )"}
                """
            )
        );
    }

    @Test
    void takesLineWithoutBodyForAnAnswer(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        MatcherAssert.assertThat(
            "an answer stuck on a λ is an answer, not a fire, but it was fired",
            EngineTest.talked(
                EngineTest.engine(new Channel(out), temp),
                out,
                "{\"id\":3,\"λ\":\"L_dataized\",\"𝑏\":\"⟦ target ↦ ξ.ρ.x ⟧\"}",
                "{\"id\":1000001,\"𝑛\":\"⟦ λ ⤍ S7 ⟧\",\"λ\":\"S7\"}"
            ),
            Matchers.endsWith(
                """
                {"id":3,"𝑛":"Φ.bytes( φ ↦ ⟦ λ ⤍ S7 ⟧ )"}
                """
            )
        );
    }

    @Test
    void handsFailedFireToHandler(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        final AtomicReference<Throwable> seen = new AtomicReference<>();
        try (
            BufferedReader input = new BufferedReader(
                new StringReader("{\"id\":2,\"λ\":\"L_miracle\",\"𝑏\":\"⟦ ⟧\"}")
            )
        ) {
            new Engine(
                channel,
                new Fires(
                    new Symbols(temp.resolve("s.tsv")), new Boxes(temp.resolve("b.tsv")), channel
                ),
                new Trips(temp.resolve("t.txt")),
                (thread, error) -> seen.set(error)
            ).serve(input);
        }
        MatcherAssert.assertThat(
            "a fire nobody serves must fail through the handler, naming the fire, but it didnt",
            seen.get().getMessage(),
            Matchers.equalTo("The fire #2 of 'L_miracle' failed")
        );
    }

    @Test
    void countsEveryServedLineAsATrip(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        final Trips trips = new Trips(temp.resolve("t.txt"));
        try (
            BufferedReader input = new BufferedReader(
                new StringReader(
                    String.join(
                        System.lineSeparator(),
                        "{\"𝑒\":\"⟦ ⟧\"}",
                        "{\"id\":2,\"λ\":\"L_miracle\",\"𝑏\":\"⟦ ⟧\"}",
                        "{\"id\":3,\"λ\":\"L_miracle\",\"𝑏\":\"⟦ ⟧\"}"
                    )
                )
            )
        ) {
            new Engine(
                channel,
                new Fires(
                    new Symbols(temp.resolve("s.tsv")), new Boxes(temp.resolve("b.tsv")), channel
                ),
                trips,
                (thread, error) -> {
                }
            ).serve(input);
        }
        MatcherAssert.assertThat(
            "the two fires must count as two trips and the universe as none, but they dont",
            trips.total(),
            Matchers.equalTo(2L)
        );
    }

    private static Engine engine(final Channel channel, final Path temp) {
        return new Engine(
            channel,
            new Fires(
                new Symbols(temp.resolve("s.tsv")), new Boxes(temp.resolve("b.tsv")), channel
            ),
            new Trips(temp.resolve("t.txt")),
            (thread, error) -> {
            }
        );
    }

    private static String talked(final Engine engine, final StringWriter out,
        final String... lines) throws Exception {
        try (
            PipedWriter feed = new PipedWriter();
            BufferedReader input = new BufferedReader(new PipedReader(feed))
        ) {
            final Thread serving = new Thread(
                () -> {
                    try {
                        engine.serve(input);
                    } catch (final IOException ex) {
                        throw new IllegalStateException(ex);
                    } catch (final InterruptedException ex) {
                        Thread.currentThread().interrupt();
                    }
                }
            );
            serving.start();
            for (int idx = 0; idx < lines.length; ++idx) {
                final String asked = String.format("\"id\":%d", 1_000_000 + idx);
                final long deadline = System.currentTimeMillis() + 5_000L;
                while (idx > 0 && !out.toString().contains(asked)
                    && System.currentTimeMillis() < deadline) {
                    Thread.sleep(10L);
                }
                feed.write(lines[idx]);
                feed.write('\n');
                feed.flush();
            }
            feed.close();
            serving.join(5_000L);
        }
        return out.toString();
    }
}
