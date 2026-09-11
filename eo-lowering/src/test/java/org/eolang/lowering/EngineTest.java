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
        final Channel channel = new Channel(out);
        new Engine(
            channel,
            new Fires(new Symbols(temp.resolve("s.tsv")), new Boxes(temp.resolve("b.tsv")), channel),
            (thread, error) -> {
            }
        ).serve(
            new BufferedReader(
                new StringReader(
                    String.join(
                        "\n",
                        "{\"𝑒\":\"⟦ ⟧\"}",
                        "{\"id\":7,\"λ\":\"L_number_times\",\"𝑏\":\"⟦ ρ ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ) ), x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) ) ⟧\"}"
                    )
                )
            )
        );
        MatcherAssert.assertThat(
            "the fire must be answered on its own id with the folded node, but it wasnt",
            out.toString(),
            Matchers.equalTo(
                "{\"id\":7,\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-18-00-00-00-00-00-00 ⟧ ) )\"}\n"
            )
        );
    }

    @Test
    void routesAnswerToAskingFire(@Mktmp final Path temp) throws Exception {
        final StringWriter out = new StringWriter();
        final Channel channel = new Channel(out);
        final Engine engine = new Engine(
            channel,
            new Fires(new Symbols(temp.resolve("s.tsv")), new Boxes(temp.resolve("b.tsv")), channel),
            (thread, error) -> {
            }
        );
        final PipedWriter feed = new PipedWriter();
        final BufferedReader input = new BufferedReader(new PipedReader(feed));
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
        feed.write("{\"id\":3,\"λ\":\"L_dataized\",\"𝑏\":\"⟦ target ↦ ξ.ρ.x ⟧\"}\n");
        feed.flush();
        final long deadline = System.currentTimeMillis() + 5_000L;
        while (!out.toString().contains("\"of\":3") && System.currentTimeMillis() < deadline) {
            Thread.yield();
        }
        feed.write("{\"id\":1000001,\"𝑛\":\"⟦ Δ ⤍ 2A- ⟧\"}\n");
        feed.flush();
        feed.close();
        serving.join(5_000L);
        MatcherAssert.assertThat(
            "the reply to the question must reach the fire and shape its answer, but it didnt",
            out.toString(),
            Matchers.endsWith("{\"id\":3,\"𝑛\":\"Φ.bytes( φ ↦ ⟦ Δ ⤍ 2A- ⟧ )\"}\n")
        );
    }

    @Test
    void handsFailedFireToHandler(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        final AtomicReference<Throwable> seen = new AtomicReference<>();
        new Engine(
            channel,
            new Fires(new Symbols(temp.resolve("s.tsv")), new Boxes(temp.resolve("b.tsv")), channel),
            (thread, error) -> seen.set(error)
        ).serve(
            new BufferedReader(
                new StringReader("{\"id\":2,\"λ\":\"L_miracle\",\"𝑏\":\"⟦ ⟧\"}")
            )
        );
        MatcherAssert.assertThat(
            "a fire nobody serves must fail through the handler, naming the fire, but it didnt",
            seen.get().getMessage(),
            Matchers.equalTo("The fire #2 of 'L_miracle' failed")
        );
    }
}
