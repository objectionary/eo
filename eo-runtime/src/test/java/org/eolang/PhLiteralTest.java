/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhLiteral}.
 *
 * @since 0.1
 */
final class PhLiteralTest {

    @Test
    void returnsDefensiveSnapshotWithoutDataizingOrigin() {
        final byte[] source = {1, 2};
        final PhLiteralTest.Probe origin = new PhLiteralTest.Probe();
        final Phi literal = new PhLiteral(origin, source);
        source[0] = 9;
        final byte[] first = literal.delta();
        first[1] = 9;
        MatcherAssert.assertThat(
            "literal must isolate its snapshot and avoid origin dataization",
            Arrays.asList(
                Arrays.toString(literal.delta()), origin.deltas + origin.takes
            ),
            Matchers.equalTo(Arrays.asList("[1, 2]", 0))
        );
    }

    @Test
    void delegatesObservablePhiOperations() {
        final PhLiteralTest.Probe origin = new PhLiteralTest.Probe();
        final Phi taken = new PhLiteralTest.Probe();
        origin.taken = taken;
        final Phi literal = new PhLiteral(origin, new byte[] {1});
        final Phi selected = literal.take("x");
        literal.put(0, taken);
        literal.put("x", taken);
        MatcherAssert.assertThat(
            "all observable Phi operations must reach the origin",
            Arrays.asList(
                selected == taken, literal.needsRho(), literal.forma(),
                literal.locator(), literal.φTerm(), literal.toString(),
                origin.takes, origin.puts
            ),
            Matchers.equalTo(
                Arrays.asList(
                    true, true, "probe", "probe:1:2", "probe-term",
                    "probe-text", 1, 2
                )
            )
        );
    }

    @Test
    void copiesOriginAndKeepsLiteralBytes() {
        final PhLiteralTest.Probe origin = new PhLiteralTest.Probe();
        final Phi literal = new PhLiteral(origin, new byte[] {4, 5});
        final Phi other = new PhLiteral(origin, new byte[] {4, 5});
        final int hash = literal.hashCode();
        final Phi copy = literal.copy();
        MatcherAssert.assertThat(
            "copy and identity must preserve literal wrapper semantics",
            Arrays.asList(
                copy != literal, copy instanceof PhLiteral,
                literal.equals(literal), !literal.equals(origin),
                !literal.equals(other), hash == literal.hashCode(),
                origin.copies, Arrays.toString(copy.delta()), copy.toString()
            ),
            Matchers.equalTo(
                Arrays.asList(
                    true, true, true, true, true, true, 1, "[4, 5]",
                    "copy-text"
                )
            )
        );
    }

    @Test
    void preservesNormalizationAndTerminators() {
        final PhLiteralTest.Probe origin = new PhLiteralTest.Probe();
        final Phi same = new PhLiteral(origin, new byte[] {7});
        final List<Object> observed = new ArrayList<>(4);
        origin.normal = origin;
        observed.add(same.normalized() == same);
        final PhLiteralTest.Probe normalized = new PhLiteralTest.Probe();
        origin.normal = normalized;
        final Phi result = same.normalized();
        observed.add(result instanceof PhLiteral);
        observed.add(Arrays.toString(result.delta()));
        origin.normal = new PhTerminator();
        observed.add(same.normalized() == origin.normal);
        MatcherAssert.assertThat(
            "normalization must preserve wrappers except for terminators",
            observed,
            Matchers.equalTo(Arrays.asList(true, true, "[7]", true))
        );
    }

    @Test
    void keepsInterruptedSignalAndPhSafeLocationSemantics()
        throws InterruptedException {
        final AtomicReference<ExInterrupted> thrown = new AtomicReference<>();
        final AtomicBoolean marked = new AtomicBoolean();
        final Thread worker = new Thread(
            () -> {
                Thread.currentThread().interrupt();
                try {
                    new PhSafe(
                        new PhLiteral(
                            new PhLiteralTest.Probe(), new byte[] {1}
                        ),
                        "file.eo", 3, 5
                    ).delta();
                } catch (final ExInterrupted err) {
                    thrown.set(err);
                    marked.set(Thread.currentThread().isInterrupted());
                }
            },
            "ph-literal-interrupt"
        );
        worker.setDaemon(true);
        worker.start();
        worker.join(1000L);
        final PhLiteralTest.Probe failing = new PhLiteralTest.Probe();
        failing.failing = true;
        boolean located = false;
        try {
            new PhSafe(
                new PhLiteral(failing, new byte[] {1}), "file.eo", 3, 5
            ).take("missing");
        } catch (final ExFailure err) {
            located = err.getMessage().contains("file.eo:3:5");
        }
        MatcherAssert.assertThat(
            "interrupt and delegated failure semantics must survive decoration",
            Arrays.asList(
                !worker.isAlive(), thrown.get() instanceof ExInterrupted,
                marked.get(), located
            ),
            Matchers.everyItem(Matchers.is(true))
        );
    }

    /**
     * Small real Phi used to observe delegation.
     *
     * @since 0.1
     */
    private static final class Probe implements Phi {

        /** Number of delta calls. */
        private int deltas;

        /** Number of take calls. */
        private int takes;

        /** Number of put calls. */
        private int puts;

        /** Number of copies. */
        private int copies;

        /** Object returned by take. */
        private Phi taken;

        /** Object returned by normalized. */
        private Phi normal;

        /** Whether this probe was copied. */
        private boolean copied;

        /** Whether taking an attribute should fail. */
        private boolean failing;

        @Override
        public Phi copy() {
            this.copies += 1;
            final PhLiteralTest.Probe copy = new PhLiteralTest.Probe();
            copy.copied = true;
            copy.normal = copy;
            return copy;
        }

        @Override
        public boolean needsRho() {
            return true;
        }

        @Override
        public Phi take(final String name) {
            this.takes += 1;
            if (this.failing) {
                throw new IllegalStateException("missing attribute");
            }
            return this.taken;
        }

        @Override
        public void put(final int pos, final Phi object) {
            this.puts += 1;
        }

        @Override
        public void put(final String name, final Phi object) {
            this.puts += 1;
        }

        @Override
        public String locator() {
            return "probe:1:2";
        }

        @Override
        public String forma() {
            return "probe";
        }

        @Override
        public byte[] delta() {
            this.deltas += 1;
            return new byte[] {9};
        }

        @Override
        public Phi normalized() {
            return this.normal;
        }

        @Override
        public String φTerm() {
            return "probe-term";
        }

        @Override
        public String toString() {
            String text = "probe-text";
            if (this.copied) {
                text = "copy-text";
            }
            return text;
        }
    }
}
